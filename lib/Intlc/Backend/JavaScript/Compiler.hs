module Intlc.Backend.JavaScript.Compiler (InterpStrat (..), Overrides (..), emptyOverrides, compileStmt, buildReactImport, emptyModule, validateKey) where

import           Control.Monad.Extra               (pureIf)
import           Data.Char                         (isAlpha, isDigit)
import qualified Data.Text                         as T
import           Intlc.Backend.JavaScript.Language
import           Intlc.Core                        (Backend (..), Dataset,
                                                    Locale (Locale),
                                                    Translation (backend))
import qualified Intlc.ICU                         as ICU
import           Prelude
import           Utils                             (apply2, (<>^))

type Compiler = Reader Cfg

-- Allow other compilers to leverage this one, overriding only specific parts
-- of it. What's here and in what form is merely ad hoc.
data Overrides = Overrides
  { stmtOverride         :: Maybe (Text -> Text -> Text)
  , matchLitCondOverride :: Maybe (Text -> Text)
  }

emptyOverrides :: Overrides
emptyOverrides = Overrides mempty mempty

fromOverride :: (Overrides -> Maybe a) -> a -> Compiler a
fromOverride f x = fromMaybe x <$> asks (f . overrides)

override :: (Overrides -> Maybe (a -> a)) -> a -> Compiler a
override f x = maybe x ($ x) <$> asks (f . overrides)

data Cfg = Cfg
  { locale    :: Locale
  , interp    :: InterpStrat
  , overrides :: Overrides
  }

compileStmt :: Overrides -> InterpStrat -> Locale -> Text -> ICU.Message -> Text
compileStmt o s l k m = f' fromKeyedMsg'
  where f' = flip runReader (Cfg l s o) . stmt
        fromKeyedMsg' = runReader (fromKeyedMsg k m) l

data InterpStrat
  = TemplateLit
  | JSX

data Interp = Interp
  { open        :: Text
  , close       :: Text
  , interpOpen  :: Text
  , interpClose :: Text
  }

fromStrat :: InterpStrat -> Interp
fromStrat TemplateLit = Interp
  { open = "`"
  , close = "`"
  , interpOpen = "${"
  , interpClose = "}"
  }
fromStrat JSX         = Interp
  { open = "<>"
  , close = "</>"
  , interpOpen = "{"
  , interpClose = "}"
  }

-- | Everything shares a single argument object whence we can access
-- interpolations.
argName :: Text
argName = "x"

prop :: ICU.Arg -> Text
prop (ICU.Arg x) = argName <> "." <> x

wrap :: Text -> Compiler Text
wrap x = do
  (o, c) <- asks ((open &&& close) . fromStrat . interp)
  pure $ o <> x <> c

interpc :: Text -> Compiler Text
interpc x = do
  (o, c) <- asks ((interpOpen &&& interpClose) . fromStrat . interp)
  pure $ o <> x <> c

stmt :: Stmt -> Compiler Text
stmt (Stmt n xs) = do
  r <- wrap =<< exprs xs
  (fmap (apply2 n r) . stmtOverride) `fromOverride` ("export const " <> n <> " = " <> r)

exprs :: Foldable f => f Expr -> Compiler Text
exprs = foldMapM expr

expr :: Expr -> Compiler Text
expr (TPrint x)    = asks interp <&> \case
  TemplateLit -> T.concatMap escape x
  _           -> x
  where escape '`' = "\\`"
        escape c   = T.singleton c
expr (TStr x)      = interpc (prop x)
expr (TNum x)      = do
  (Locale l) <- asks locale
  interpc $ "new Intl.NumberFormat('" <> l <> "').format(" <> prop x <> ")"
expr (TDate x fmt) = interpc =<< date x fmt
expr (TTime x fmt) = interpc =<< time x fmt
expr (TApply x ys) = interpc =<< apply x ys
expr (TMatch x)    = interpc =<< match x

apply :: ICU.Arg -> [Expr] -> Compiler Text
apply x ys = pure (prop x <> "(") <>^ (wrap =<< exprs ys) <>^ pure ")"

match :: Match -> Compiler Text
match = fmap iife . go where
  go (Match n c m) = case m of
    LitMatchRet bs      -> switch <$> cond <*> branches bs
    NonLitMatchRet bs w -> switch <$> cond <*> wildBranches bs w
    RecMatchRet bs m'   -> switch <$> cond <*> recBranches bs (go m')
    where cond = matchCond n c
  iife x = "(() => { " <> x <> " })()"
  switch x ys = "switch (" <> x <> ") { " <> ys <> " }"
  branches xs = concatBranches . toList <$> mapM branch xs
    where branch (Branch x ys) = pure ("case " <> x <> ": return ") <>^ (wrap =<< exprs ys) <>^ pure ";"
          concatBranches = unwords
  wildBranches xs w = (<>) <$> branches xs <*> ((" " <>) <$> wildcard w)
    where wildcard (Wildcard xs') = pure "default: return " <>^ (wrap =<< exprs xs') <>^ pure ";"
  recBranches xs y = (<>) <$> branches xs <*> ((" " <>) . nest <$> y)
    where nest x = "default: { " <> x <> " }"

matchCond :: ICU.Arg -> MatchCond -> Compiler Text
matchCond n LitCond                = override matchLitCondOverride (prop n)
matchCond n CardinalPluralRuleCond = f <$> asks locale
  where f (Locale l) = "new Intl.PluralRules('" <> l <> "').select(" <> prop n <> ")"
matchCond n OrdinalPluralRuleCond  = f <$> asks locale
  where f (Locale l) = "new Intl.PluralRules('" <> l <> "', { type: 'ordinal' }).select(" <> prop n <> ")"

date :: ICU.Arg -> ICU.DateTimeFmt -> Compiler Text
date n d = do
  (Locale l) <- asks locale
  pure $ "new Intl.DateTimeFormat('" <> l <> "', { dateStyle: '" <> dateTimeFmt d <> "' }).format(" <> prop n <> ")"

time :: ICU.Arg -> ICU.DateTimeFmt -> Compiler Text
time n d = do
  (Locale l) <- asks locale
  pure $ "new Intl.DateTimeFormat('" <> l <> "', { timeStyle: '" <> dateTimeFmt d <> "' }).format(" <> prop n <> ")"

dateTimeFmt :: ICU.DateTimeFmt -> Text
dateTimeFmt ICU.Short  = "short"
dateTimeFmt ICU.Medium = "medium"
dateTimeFmt ICU.Long   = "long"
dateTimeFmt ICU.Full   = "full"

-- A no-op that clarifies a JS/TS file as an ES module.
emptyModule :: Text
emptyModule = "export {}"

buildReactImport :: Dataset Translation -> Maybe Text
buildReactImport = flip pureIf text . any ((TypeScriptReact ==) . backend)
  where text = "import { ReactElement } from 'react'"

validateKey :: Text -> Either Text ()
validateKey k
  | T.null k                        = Left "[Empty identifier found.]"
  | k `elem` reservedWords          = Left $ k <> ": reserved word."
  | not (isValidIdent (T.unpack k)) = Left $ k <> ": invalid identifier."
  | otherwise                       = Right ()
  -- https://developer.mozilla.org/en-US/docs/Glossary/identifier
  where isValidIdent []     = False -- Technically already caught by `T.null`.
        isValidIdent (c:cs) = isValidIdentHeadChar c && all isValidIdentTailChar cs
        isValidIdentHeadChar = liftA2 (||) isAlpha (`elem` ['$', '_'])
        isValidIdentTailChar = liftA2 (||) isValidIdentHeadChar isDigit

-- Useful docs:
--   https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#keywords
reservedWords :: [Text]
reservedWords = es2015 <> future <> module' <> legacy <> literals where
  es2015 =
    [ "break"
    , "case"
    , "catch"
    , "class"
    , "const"
    , "continue"
    , "debugger"
    , "default"
    , "delete"
    , "do"
    , "else"
    , "export"
    , "extends"
    , "finally"
    , "for"
    , "function"
    , "if"
    , "import"
    , "in"
    , "instanceof"
    , "new"
    , "return"
    , "super"
    , "switch"
    , "this"
    , "throw"
    , "try"
    , "typeof"
    , "var"
    , "void"
    , "while"
    , "with"
    , "yield"
    ]
  future =
    [ "enum"
    , "implements"
    , "interface"
    , "let"
    , "package"
    , "private"
    , "protected"
    , "public"
    , "static"
    , "yield"
    ]
  module' =
    [ "await"
    ]
  legacy =
    [ "abstract"
    , "boolean"
    , "byte"
    , "char"
    , "double"
    , "final"
    , "float"
    , "goto"
    , "int"
    , "long"
    , "native"
    , "short"
    , "synchronized"
    , "throws"
    , "transient"
    , "volatile"
    ]
  literals =
    [ "null"
    , "true"
    , "false"
    ]
