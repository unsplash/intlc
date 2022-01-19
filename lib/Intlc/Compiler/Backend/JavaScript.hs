module Intlc.Compiler.Backend.JavaScript (Pieces (..), InterpStrat (..), compileStmt, compileStmtPieces) where

import           Intlc.Core (Locale (Locale))
import qualified Intlc.ICU  as ICU
import           Prelude    hiding (Type, fromList)
import           Utils      ((<>^))

-- The primary code generation compiler.
type Compiler = Reader Cfg

-- The intermediary AST compiler.
type ASTCompiler = Reader Locale

data Cfg = Cfg
  { locale :: Locale
  , interp :: Interp
  }

-- Offers cheap extensibility with TypeScript over rendering statements.
data Pieces
  = ConstantPieces
  | LambdaPieces

compileStmt :: InterpStrat -> Locale -> Text -> ICU.Message -> Text
compileStmt = compileWith stmt

compileStmtPieces :: InterpStrat -> Locale -> Text -> ICU.Message -> (Pieces, Text, Text)
compileStmtPieces = compileWith stmtPieces

compileWith :: (Stmt -> Compiler a) -> InterpStrat -> Locale -> Text -> ICU.Message -> a
compileWith f o l k m = f' fromKeyedMsg'
  where f' = flip runReader cfg . f
        fromKeyedMsg' = runReader (fromKeyedMsg k m) l
        cfg = Cfg l (fromStrat o)

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

-- | A representation of the output we will be compiling. It's a little verbose
-- split into these various sum types, but in doing so it's correct by
-- construction.
data Stmt = Stmt Text Ret

data Ret
  = Constant Text
  | Lambda (NonEmpty Expr)

data Expr
  = TPrint Text
  | TStr Ref
  | TNum Ref
  | TDate Ref ICU.DateTimeFmt
  | TApply Ref [Expr]
  | TMatch MatchOn

type MatchOn = (Text, Match)

data Match
  = LitMatch (NonEmpty Branch)
  | NonLitMatch (NonEmpty Branch) Wildcard
  | RecMatch (NonEmpty Branch) MatchOn

newtype Ref = Ref Text

data Branch = Branch Text [Expr]
newtype Wildcard = Wildcard [Expr]

fromKeyedMsg :: Text -> ICU.Message -> ASTCompiler Stmt
fromKeyedMsg n (ICU.Static x)   = pure $ Stmt n (Constant x)
fromKeyedMsg n (ICU.Dynamic ys) = Stmt n . Lambda <$> (fromToken `mapM` ys)

fromToken :: ICU.Token -> ASTCompiler Expr
fromToken (ICU.Plaintext x)     = pure $ TPrint x
fromToken (ICU.Interpolation x) = fromArg x

fromArg :: ICU.Arg -> ASTCompiler Expr
fromArg (ICU.Arg n ICU.String)               = pure $ TStr   (Ref n)
fromArg (ICU.Arg n ICU.Number)               = pure $ TNum   (Ref n)
fromArg (ICU.Arg n (ICU.Date x))             = pure $ TDate  (Ref n) x
fromArg (ICU.Arg n (ICU.Plural cs))          = TMatch <$> fromPlural (Ref n) cs
fromArg (ICU.Arg n (ICU.Select cs (Just w))) = ((TMatch . (prop (Ref n),)) .) . NonLitMatch <$> (fromSelectCase `mapM` cs) <*> fromSelectWildcard w
fromArg (ICU.Arg n (ICU.Select cs Nothing))  = TMatch . (prop (Ref n),) . LitMatch <$> (fromSelectCase `mapM` cs)
fromArg (ICU.Arg n (ICU.Callback xs))        = TApply (Ref n) <$> (fromToken `mapM` xs)

fromPlural :: Ref -> ICU.Plural -> ASTCompiler MatchOn
fromPlural r (ICU.LitPlural lcs Nothing)  = (prop r,) . LitMatch <$> (fromExactPluralCase `mapM` lcs)
fromPlural r (ICU.LitPlural lcs (Just w)) = ((prop r,) .) . NonLitMatch <$> (fromExactPluralCase `mapM` lcs) <*> fromPluralWildcard w
fromPlural r (ICU.RulePlural rcs w)       = ((prop r,) .) . NonLitMatch <$> (fromRulePluralCase `mapM` rcs) <*> fromPluralWildcard w
fromPlural r (ICU.MixedPlural lcs rcs w)  = ((prop r,) .) . RecMatch <$> (fromExactPluralCase `mapM` lcs) <*> nested
  where nested = (,) <$> (ruleCond <$> ask) <*> (NonLitMatch <$> (fromRulePluralCase `mapM` rcs) <*> fromPluralWildcard w)
        ruleCond (Locale l) = "new Intl.PluralRules('" <> l <> "').select(" <> prop r <> ")"

fromExactPluralCase :: ICU.PluralCase ICU.PluralExact -> ASTCompiler Branch
fromExactPluralCase (ICU.PluralCase (ICU.PluralExact n) xs) = Branch n <$> (fromToken `mapM` xs)

fromRulePluralCase :: ICU.PluralCase ICU.PluralRule -> ASTCompiler Branch
fromRulePluralCase (ICU.PluralCase r xs) = Branch (qts matcher) <$> (fromToken `mapM` xs)
  where matcher = case r of
         ICU.Zero -> "zero"
         ICU.One  -> "one"
         ICU.Two  -> "two"
         ICU.Few  -> "few"
         ICU.Many -> "many"
        qts x = "'" <> x <> "'"

fromPluralWildcard :: ICU.PluralWildcard -> ASTCompiler Wildcard
fromPluralWildcard (ICU.PluralWildcard xs) = Wildcard <$> (fromToken `mapM` xs)

fromSelectCase :: ICU.SelectCase -> ASTCompiler Branch
fromSelectCase (ICU.SelectCase x ys) = Branch ("'" <> x <> "'") <$> (fromToken `mapM` ys)

fromSelectWildcard :: ICU.SelectWildcard -> ASTCompiler Wildcard
fromSelectWildcard (ICU.SelectWildcard xs) = Wildcard <$> (fromToken `mapM` xs)

-- | Everything shares a single argument object whence we can access
-- interpolations.
argName :: Text
argName = "x"

prop :: Ref -> Text
prop (Ref x) = argName <> "." <> x

wrap :: Text -> Compiler Text
wrap x = do
  (o, c) <- asks ((open &&& close) . interp)
  pure $ o <> x <> c

interpc :: Text -> Compiler Text
interpc x = do
  (o, c) <- asks ((interpOpen &&& interpClose) . interp)
  pure $ o <> x <> c

stmt :: Stmt -> Compiler Text
stmt = fmap f . stmtPieces
  where f (_, n, r) = "export const " <> n <> " = " <> r

stmtPieces :: Stmt -> Compiler (Pieces, Text, Text)
stmtPieces (Stmt n (Constant r)) = pure (ConstantPieces, n, r)
stmtPieces (Stmt n (Lambda xs))  = (LambdaPieces, n, ) <$> (wrap =<< exprs xs)

exprs :: Foldable f => f Expr -> Compiler Text
exprs = foldMapM expr

expr :: Expr -> Compiler Text
expr (TPrint x)    = pure x
expr (TStr x)      = interpc (prop x)
expr (TNum x)      = do
  (Locale l) <- asks locale
  interpc $ "new Intl.NumberFormat('" <> l <> "').format(" <> prop x <> ")"
expr (TDate x fmt) = interpc =<< date x fmt
expr (TApply x ys) = interpc =<< apply x ys
expr (TMatch x)    = interpc =<< match x

apply :: Ref -> [Expr] -> Compiler Text
apply x ys = pure (prop x <> "(") <>^ (wrap =<< exprs ys) <>^ pure ")"

match :: MatchOn -> Compiler Text
match = fmap iife . go
  where go (n, m) = case m of
          LitMatch bs      -> switch n <$> branches bs
          NonLitMatch bs w -> switch n <$> wildBranches bs w
          RecMatch bs m'   -> switch n <$> recBranches bs (go m')
        iife x = "(() => { " <> x <> " })()"
        switch x ys = "switch (" <> x <> ") { " <> ys <> " }"
        branches xs = concatBranches . toList <$> mapM branch xs
          where branch (Branch x ys) = pure ("case " <> x <> ": return ") <>^ (wrap =<< exprs ys) <>^ pure ";"
                concatBranches = unwords
        wildBranches xs w = (<>) <$> branches xs <*> ((" " <>) <$> wildcard w)
          where wildcard (Wildcard xs') = pure "default: return " <>^ (wrap =<< exprs xs') <>^ pure ";"
        recBranches xs y = (<>) <$> branches xs <*> ((" " <>) . nest <$> y)
          where nest x = "default: { " <> x <> " }"

date :: Ref -> ICU.DateTimeFmt -> Compiler Text
date n d = do
  (Locale l) <- asks locale
  pure $ "new Intl.DateTimeFormat('" <> l <> "', { dateStyle: '" <> style d <> "' }).format(" <> prop n <> ")"
  where style ICU.Short  = "short"
        style ICU.Medium = "medium"
        style ICU.Long   = "long"
        style ICU.Full   = "full"
