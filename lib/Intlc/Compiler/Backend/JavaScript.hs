module Intlc.Compiler.Backend.JavaScript (Pieces (..), InterpStrat (..), compileStmt, compileStmtPieces) where

import qualified Intlc.ICU as ICU
import           Prelude   hiding (Type, fromList)
import           Utils     ((<>^))

-- Offers cheap extensibility with TypeScript over rendering statements.
data Pieces
  = ConstantPieces
  | LambdaPieces

compileStmt :: InterpStrat -> Text -> ICU.Message -> Text
compileStmt = compileWith stmt

compileStmtPieces :: InterpStrat -> Text -> ICU.Message -> (Pieces, Text, Text)
compileStmtPieces = compileWith stmtPieces

compileWith :: (Stmt -> Compiler a) -> InterpStrat -> Text -> ICU.Message -> a
compileWith f o k = flip runReader (fromStrat o) . f . fromKeyedMsg k

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
  | TDate Ref ICU.DateFmt
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

fromKeyedMsg :: Text -> ICU.Message -> Stmt
fromKeyedMsg n (ICU.Static x)   = Stmt n (Constant x)
fromKeyedMsg n (ICU.Dynamic ys) = Stmt n (Lambda $ fromToken <$> ys)

fromToken :: ICU.Token -> Expr
fromToken (ICU.Plaintext x)     = TPrint x
fromToken (ICU.Interpolation x) = fromArg x

fromArg :: ICU.Arg -> Expr
fromArg (ICU.Arg n ICU.String)               = TStr   (Ref n)
fromArg (ICU.Arg n ICU.Number)               = TNum   (Ref n)
fromArg (ICU.Arg n (ICU.Date x))             = TDate  (Ref n) x
fromArg (ICU.Arg n (ICU.Plural cs))          = TMatch (fromPlural (Ref n) cs)
fromArg (ICU.Arg n (ICU.Select cs (Just w))) = TMatch (prop (Ref n), NonLitMatch (fromSelectCase <$> cs) (fromSelectWildcard w))
fromArg (ICU.Arg n (ICU.Select cs Nothing))  = TMatch (prop (Ref n), LitMatch (fromSelectCase <$> cs))
fromArg (ICU.Arg n (ICU.Callback xs))        = TApply (Ref n) (fromToken <$> xs)

fromPlural :: Ref -> ICU.Plural -> MatchOn
fromPlural r (ICU.LitPlural lcs Nothing)  = (prop r, LitMatch    (fromExactPluralCase <$> lcs))
fromPlural r (ICU.LitPlural lcs (Just w)) = (prop r, NonLitMatch (fromExactPluralCase <$> lcs) (fromPluralWildcard w))
fromPlural r (ICU.RulePlural rcs w)       = (prop r, NonLitMatch (fromRulePluralCase <$> rcs)  (fromPluralWildcard w))
fromPlural r (ICU.MixedPlural lcs rcs w)  = (prop r, RecMatch    (fromExactPluralCase <$> lcs) nested)
  where nested = (ruleCond, NonLitMatch (fromRulePluralCase <$> rcs) (fromPluralWildcard w))
        ruleCond = "new Intl.PluralRules('en-US').select(" <> prop r <> ")"

fromExactPluralCase :: ICU.PluralCase ICU.PluralExact -> Branch
fromExactPluralCase (ICU.PluralCase (ICU.PluralExact n) xs) = Branch n (fromToken <$> xs)

fromRulePluralCase :: ICU.PluralCase ICU.PluralRule -> Branch
fromRulePluralCase (ICU.PluralCase r xs) = flip Branch (fromToken <$> xs) . qts $ case r of
   ICU.Zero -> "zero"
   ICU.One  -> "one"
   ICU.Two  -> "two"
   ICU.Few  -> "few"
   ICU.Many -> "many"
  where qts x = "'" <> x <> "'"

fromPluralWildcard :: ICU.PluralWildcard -> Wildcard
fromPluralWildcard (ICU.PluralWildcard xs) = Wildcard (fromToken <$> xs)

fromSelectCase :: ICU.SelectCase -> Branch
fromSelectCase (ICU.SelectCase x ys) = Branch ("'" <> x <> "'") (fromToken <$> ys)

fromSelectWildcard :: ICU.SelectWildcard -> Wildcard
fromSelectWildcard (ICU.SelectWildcard xs) = Wildcard (fromToken <$> xs)

type Compiler = Reader Interp

-- | Everything shares a single argument object whence we can access
-- interpolations.
argName :: Text
argName = "x"

prop :: Ref -> Text
prop (Ref x) = argName <> "." <> x

wrap :: Text -> Compiler Text
wrap x = do
  (o, c) <- asks (open &&& close)
  pure $ o <> x <> c

interp :: Text -> Compiler Text
interp x = do
  (o, c) <- asks (interpOpen &&& interpClose)
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
expr (TStr x)      = interp (prop x)
expr (TNum x)      = interp $ "new Intl.NumberFormat('en-US').format(" <> prop x <> ")"
expr (TDate x fmt) = interp =<< date x fmt
expr (TApply x ys) = interp =<< apply x ys
expr (TMatch x)    = interp =<< match x

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

date :: Ref -> ICU.DateFmt -> Compiler Text
date n d = pure $ prop n <> ".toLocaleString('en-US', { dateStyle: '" <> style d <> "' }" <> ")"
  where style ICU.Short  = "short"
        style ICU.Medium = "medium"
        style ICU.Long   = "long"
        style ICU.Full   = "full"
