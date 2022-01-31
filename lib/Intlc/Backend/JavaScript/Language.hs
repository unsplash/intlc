module Intlc.Backend.JavaScript.Language where

import           Intlc.Core (Locale (Locale))
import qualified Intlc.ICU  as ICU
import           Prelude

type ASTCompiler = Reader Locale

-- | A representation of the output we will be compiling. It's a little verbose
-- split into these various sum types, but in doing so it's correct by
-- construction.
data Stmt = Stmt Text (NonEmpty Expr)

data Expr
  = TPrint Text
  | TStr Ref
  | TNum Ref
  | TDate Ref ICU.DateTimeFmt
  | TTime Ref ICU.DateTimeFmt
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

-- | Everything shares a single argument object whence we can access
-- interpolations.
argName :: Text
argName = "x"

prop :: Ref -> Text
prop (Ref x) = argName <> "." <> x

fromKeyedMsg :: Text -> ICU.Message -> ASTCompiler Stmt
fromKeyedMsg n (ICU.Static x)   = pure $ Stmt n (pure $ TPrint x)
fromKeyedMsg n (ICU.Dynamic ys) = Stmt n <$> (fromToken `mapM` ys)

fromToken :: ICU.Token -> ASTCompiler Expr
fromToken (ICU.Plaintext x)     = pure $ TPrint x
fromToken (ICU.Interpolation x) = fromArg x

fromArg :: ICU.Arg -> ASTCompiler Expr
fromArg (ICU.Arg nraw t) =
  case t of
    ICU.String             -> pure $ TStr n
    ICU.Number             -> pure $ TNum n
    ICU.Date x             -> pure $ TDate n x
    ICU.Time x             -> pure $ TTime n x
    ICU.Plural x           -> TMatch <$> fromPlural n x
    ICU.Select cs (Just w) -> ((TMatch . (prop n,)) .) . NonLitMatch <$> (fromSelectCase `mapM` cs) <*> fromSelectWildcard w
    ICU.Select cs Nothing  -> TMatch . (prop n,) . LitMatch <$> (fromSelectCase `mapM` cs)
    ICU.Callback xs        -> TApply n <$> (fromToken `mapM` xs)
  where n = Ref nraw

fromPlural :: Ref -> ICU.Plural -> ASTCompiler MatchOn
fromPlural r (ICU.Cardinal (ICU.LitPlural lcs Nothing))       = (prop r,) . LitMatch <$> (fromExactPluralCase `mapM` lcs)
fromPlural r (ICU.Cardinal (ICU.LitPlural lcs (Just w)))      = ((prop r,) .) . NonLitMatch <$> (fromExactPluralCase `mapM` lcs) <*> fromPluralWildcard w
fromPlural r (ICU.Cardinal (ICU.RulePlural rcs w))            = (,) <$> c <*> m
  where c = cardinalCond r <$> ask
        m = NonLitMatch <$> (fromRulePluralCase `mapM` rcs) <*> fromPluralWildcard w
fromPlural r (ICU.Cardinal (ICU.MixedPlural lcs rcs w))       = ((prop r,) .) . RecMatch <$> (fromExactPluralCase `mapM` lcs) <*> nested
  where nested = (,) <$> (cardinalCond r <$> ask) <*> (NonLitMatch <$> (fromRulePluralCase `mapM` rcs) <*> fromPluralWildcard w)
fromPlural r (ICU.Ordinal (ICU.OrdinalPlural [] rcs w))       = (,) <$> c <*> m
  where c = ordinalCond r <$> ask
        m = NonLitMatch <$> (fromRulePluralCase `mapM` rcs) <*> fromPluralWildcard w
fromPlural r (ICU.Ordinal (ICU.OrdinalPlural (lc:lcs) rcs w)) = ((prop r,) .) . RecMatch <$> ((:|) <$> fromExactPluralCase lc <*> (fromExactPluralCase `mapM` lcs)) <*> nested
  where nested = (,) <$> (ordinalCond r <$> ask) <*> (NonLitMatch <$> (fromRulePluralCase `mapM` rcs) <*> fromPluralWildcard w)

cardinalCond :: Ref -> Locale -> Text
cardinalCond r (Locale l) = "new Intl.PluralRules('" <> l <> "').select(" <> prop r <> ")"

ordinalCond :: Ref -> Locale -> Text
ordinalCond r (Locale l) = "new Intl.PluralRules('" <> l <> "', { type: 'ordinal' }).select(" <> prop r <> ")"

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
