module Intlc.Backend.JavaScript.Language where

import           Intlc.Core (Locale)
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

data MatchOn = MatchOn Ref MatchCond MatchRet

data MatchCond
  = LitCond
  | CardinalPluralRuleCond
  | OrdinalPluralRuleCond

data MatchRet
  = LitMatchRet (NonEmpty Branch)
  | NonLitMatchRet (NonEmpty Branch) Wildcard
  | RecMatchRet (NonEmpty Branch) MatchOn

newtype Ref = Ref Text

data Branch = Branch Text [Expr]
newtype Wildcard = Wildcard [Expr]

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
    ICU.Select cs (Just w) -> ((TMatch . MatchOn n LitCond) .) . NonLitMatchRet <$> (fromSelectCase `mapM` cs) <*> fromSelectWildcard w
    ICU.Select cs Nothing  -> TMatch . MatchOn n LitCond . LitMatchRet <$> (fromSelectCase `mapM` cs)
    ICU.Callback xs        -> TApply n <$> (fromToken `mapM` xs)
  where n = Ref nraw

fromPlural :: Ref -> ICU.Plural -> ASTCompiler MatchOn
fromPlural r p = case p of
  ICU.Cardinal (ICU.LitPlural lcs mw)            -> MatchOn r LitCond <$> case mw of
    Nothing -> LitMatchRet    <$> (fromExactPluralCase `mapM` lcs)
    Just w  -> NonLitMatchRet <$> (fromExactPluralCase `mapM` lcs) <*> fromPluralWildcard w
  ICU.Cardinal (ICU.RulePlural rcs w)            -> MatchOn r CardinalPluralRuleCond <$> m
    where m = NonLitMatchRet <$> (fromRulePluralCase `mapM` rcs) <*> fromPluralWildcard w
  ICU.Cardinal (ICU.MixedPlural lcs rcs w)       -> MatchOn r LitCond <$> m
    where m = RecMatchRet <$> (fromExactPluralCase `mapM` lcs) <*> (MatchOn r CardinalPluralRuleCond <$> im)
          im = NonLitMatchRet <$> (fromRulePluralCase `mapM` rcs) <*> fromPluralWildcard w
  ICU.Ordinal (ICU.OrdinalPlural [] rcs w)       -> MatchOn r OrdinalPluralRuleCond <$> m
    where m = NonLitMatchRet <$> (fromRulePluralCase `mapM` rcs) <*> fromPluralWildcard w
  ICU.Ordinal (ICU.OrdinalPlural (lc:lcs) rcs w) -> MatchOn r LitCond <$> m
    where m = RecMatchRet <$> ((:|) <$> fromExactPluralCase lc <*> (fromExactPluralCase `mapM` lcs)) <*> im
          im = MatchOn r OrdinalPluralRuleCond <$> (NonLitMatchRet <$> (fromRulePluralCase `mapM` rcs) <*> fromPluralWildcard w)

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
