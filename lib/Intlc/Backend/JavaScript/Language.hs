module Intlc.Backend.JavaScript.Language where

import           Intlc.Core (Locale)
import qualified Intlc.ICU  as ICU
import           Prelude

type ASTCompiler = Reader Locale

-- | A representation of the output we will be compiling. It's a little verbose
-- split into these various sum types, but in doing so it's correct by
-- construction.
data Stmt = Stmt Text [Expr]
  deriving (Show, Eq)

data Expr
  = TPrint Text
  | TStr Ref
  | TNum Ref
  | TDate Ref ICU.DateTimeFmt
  | TTime Ref ICU.DateTimeFmt
  | TApply Ref [Expr]
  | TMatch Match
  deriving (Show, Eq)

data Match = Match Ref MatchCond MatchRet
  deriving (Show, Eq)

data MatchCond
  = LitCond
  | CardinalPluralRuleCond
  | OrdinalPluralRuleCond
  deriving (Show, Eq)

data MatchRet
  = LitMatchRet (NonEmpty Branch)
  | NonLitMatchRet (NonEmpty Branch) Wildcard
  | RecMatchRet (NonEmpty Branch) Match
  deriving (Show, Eq)

newtype Ref = Ref Text
  deriving (Show, Eq)

data Branch = Branch Text [Expr]
  deriving (Show, Eq)

newtype Wildcard = Wildcard [Expr]
  deriving (Show, Eq)

fromKeyedMsg :: Text -> ICU.Message -> ASTCompiler Stmt
fromKeyedMsg n (ICU.Message xs) = Stmt n <$> (fromToken `mapM` xs)

fromToken :: ICU.Token -> ASTCompiler Expr
fromToken (ICU.Plaintext x)       = pure $ TPrint x
fromToken (ICU.Interpolation x y) = fromInterp x y

fromInterp :: Text -> ICU.Type -> ASTCompiler Expr
fromInterp nraw t =
  case t of
    ICU.Bool { ICU.trueCase, ICU.falseCase } -> do
      x <- fromBoolCase True trueCase
      y <- fromBoolCase False falseCase
      pure . TMatch . Match n LitCond . LitMatchRet $ x :| [y]
    ICU.String             -> pure $ TStr n
    ICU.Number             -> pure $ TNum n
    ICU.Date x             -> pure $ TDate n x
    ICU.Time x             -> pure $ TTime n x
    ICU.Plural x           -> TMatch <$> fromPlural n x
    ICU.PluralRef          -> pure $ TNum n
    ICU.Select cs (Just w) -> ((TMatch . Match n LitCond) .) . NonLitMatchRet <$> (fromSelectCase `mapM` cs) <*> fromSelectWildcard w
    ICU.Select cs Nothing  -> TMatch . Match n LitCond . LitMatchRet <$> (fromSelectCase `mapM` cs)
    ICU.Callback xs        -> TApply n <$> (fromToken `mapM` xs)
  where n = Ref nraw

fromPlural :: Ref -> ICU.Plural -> ASTCompiler Match
fromPlural r p = case p of
  ICU.Cardinal (ICU.LitPlural lcs mw)            -> Match r LitCond <$> case mw of
    Nothing -> LitMatchRet    <$> (fromExactPluralCase `mapM` lcs)
    Just w  -> NonLitMatchRet <$> (fromExactPluralCase `mapM` lcs) <*> fromPluralWildcard w
  ICU.Cardinal (ICU.RulePlural rcs w)            -> Match r CardinalPluralRuleCond <$> m
    where m = NonLitMatchRet <$> (fromRulePluralCase `mapM` rcs) <*> fromPluralWildcard w
  ICU.Cardinal (ICU.MixedPlural lcs rcs w)       -> Match r LitCond <$> m
    where m = RecMatchRet <$> (fromExactPluralCase `mapM` lcs) <*> (Match r CardinalPluralRuleCond <$> im)
          im = NonLitMatchRet <$> (fromRulePluralCase `mapM` rcs) <*> fromPluralWildcard w
  ICU.Ordinal (ICU.OrdinalPlural [] rcs w)       -> Match r OrdinalPluralRuleCond <$> m
    where m = NonLitMatchRet <$> (fromRulePluralCase `mapM` rcs) <*> fromPluralWildcard w
  ICU.Ordinal (ICU.OrdinalPlural (lc:lcs) rcs w) -> Match r LitCond <$> m
    where m = RecMatchRet <$> ((:|) <$> fromExactPluralCase lc <*> (fromExactPluralCase `mapM` lcs)) <*> im
          im = Match r OrdinalPluralRuleCond <$> (NonLitMatchRet <$> (fromRulePluralCase `mapM` rcs) <*> fromPluralWildcard w)

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

fromBoolCase :: Bool -> ICU.Stream -> ASTCompiler Branch
fromBoolCase b xs = Branch b' <$> (fromToken `mapM` xs)
  where b' = if b then "true" else "false"
