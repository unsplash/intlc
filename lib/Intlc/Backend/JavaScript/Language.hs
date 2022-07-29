module Intlc.Backend.JavaScript.Language where

import           Data.These (These (..))
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
  | TStr ICU.Arg
  | TNum ICU.Arg
  | TDate ICU.Arg ICU.DateTimeFmt
  | TTime ICU.Arg ICU.DateTimeFmt
  | TApply ICU.Arg [Expr]
  | TMatch Match
  deriving (Show, Eq)

data Match = Match ICU.Arg MatchCond MatchRet
  deriving (Show, Eq)

data MatchCond
  = LitCond
  | CardinalPluralRuleCond
  | OrdinalPluralRuleCond
  deriving (Show, Eq)

data MatchRet
  = LitMatchRet (NonEmpty Branch)
  | NonLitMatchRet [Branch] Wildcard
  | RecMatchRet (NonEmpty Branch) Match
  deriving (Show, Eq)

data Branch = Branch Text [Expr]
  deriving (Show, Eq)

newtype Wildcard = Wildcard [Expr]
  deriving (Show, Eq)

fromKeyedMsg :: Text -> ICU.Message -> ASTCompiler Stmt
fromKeyedMsg n (ICU.Message xs) = Stmt n <$> (fromNode `mapM` xs)

fromNode :: ICU.Node -> ASTCompiler Expr
fromNode (ICU.Plaintext x)   = pure $ TPrint x
fromNode x@ICU.Bool {}       = do
      l <- fromBoolCase True (ICU.trueCase x)
      r <- fromBoolCase False (ICU.falseCase x)
      pure . TMatch . Match (ICU.name x) LitCond . LitMatchRet $ l :| [r]
fromNode (ICU.String n)      = pure $ TStr n
fromNode (ICU.Number n)      = pure $ TNum n
fromNode (ICU.Date n x)      = pure $ TDate n x
fromNode (ICU.Time n x)      = pure $ TTime n x
fromNode (ICU.Plural n x)    = TMatch <$> fromPlural n x
fromNode (ICU.PluralRef n)   = pure $ TNum n
fromNode (ICU.Select n x)    = case x of
      (This cs)    -> TMatch . Match n LitCond . LitMatchRet <$> ret
        where ret = fromSelectCase `mapM` cs
      (That w)     -> TMatch . Match n LitCond <$> ret
        where ret = NonLitMatchRet mempty <$> fromSelectWildcard w
      (These cs w) -> TMatch . Match n LitCond <$> ret
        where ret = NonLitMatchRet <$> (toList <$> fromSelectCase `mapM` cs) <*> fromSelectWildcard w
fromNode (ICU.Callback n xs) = TApply n <$> (fromNode `mapM` xs)

fromPlural :: ICU.Arg -> ICU.Plural -> ASTCompiler Match
fromPlural r p = case p of
  ICU.CardinalExact lcs              -> Match r LitCond . LitMatchRet <$> (fromExactPluralCase `mapM` lcs)
  ICU.CardinalInexact lcs [] w       -> Match r LitCond <$> ret
    where ret = NonLitMatchRet <$> (fromExactPluralCase `mapM` lcs) <*> fromPluralWildcard w
  ICU.CardinalInexact [] rcs w       -> Match r CardinalPluralRuleCond <$> ret
    where ret = NonLitMatchRet <$> (fromRulePluralCase `mapM` rcs) <*> fromPluralWildcard w
  ICU.CardinalInexact (lc:lcs) rcs w -> Match r LitCond <$> litRet
    where litRet = RecMatchRet <$> (fromExactPluralCase `mapM` lcs') <*> (Match r CardinalPluralRuleCond <$> ruleRet)
          ruleRet = NonLitMatchRet <$> (fromRulePluralCase `mapM` rcs) <*> fromPluralWildcard w
          lcs' = lc :| lcs
  ICU.Ordinal [] rcs w               -> Match r OrdinalPluralRuleCond <$> m
    where m = NonLitMatchRet <$> (fromRulePluralCase `mapM` rcs) <*> fromPluralWildcard w
  ICU.Ordinal (lc:lcs) rcs w         -> Match r LitCond <$> m
    where m = RecMatchRet <$> ((:|) <$> fromExactPluralCase lc <*> (fromExactPluralCase `mapM` lcs)) <*> im
          im = Match r OrdinalPluralRuleCond <$> (NonLitMatchRet <$> (fromRulePluralCase `mapM` rcs) <*> fromPluralWildcard w)

fromExactPluralCase :: ICU.PluralCase ICU.PluralExact -> ASTCompiler Branch
fromExactPluralCase (ICU.PluralCase (ICU.PluralExact n) xs) = Branch n <$> (fromNode `mapM` xs)

fromRulePluralCase :: ICU.PluralCase ICU.PluralRule -> ASTCompiler Branch
fromRulePluralCase (ICU.PluralCase r xs) = Branch (qts matcher) <$> (fromNode `mapM` xs)
  where matcher = case r of
         ICU.Zero -> "zero"
         ICU.One  -> "one"
         ICU.Two  -> "two"
         ICU.Few  -> "few"
         ICU.Many -> "many"
        qts x = "'" <> x <> "'"

fromPluralWildcard :: ICU.PluralWildcard -> ASTCompiler Wildcard
fromPluralWildcard (ICU.PluralWildcard xs) = Wildcard <$> (fromNode `mapM` xs)

fromSelectCase :: ICU.SelectCase -> ASTCompiler Branch
fromSelectCase (ICU.SelectCase x ys) = Branch ("'" <> x <> "'") <$> (fromNode `mapM` ys)

fromSelectWildcard :: ICU.SelectWildcard -> ASTCompiler Wildcard
fromSelectWildcard (ICU.SelectWildcard xs) = Wildcard <$> (fromNode `mapM` xs)

fromBoolCase :: Bool -> ICU.Stream -> ASTCompiler Branch
fromBoolCase b xs = Branch b' <$> (fromNode `mapM` xs)
  where b' = if b then "true" else "false"
