module Intlc.Backend.JavaScript.Language where

import           Data.Functor.Foldable (cataA)
import qualified Data.Text             as T
import           Intlc.Core            (Locale)
import qualified Intlc.ICU             as ICU
import           Prelude
import           Utils                 ((<>^))

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

fromKeyedMsg :: Text -> ICU.Message ICU.Node -> ASTCompiler Stmt
fromKeyedMsg n (ICU.Message x) = Stmt n <$> fromNode x

fromNode :: ICU.Node -> ASTCompiler [Expr]
fromNode = cataA $ \case
  ICU.FinF        -> pure mempty
  (ICU.CharF c x) -> pure (pure (TPrint (T.singleton c))) <>^ x
  x@ICU.BoolF {}  -> do
        l <- fromBoolCase True (ICU.trueCaseF x)
        r <- fromBoolCase False (ICU.falseCaseF x)
        let start = TMatch . Match (ICU.nameF x) LitCond . LitMatchRet $ l :| [r]
        pure (pure start) <>^ ICU.nextF x
  (ICU.StringF n x)      -> pure (pure (TStr n)) <>^ x
  (ICU.NumberF n x)      -> pure (pure (TNum n)) <>^ x
  (ICU.DateF n x y)      -> pure (pure (TDate n x)) <>^ y
  (ICU.TimeF n x y)      -> pure (pure (TTime n x)) <>^ y
  (ICU.CardinalExactF n lcs x)              -> (pure . TMatch . Match n LitCond . LitMatchRet <$> (fromExactPluralCase `mapM` lcs)) <>^ x
  (ICU.CardinalInexactF n lcs [] w x)       -> (pure . TMatch . Match n LitCond <$> ret) <>^ x
      where ret = NonLitMatchRet <$> (fromExactPluralCase `mapM` lcs) <*> (Wildcard <$> w)
  (ICU.CardinalInexactF n [] rcs w x)       -> (pure . TMatch . Match n CardinalPluralRuleCond <$> ret) <>^ x
      where ret = NonLitMatchRet <$> (fromRulePluralCase `mapM` rcs) <*> (Wildcard <$> w)
  (ICU.CardinalInexactF n (lc:lcs) rcs w x) -> (pure . TMatch . Match n LitCond <$> litRet) <>^ x
      where litRet = RecMatchRet <$> (fromExactPluralCase `mapM` lcs') <*> (Match n CardinalPluralRuleCond <$> ruleRet)
            ruleRet = NonLitMatchRet <$> (fromRulePluralCase `mapM` rcs) <*> (Wildcard <$> w)
            lcs' = lc :| lcs
  (ICU.OrdinalF n [] rcs w x)               -> (pure . TMatch . Match n OrdinalPluralRuleCond <$> m) <>^ x
      where m = NonLitMatchRet <$> (fromRulePluralCase `mapM` rcs) <*> (Wildcard <$> w)
  (ICU.OrdinalF n (lc:lcs) rcs w x)         -> (pure . TMatch . Match n LitCond <$> m) <>^ x
      where m = RecMatchRet <$> ((:|) <$> fromExactPluralCase lc <*> (fromExactPluralCase `mapM` lcs)) <*> im
            im = Match n OrdinalPluralRuleCond <$> (NonLitMatchRet <$> (fromRulePluralCase `mapM` rcs) <*> (Wildcard <$> w))
  (ICU.PluralRefF n x)   -> pure (pure (TNum n)) <>^ x
  (ICU.SelectNamedF n cs x)       -> (pure . TMatch . Match n LitCond . LitMatchRet <$> ret) <>^ x
    where ret = fromSelectCase `mapM` cs
  (ICU.SelectWildF n w x)         -> (pure . TMatch . Match n LitCond <$> ret) <>^ x
    where ret = NonLitMatchRet mempty <$> (Wildcard <$> w)
  (ICU.SelectNamedWildF n cs w x) -> (pure . TMatch . Match n LitCond <$> ret) <>^ x
    where ret = NonLitMatchRet <$> (toList <$> fromSelectCase `mapM` cs) <*> (Wildcard <$> w)
  (ICU.CallbackF n x y) -> (pure . TApply n <$> x) <>^ y

fromExactPluralCase :: ICU.PluralCaseF ICU.PluralExact (ASTCompiler [Expr]) -> ASTCompiler Branch
fromExactPluralCase (ICU.PluralExact n, x) = Branch n <$> x

fromRulePluralCase :: ICU.PluralCaseF ICU.PluralRule (ASTCompiler [Expr]) -> ASTCompiler Branch
fromRulePluralCase (r, x) = Branch (qts matcher) <$> x
  where matcher = case r of
         ICU.Zero -> "zero"
         ICU.One  -> "one"
         ICU.Two  -> "two"
         ICU.Few  -> "few"
         ICU.Many -> "many"
        qts y = "'" <> y <> "'"

fromSelectCase :: ICU.SelectCaseF (ASTCompiler [Expr]) -> ASTCompiler Branch
fromSelectCase (x, y) = Branch ("'" <> x <> "'") <$> y

fromBoolCase :: Bool -> ASTCompiler [Expr] -> ASTCompiler Branch
fromBoolCase b x = Branch b' <$> x
  where b' = if b then "true" else "false"
