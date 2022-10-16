module Intlc.Backend.JavaScript.Language where

import qualified Data.Text  as T
import           Intlc.Core (Locale)
import qualified Intlc.ICU  as ICU
import           Prelude
import           Utils      ((<>^))

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
fromKeyedMsg n (ICU.Message x) = Stmt n <$> fromNode x

fromNode :: ICU.Node -> ASTCompiler [Expr]
fromNode ICU.Fin        = pure mempty
fromNode (ICU.Char c x) = pure (pure (TPrint (T.singleton c))) <>^ fromNode x
fromNode x@ICU.Bool {}  = do
      l <- fromBoolCase True (ICU.trueCase x)
      r <- fromBoolCase False (ICU.falseCase x)
      let start = TMatch . Match (ICU.name x) LitCond . LitMatchRet $ l :| [r]
      pure (pure start) <>^ fromNode (ICU.next x)
fromNode (ICU.String n x)      = pure (pure (TStr n)) <>^ fromNode x
fromNode (ICU.Number n x)      = pure (pure (TNum n)) <>^ fromNode x
fromNode (ICU.Date n x y)      = pure (pure (TDate n x)) <>^ fromNode y
fromNode (ICU.Time n x y)      = pure (pure (TTime n x)) <>^ fromNode y
fromNode (ICU.CardinalExact n lcs x)              = (pure . TMatch . Match n LitCond . LitMatchRet <$> (fromExactPluralCase `mapM` lcs)) <>^ fromNode x
fromNode (ICU.CardinalInexact n lcs [] w x)       = (pure . TMatch . Match n LitCond <$> ret) <>^ fromNode x
    where ret = NonLitMatchRet <$> (fromExactPluralCase `mapM` lcs) <*> fromPluralWildcard w
fromNode (ICU.CardinalInexact n [] rcs w x)       = (pure . TMatch . Match n CardinalPluralRuleCond <$> ret) <>^ fromNode x
    where ret = NonLitMatchRet <$> (fromRulePluralCase `mapM` rcs) <*> fromPluralWildcard w
fromNode (ICU.CardinalInexact n (lc:lcs) rcs w x) = (pure . TMatch . Match n LitCond <$> litRet) <>^ fromNode x
    where litRet = RecMatchRet <$> (fromExactPluralCase `mapM` lcs') <*> (Match n CardinalPluralRuleCond <$> ruleRet)
          ruleRet = NonLitMatchRet <$> (fromRulePluralCase `mapM` rcs) <*> fromPluralWildcard w
          lcs' = lc :| lcs
fromNode (ICU.Ordinal n [] rcs w x)               = (pure . TMatch . Match n OrdinalPluralRuleCond <$> m) <>^ fromNode x
    where m = NonLitMatchRet <$> (fromRulePluralCase `mapM` rcs) <*> fromPluralWildcard w
fromNode (ICU.Ordinal n (lc:lcs) rcs w x)         = (pure . TMatch . Match n LitCond <$> m) <>^ fromNode x
    where m = RecMatchRet <$> ((:|) <$> fromExactPluralCase lc <*> (fromExactPluralCase `mapM` lcs)) <*> im
          im = Match n OrdinalPluralRuleCond <$> (NonLitMatchRet <$> (fromRulePluralCase `mapM` rcs) <*> fromPluralWildcard w)

fromNode (ICU.PluralRef n x)   = pure (pure (TNum n)) <>^ fromNode x
fromNode (ICU.SelectNamed n cs x)       = (pure . TMatch . Match n LitCond . LitMatchRet <$> ret) <>^ fromNode x
  where ret = fromSelectCase `mapM` cs
fromNode (ICU.SelectWild n w x)         = (pure . TMatch . Match n LitCond <$> ret) <>^ fromNode x
  where ret = NonLitMatchRet mempty <$> fromSelectWildcard w
fromNode (ICU.SelectNamedWild n cs w x) = (pure . TMatch . Match n LitCond <$> ret) <>^ fromNode x
  where ret = NonLitMatchRet <$> (toList <$> fromSelectCase `mapM` cs) <*> fromSelectWildcard w
fromNode (ICU.Callback n x y) = (pure . TApply n <$> fromNode x) <>^ fromNode y

fromExactPluralCase :: ICU.PluralCase ICU.PluralExact -> ASTCompiler Branch
fromExactPluralCase (ICU.PluralExact n, x) = Branch n <$> fromNode x

fromRulePluralCase :: ICU.PluralCase ICU.PluralRule -> ASTCompiler Branch
fromRulePluralCase (r, x) = Branch (qts matcher) <$> fromNode x
  where matcher = case r of
         ICU.Zero -> "zero"
         ICU.One  -> "one"
         ICU.Two  -> "two"
         ICU.Few  -> "few"
         ICU.Many -> "many"
        qts y = "'" <> y <> "'"

fromPluralWildcard :: ICU.Node -> ASTCompiler Wildcard
fromPluralWildcard x = Wildcard <$> fromNode x

fromSelectCase :: ICU.SelectCase -> ASTCompiler Branch
fromSelectCase (x, y) = Branch ("'" <> x <> "'") <$> fromNode y

fromSelectWildcard :: ICU.Node -> ASTCompiler Wildcard
fromSelectWildcard x = Wildcard <$> fromNode x

fromBoolCase :: Bool -> ICU.Node -> ASTCompiler Branch
fromBoolCase b x = Branch b' <$> fromNode x
  where b' = if b then "true" else "false"
