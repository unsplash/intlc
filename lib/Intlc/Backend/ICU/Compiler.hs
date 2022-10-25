-- This module is essentially the inverse of the parsing we perform; it's
-- semantically reversible in that respect if not precisely due to formatting,
-- no preservation of the presence of defaults, etc.
--
-- This is a special backend in that it's not exposed as a "backend" but is
-- instead used post-flattening. Additionally it only operates upon individual
-- ICU messages, offloading JSON handling to the caller.

module Intlc.Backend.ICU.Compiler (compileMsg, Formatting (..), pluralExact, pluralRule) where

import           Data.Functor.Foldable (cata)
import qualified Data.Text             as T
import           Intlc.ICU
import           Prelude
import           Utils                 ((<>^))

compileMsg :: Formatting -> Message -> Text
compileMsg x y = node x (unMessage y)

data Formatting
  = SingleLine
  | MultiLine

data Config = Config
  -- Expected to be potentially supplied externally.
  { fmt          :: Formatting
  -- Expected to be supplied internally.
  , indentLevels :: Int
  }

type Compiler = Reader Config

increment :: Compiler a -> Compiler a
increment = local $ \x -> x { indentLevels = x.indentLevels + 1 }

node :: Formatting -> Node -> Text
node fo ast = runReader (cata go ast) (Config fo 0) where
  go :: NodeF (Compiler Text) -> Compiler Text
  go = \case
    FinF -> pure mempty

    (CharF c next) -> (T.singleton c <>) <$> next

    (BoolF { nameF, trueCaseF, falseCaseF, nextF }) ->
      let cs = sequence [("true",) <$> trueCaseF, ("false",) <$> falseCaseF]
       in (boolean nameF cs) <>^ nextF

    (StringF n next) -> (string n <>) <$> next

    (NumberF n next) -> (number n <>) <$> next

    (DateF n fmt next) -> (date n fmt <>) <$> next

    (TimeF n fmt next) -> (time n fmt <>) <$> next

    (CardinalExactF n xs next) -> (cardinal n $ exactPluralCases xs) <>^ next

    (CardinalInexactF n xs ys w next) ->
      let cs = join <$> sequence [exactPluralCases xs, rulePluralCases ys, pure . wildcard <$> w]
       in (cardinal n cs) <>^ next

    (OrdinalF n xs ys w next) ->
      let cs = join <$> sequence [exactPluralCases xs, rulePluralCases ys, pure . wildcard <$> w]
       in (ordinal n cs) <>^ next

    (PluralRefF _ next) -> ("#" <>) <$> next

    (SelectNamedF n xs y) -> (select n $ selectCases xs) <>^ y

    (SelectWildF n w x) -> (select n $ pure . wildcard <$> w) <>^ x

    (SelectNamedWildF n xs w next) ->
      let cs = (<>) <$> selectCases xs <*> (pure . wildcard <$> w)
       in (select n cs) <>^ next

    (CallbackF n xs next) -> (callback n <$> xs) <>^ next

cardinal :: Arg -> Compiler [Case] -> Compiler Text
cardinal n x = typedInterp "plural" n <$> (pure <$> cases x)

ordinal :: Arg -> Compiler [Case] -> Compiler Text
ordinal n x = typedInterp "selectordinal" n <$> (pure <$> cases x)

select :: Arg -> Compiler [Case] -> Compiler Text
select n x = typedInterp "select" n <$> (pure <$> cases x)

boolean :: Arg -> Compiler [Case] -> Compiler Text
boolean n x = typedInterp "boolean" n <$> (pure <$> cases x)

datetime :: Text -> Arg -> DateTimeFmt -> Text
datetime t n f = typedInterp t n (pure . dateTimeFmt $ f)

date :: Arg -> DateTimeFmt -> Text
date = datetime "date"

time :: Arg -> DateTimeFmt -> Text
time = datetime "time"

typedInterp :: Text -> Arg -> [Text] -> Text
typedInterp t n xs = interp n (t : xs)

number :: Arg -> Text
number = flip interp (pure "number")

string :: Arg -> Text
string = flip interp mempty

interp :: Arg -> [Text] -> Text
interp n xs = "{" <> interpPieces (unArg n : xs) <> "}"

interpPieces :: [Text] -> Text
interpPieces = T.intercalate ", "

callback :: Arg -> Text -> Text
callback n x = "<" <> unArg n <> ">" <> x <> "</" <> unArg n <> ">"

type Case = (Text, Text)

-- | This is where we'll manage indentation for all case-style interpolations,
-- hence taking a monadic input.
cases :: Compiler [Case] -> Compiler Text
cases mcs = asks fmt >>= \case
  SingleLine -> unwords . fmap (uncurry case') <$> mcs
  MultiLine  -> do
    i <- asks indentLevels
    let indentedCase = (indentBy (i + 1) <>) . uncurry case'
    cs <- fmap indentedCase <$> increment mcs
    pure $ newline <> T.intercalate newline cs <> newline <> indentBy i
    where newline = "\n"
          indentBy = flip T.replicate "\t"

case' :: Text -> Text -> Text
case' n x = n <> " {" <> x <> "}"

wildcard :: Text -> Case
wildcard = ("other",)

selectCases :: Traversable t => t (SelectCaseF (Compiler Text)) -> Compiler [Case]
selectCases = fmap toList . traverse selectCaseF

selectCaseF :: Functor f => SelectCaseF (f Text) -> f Case
selectCaseF (n, mx) = selectCase . (n,) <$> mx

selectCase :: SelectCaseF Text -> Case
selectCase = id

exactPluralCases :: Traversable t => t (PluralCaseF PluralExact (Compiler Text)) -> Compiler [Case]
exactPluralCases = fmap toList . traverse exactPluralCaseF

exactPluralCaseF :: PluralCaseF PluralExact (Compiler Text) -> Compiler Case
exactPluralCaseF (n, mx) = exactPluralCase . (n,) <$> mx

exactPluralCase :: PluralCaseF PluralExact Text -> Case
exactPluralCase = first pluralExact

rulePluralCases :: Traversable t => t (PluralCaseF PluralRule (Compiler Text)) -> Compiler [Case]
rulePluralCases = fmap toList . traverse rulePluralCaseF

rulePluralCaseF :: PluralCaseF PluralRule (Compiler Text) -> Compiler Case
rulePluralCaseF (r, mx) = rulePluralCase . (r,) <$> mx

rulePluralCase :: PluralCaseF PluralRule Text -> Case
rulePluralCase = first pluralRule

pluralRule :: PluralRule -> Text
pluralRule Zero = "zero"
pluralRule One  = "one"
pluralRule Two  = "two"
pluralRule Few  = "few"
pluralRule Many = "many"

pluralExact :: PluralExact -> Text
pluralExact (PluralExact n) = "=" <> n

dateTimeFmt :: DateTimeFmt -> Text
dateTimeFmt Short  = "short"
dateTimeFmt Medium = "medium"
dateTimeFmt Long   = "long"
dateTimeFmt Full   = "full"
