-- This module is essentially the inverse of the parsing we perform; it's
-- semantically reversible in that respect if not precisely due to formatting,
-- no preservation of the presence of defaults, etc.
--
-- This is a special backend in that it's not exposed as a "backend" but is
-- instead used post-flattening. Additionally it only operates upon individual
-- ICU messages, offloading JSON handling to the caller.

module Intlc.Backend.ICU.Compiler where

import           Data.Functor.Foldable (cata)
import qualified Data.Text             as T
import           Intlc.ICU
import           Prelude
import           Utils                 ((<>^))

compileMsg :: Message -> Text
compileMsg = node . unMessage

node :: Node -> Text
node ast = runReader (cata go ast) (0 :: Int) where
  go :: Monad m => NodeF (m Text) -> m Text
  go = \case
    FinF -> pure mempty

    (CharF c next) -> (T.singleton c <>) <$> next

    (BoolF { nameF, trueCaseF, falseCaseF, nextF }) ->
      let cs = sequence [("true",) <$> trueCaseF, ("false",) <$> falseCaseF]
       in (boolean nameF <$> cs) <>^ nextF

    (StringF n next) -> (string n <>) <$> next

    (NumberF n next) -> (number n <>) <$> next

    (DateF n fmt next) -> (date n fmt <>) <$> next

    (TimeF n fmt next) -> (time n fmt <>) <$> next

    (CardinalExactF n xs next) -> (cardinal n <$> exactPluralCases xs) <>^ next

    (CardinalInexactF n xs ys w next) ->
      let cs = join <$> sequence [exactPluralCases xs, rulePluralCases ys, pure . wildcard <$> w]
       in (cardinal n <$> cs) <>^ next

    (OrdinalF n xs ys w next) ->
      let cs = join <$> sequence [exactPluralCases xs, rulePluralCases ys, pure . wildcard <$> w]
       in (ordinal n <$> cs) <>^ next

    (PluralRefF _ next) -> ("#" <>) <$> next

    (SelectNamedF n xs y) -> (select n <$> selectCases xs) <>^ y

    (SelectWildF n w x) -> (select n . pure . wildcard <$> w) <>^ x

    (SelectNamedWildF n xs w next) ->
      let cs = (<>) <$> selectCases xs <*> (pure . wildcard <$> w)
       in (select n <$> cs) <>^ next

    (CallbackF n xs next) -> (callback n <$> xs) <>^ next

cardinal :: Arg -> [Case] -> Text
cardinal n x = typedInterp "plural" n (pure . cases $ x)

ordinal :: Arg -> [Case] -> Text
ordinal n x = typedInterp "selectordinal" n (pure . cases $ x)

select :: Arg -> [Case] -> Text
select n x = typedInterp "select" n (pure . cases $ x)

boolean :: Arg -> [Case] -> Text
boolean n x = typedInterp "boolean" n (pure . cases $ x)

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

cases :: [Case] -> Text
cases = unwords . fmap (uncurry case')

case' :: Text -> Text -> Text
case' n x = n <> " {" <> x <> "}"

wildcard :: Text -> Case
wildcard = ("other",)

selectCases :: (Traversable t, Applicative f) => t (SelectCaseF (f Text)) -> f [Case]
selectCases = fmap toList . traverse selectCaseF

selectCaseF :: Functor f => SelectCaseF (f Text) -> f Case
selectCaseF (n, mx) = selectCase . (n,) <$> mx

selectCase :: SelectCaseF Text -> Case
selectCase = id

exactPluralCases :: (Traversable t, Applicative f) => t (PluralCaseF PluralExact (f Text)) -> f [Case]
exactPluralCases = fmap toList . traverse exactPluralCaseF

exactPluralCaseF :: Functor f => PluralCaseF PluralExact (f Text) -> f Case
exactPluralCaseF (n, mx) = exactPluralCase . (n,) <$> mx

exactPluralCase :: PluralCaseF PluralExact Text -> Case
exactPluralCase (PluralExact n, x) = ("=" <> n, x)

rulePluralCases :: (Traversable t, Applicative f) => t (PluralCaseF PluralRule (f Text)) -> f [Case]
rulePluralCases = fmap toList . traverse rulePluralCaseF

rulePluralCaseF :: Functor f => PluralCaseF PluralRule (f Text) -> f Case
rulePluralCaseF (r, mx) = rulePluralCase . (r,) <$> mx

rulePluralCase :: PluralCaseF PluralRule Text -> Case
rulePluralCase = first pluralRule

pluralRule :: PluralRule -> Text
pluralRule Zero = "zero"
pluralRule One  = "one"
pluralRule Two  = "two"
pluralRule Few  = "few"
pluralRule Many = "many"

dateTimeFmt :: DateTimeFmt -> Text
dateTimeFmt Short  = "short"
dateTimeFmt Medium = "medium"
dateTimeFmt Long   = "long"
dateTimeFmt Full   = "full"
