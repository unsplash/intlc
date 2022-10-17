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

compileMsg :: Message -> Text
compileMsg = node . unMessage

node :: Node -> Text
node = cata $ \case
  FinF -> mempty
  (CharF c x)     -> T.singleton c <> x
  x@(BoolF {})    -> "{" <> (unArg . nameF $ x) <> ", boolean, true {" <> trueCaseF x <> "} false {" <> falseCaseF x <> "}}" <> nextF x
  (StringF n x)   -> "{" <> unArg n <> "}" <> x
  (NumberF n x)   -> "{" <> unArg n <> ", number}" <> x
  (DateF n fmt x) -> "{" <> unArg n <> ", date, " <> dateTimeFmt fmt  <> "}" <> x
  (TimeF n fmt x) -> "{" <> unArg n <> ", time, " <> dateTimeFmt fmt  <> "}" <> x
  (CardinalExactF n xs y)        -> "{" <> unArg n <> ", plural, " <> cases <> "}" <> y
    where cases = unwords . toList . fmap exactPluralCase $ xs
  (CardinalInexactF n xs ys w z) -> "{" <> unArg n <> ", plural, " <> cases <> "}" <> z
    where cases = unwords . mconcat $ [exactPluralCase <$> xs, rulePluralCase <$> ys, pure $ wildcard w]
  (OrdinalF n xs ys w z)         -> "{" <> unArg n <> ", selectordinal, " <> cases <> "}" <> z
    where cases = unwords $ (exactPluralCase <$> xs) <> (rulePluralCase <$> ys) <> pure (wildcard w)
  (PluralRefF _ x)    -> "#" <> x
  (SelectNamedF n xs y)       -> "{" <> unArg n <> ", select, " <> cases <> "}" <> y
    where cases = unwords . fmap selectCase . toList $ xs
  (SelectWildF n w x)         -> "{" <> unArg n <> ", select, " <> wildcard w <> "}" <> x
  (SelectNamedWildF n xs w y) -> "{" <> unArg n <> ", select, " <> cases <> "}" <> y
    where cases = unwords . (<> pure (wildcard w)) . fmap selectCase . toList $ xs
  (CallbackF n xs y) -> "<" <> unArg n <> ">" <> xs <> "</" <> unArg n <> ">" <> y

dateTimeFmt :: DateTimeFmt -> Text
dateTimeFmt Short  = "short"
dateTimeFmt Medium = "medium"
dateTimeFmt Long   = "long"
dateTimeFmt Full   = "full"

exactPluralCase :: PluralCaseF PluralExact Text -> Text
exactPluralCase (PluralExact n, x) = "=" <> n <> " {" <> x <> "}"

rulePluralCase :: PluralCaseF PluralRule Text -> Text
rulePluralCase (r, x) = pluralRule r <> " {" <> x <> "}"

pluralRule :: PluralRule -> Text
pluralRule Zero = "zero"
pluralRule One  = "one"
pluralRule Two  = "two"
pluralRule Few  = "few"
pluralRule Many = "many"

selectCase :: SelectCaseF Text -> Text
selectCase (n, x) = n <> " {" <> x <> "}"

wildcard :: Text -> Text
wildcard x = "other {" <> x <> "}"
