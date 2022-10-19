-- This module is essentially the inverse of the parsing we perform; it's
-- semantically reversible in that respect if not precisely due to formatting,
-- no preservation of the presence of defaults, etc.
--
-- This is a special backend in that it's not exposed as a "backend" but is
-- instead used post-flattening. Additionally it only operates upon individual
-- ICU messages, offloading JSON handling to the caller.

module Intlc.Backend.ICU.Compiler where

import qualified Data.Text as T
import           Intlc.ICU
import           Prelude

compileMsg :: Message -> Text
compileMsg = node . unMessage

node :: Node -> Text
node Fin = mempty
node (Char c x)     = T.singleton c <> node x
node x@(Bool {})    = "{" <> (unArg . name $ x) <> ", boolean, true {" <> node (trueCase x)  <> "} false {" <> node (falseCase x) <> "}}" <> node (next x)
node (String n x)   = "{" <> unArg n <> "}" <> node x
node (Number n x)   = "{" <> unArg n <> ", number}" <> node x
node (Date n fmt x) = "{" <> unArg n <> ", date, " <> dateTimeFmt fmt  <> "}" <> node x
node (Time n fmt x) = "{" <> unArg n <> ", time, " <> dateTimeFmt fmt  <> "}" <> node x
node (CardinalExact n xs y)        = "{" <> unArg n <> ", plural, " <> cases <> "}" <> node y
  where cases = unwords . toList . fmap exactPluralCase $ xs
node (CardinalInexact n xs ys w z) = "{" <> unArg n <> ", plural, " <> cases <> "}" <> node z
  where cases = unwords . mconcat $ [exactPluralCase <$> xs, rulePluralCase <$> ys, pure $ wildcard w]
node (Ordinal n xs ys w z)         = "{" <> unArg n <> ", selectordinal, " <> cases <> "}" <> node z
  where cases = unwords $ (exactPluralCase <$> xs) <> (rulePluralCase <$> ys) <> pure (wildcard w)
node (PluralRef _ x)    = "#" <> node x
node (SelectNamed n xs y)       = "{" <> unArg n <> ", select, " <> cases <> "}" <> node y
  where cases = unwords . fmap selectCase . toList $ xs
node (SelectWild n w x)         = "{" <> unArg n <> ", select, " <> wildcard w <> "}" <> node x
node (SelectNamedWild n xs w y) = "{" <> unArg n <> ", select, " <> cases <> "}" <> node y
  where cases = unwords . (<> pure (wildcard w)) . fmap selectCase . toList $ xs
node (Callback n xs y) = "<" <> unArg n <> ">" <> node xs <> "</" <> unArg n <> ">" <> node y

dateTimeFmt :: DateTimeFmt -> Text
dateTimeFmt Short  = "short"
dateTimeFmt Medium = "medium"
dateTimeFmt Long   = "long"
dateTimeFmt Full   = "full"

exactPluralCase :: PluralCase PluralExact -> Text
exactPluralCase (PluralExact n, x) = "=" <> n <> " {" <> node x <> "}"

rulePluralCase :: PluralCase PluralRule -> Text
rulePluralCase (r, x) = pluralRule r <> " {" <> node x <> "}"

pluralRule :: PluralRule -> Text
pluralRule Zero = "zero"
pluralRule One  = "one"
pluralRule Two  = "two"
pluralRule Few  = "few"
pluralRule Many = "many"

selectCase :: SelectCase -> Text
selectCase (n, x) = n <> " {" <> node x <> "}"

wildcard :: Node -> Text
wildcard x = "other {" <> node x <> "}"
