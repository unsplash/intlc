-- This module is essentially the inverse of the parsing we perform; it's
-- semantically reversible in that respect if not precisely due to formatting,
-- no preservation of the presence of defaults, etc.
--
-- This is a special backend in that it's not exposed as a "backend" but is
-- instead used post-flattening. Additionally it only operates upon individual
-- ICU messages, offloading JSON handling to the caller.

module Intlc.Backend.ICU.Compiler where

import           Intlc.ICU
import           Prelude   hiding (Type)

compileMsg :: Message -> Text
compileMsg (Message xs) = stream xs

stream :: Foldable f => f Token -> Text
stream = foldMap token

token :: Token -> Text
token (Plaintext x)       = x
token (Interpolation x y) = interp x y

interp :: Text -> Type -> Text
interp n Bool { trueCase, falseCase } = "{" <> n <> ", boolean, true {" <> stream trueCase <> "} false {" <> stream falseCase <> "}}"
interp n String                       = "{" <> n <> "}"
interp n Number                       = "{" <> n <> ", number}"
interp n (Date fmt)                   = "{" <> n <> ", date, "          <> dateTimeFmt fmt  <> "}"
interp n (Time fmt)                   = "{" <> n <> ", time, "          <> dateTimeFmt fmt  <> "}"
interp n (Plural (Cardinal p))        = "{" <> n <> ", plural, "        <> cardinalPlural p <> "}"
interp n (Plural (Ordinal p))         = "{" <> n <> ", selectordinal, " <> ordinalPlural p  <> "}"
interp _ PluralRef                    = "#"
interp n (Select xs y)                = "{" <> n <> ", select, "        <> select xs y      <> "}"
interp n (Callback xs)                = "<" <> n <> ">"                 <> stream xs        <> "</" <> n <> ">"

dateTimeFmt :: DateTimeFmt -> Text
dateTimeFmt Short  = "short"
dateTimeFmt Medium = "medium"
dateTimeFmt Long   = "long"
dateTimeFmt Full   = "full"

cardinalPlural :: CardinalPlural -> Text
cardinalPlural (LitPlural xs mw)     = unwords $ toList (exactPluralCase <$> xs) <> foldMap (pure . pluralWildcard) mw
cardinalPlural (RulePlural xs w)     = unwords . toList $ (rulePluralCase <$> xs) <> pure (pluralWildcard w)
cardinalPlural (MixedPlural xs ys w) = unwords . toList $ (exactPluralCase <$> xs) <> (rulePluralCase <$> ys) <> pure (pluralWildcard w)

ordinalPlural :: OrdinalPlural -> Text
ordinalPlural (OrdinalPlural xs ys w) = unwords $
  (exactPluralCase <$> xs) <> (rulePluralCase <$> toList ys) <> pure (pluralWildcard w)

exactPluralCase :: PluralCase PluralExact -> Text
exactPluralCase (PluralCase (PluralExact n) xs) = "=" <> n <> " {" <> stream xs <> "}"

rulePluralCase :: PluralCase PluralRule -> Text
rulePluralCase (PluralCase r xs) = pluralRule r <> " {" <> stream xs <> "}"

pluralRule :: PluralRule -> Text
pluralRule Zero = "zero"
pluralRule One  = "one"
pluralRule Two  = "two"
pluralRule Few  = "few"
pluralRule Many = "many"

pluralWildcard :: PluralWildcard -> Text
pluralWildcard (PluralWildcard xs) = "other {" <> stream xs <> "}"

select :: NonEmpty SelectCase -> Maybe SelectWildcard -> Text
select xs mw = unwords . toList $ (case' <$> toList xs) <> foldMap (pure . wild) mw
  where case' (SelectCase n ys) = n <> " {" <> stream ys <> "}"
        wild (SelectWildcard ys) = "other {" <> stream ys <> "}"
