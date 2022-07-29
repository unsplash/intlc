-- This module is essentially the inverse of the parsing we perform; it's
-- semantically reversible in that respect if not precisely due to formatting,
-- no preservation of the presence of defaults, etc.
--
-- This is a special backend in that it's not exposed as a "backend" but is
-- instead used post-flattening. Additionally it only operates upon individual
-- ICU messages, offloading JSON handling to the caller.

module Intlc.Backend.ICU.Compiler where

import           Data.These (These (..))
import           Intlc.ICU
import           Prelude    hiding (Type)

compileMsg :: Message -> Text
compileMsg (Message xs) = stream xs

stream :: Foldable f => f Token -> Text
stream = foldMap token

token :: Token -> Text
token (Plaintext x)   = x
token x@(Bool {})     = "{" <> (unArg . name $ x) <> ", boolean, true {" <> stream (trueCase x)  <> "} false {" <> stream (falseCase x) <> "}}"
token (String n)      = "{" <> unArg n <> "}"
token (Number n)      = "{" <> unArg n <> ", number}"
token (Date n fmt)    = "{" <> unArg n <> ", date, "          <> dateTimeFmt fmt  <> "}"
token (Time n fmt)    = "{" <> unArg n <> ", time, "          <> dateTimeFmt fmt  <> "}"
token (Plural n p)    = "{" <> unArg n <> ", " <> typ <> ", " <> plural p         <> "}"
  where typ = case p of
          CardinalExact {}   -> "plural"
          CardinalInexact {} -> "plural"
          Ordinal {}         -> "selectordinal"
token PluralRef {}    = "#"
token (Select n x)    = "{" <> unArg n <> ", select, "        <> select x         <> "}"
token (Callback n xs) = "<" <> unArg n <> ">"                 <> stream xs        <> "</" <> unArg n <> ">"

dateTimeFmt :: DateTimeFmt -> Text
dateTimeFmt Short  = "short"
dateTimeFmt Medium = "medium"
dateTimeFmt Long   = "long"
dateTimeFmt Full   = "full"

plural :: Plural -> Text
plural (CardinalExact xs)        = unwords . toList . fmap exactPluralCase $ xs
plural (CardinalInexact xs ys w) = unwords . mconcat $ [exactPluralCase <$> xs, rulePluralCase <$> ys, pure $ pluralWildcard w]
plural (Ordinal xs ys w)         = unwords $
  (exactPluralCase <$> xs) <> (rulePluralCase <$> ys) <> pure (pluralWildcard w)

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

select :: These (NonEmpty SelectCase) SelectWildcard -> Text
select = unwords . bifoldMap (toList . fmap case') (pure . wild)
  where case' (SelectCase n ys) = n <> " {" <> stream ys <> "}"
        wild (SelectWildcard ys) = "other {" <> stream ys <> "}"
