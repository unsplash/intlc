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

stream :: Foldable f => f Node -> Text
stream = foldMap node

node :: Node -> Text
node (Plaintext x)   = x
node x@(Bool {})     = "{" <> (unArg . name $ x) <> ", boolean, true {" <> stream (trueCase x)  <> "} false {" <> stream (falseCase x) <> "}}"
node (String n)      = "{" <> unArg n <> "}"
node (Number n)      = "{" <> unArg n <> ", number}"
node (Date n fmt)    = "{" <> unArg n <> ", date, "          <> dateTimeFmt fmt  <> "}"
node (Time n fmt)    = "{" <> unArg n <> ", time, "          <> dateTimeFmt fmt  <> "}"
node (CardinalExact n xs)        = "{" <> unArg n <> ", plural, " <> cases <> "}"
  where cases = unwords . toList . fmap exactPluralCase $ xs
node (CardinalInexact n xs ys w) = "{" <> unArg n <> ", plural, " <> cases <> "}"
  where cases = unwords . mconcat $ [exactPluralCase <$> xs, rulePluralCase <$> ys, pure $ pluralWildcard w]
node (Ordinal n xs ys w)         = "{" <> unArg n <> ", selectordinal, " <> cases <> "}"
  where cases = unwords $ (exactPluralCase <$> xs) <> (rulePluralCase <$> ys) <> pure (pluralWildcard w)
node PluralRef {}    = "#"
node (Select n x)    = "{" <> unArg n <> ", select, "        <> select x         <> "}"
node (Callback n xs) = "<" <> unArg n <> ">"                 <> stream xs        <> "</" <> unArg n <> ">"

dateTimeFmt :: DateTimeFmt -> Text
dateTimeFmt Short  = "short"
dateTimeFmt Medium = "medium"
dateTimeFmt Long   = "long"
dateTimeFmt Full   = "full"

exactPluralCase :: PluralCase PluralExact -> Text
exactPluralCase (PluralExact n, xs) = "=" <> n <> " {" <> stream xs <> "}"

rulePluralCase :: PluralCase PluralRule -> Text
rulePluralCase (r, xs) = pluralRule r <> " {" <> stream xs <> "}"

pluralRule :: PluralRule -> Text
pluralRule Zero = "zero"
pluralRule One  = "one"
pluralRule Two  = "two"
pluralRule Few  = "few"
pluralRule Many = "many"

pluralWildcard :: Stream -> Text
pluralWildcard xs = "other {" <> stream xs <> "}"

select :: These (NonEmpty SelectCase) SelectWildcard -> Text
select = unwords . bifoldMap (toList . fmap case') (pure . wild)
  where case' (SelectCase n ys) = n <> " {" <> stream ys <> "}"
        wild (SelectWildcard ys) = "other {" <> stream ys <> "}"
