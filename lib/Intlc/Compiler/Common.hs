module Intlc.Compiler.Common where

import           Data.List  (nubBy)
import           Intlc.Core
import           Prelude

-- | Validates and dedupes `Arg`s. Duplicates are allowed and will be removed,
-- different types are incompatible and will be flagged as an error. Order is
-- preserved biased to the left.
validateArgs :: [Arg] -> Either (NonEmpty Text) [Arg]
validateArgs xs = dedupe xs <$ validate xs
  where validate = toEither . foldr checkCompat ([], []) . reverse
          where checkCompat x@(Arg _ xt) (es, seen) = find (eqName x) seen & \case
                  Nothing -> (es, x : seen)
                  Just y@(Arg _ yt)  -> if sameUnderlyingType xt yt then (es, seen) else (typeMismatchErr x y : es, seen)
                toEither (e:es, _) = Left (e :| es)
                toEither _         = Right ()
                typeMismatchErr (Arg n xt) (Arg _ yt) =
                  "Incompatible interpolation types for `" <> n <> "`. Found " <> friendlyInputType xt <> ", expected " <> friendlyInputType yt
        dedupe = nubBy eqName
        eqName (Arg x _) (Arg y _) = x == y

friendlyInputType :: ICUType -> Text
friendlyInputType String      = "string"
friendlyInputType Date {}     = "date"
friendlyInputType Number      = "number"
friendlyInputType Plural {}   = "number"
friendlyInputType Callback {} = "tag"

sameUnderlyingType :: ICUType -> ICUType -> Bool
sameUnderlyingType = (==) `on` friendlyInputType
