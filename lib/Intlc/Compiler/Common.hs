module Intlc.Compiler.Common (validateArgs) where

import           Intlc.ICU
import           Prelude   hiding (Type)

-- | Validates `ICU.Arg`s. Duplicates are allowed if their types are compatible.
validateArgs :: [Arg] -> Either (NonEmpty Text) ()
validateArgs = toEither . first reverse . foldr checkCompat ([], []) . reverse
  where checkCompat x@(Arg _ xt) (es, seen) = find (eqName x) seen & \case
          Nothing -> (es, x : seen)
          Just y@(Arg _ yt)  -> if sameUnderlyingType xt yt then (es, seen) else (typeMismatchErr x y : es, seen)
        toEither (e:es, _) = Left (e :| es)
        toEither _         = Right ()
        typeMismatchErr (Arg n xt) (Arg _ yt) =
          "Incompatible interpolation types for `" <> n <> "`. Found " <> friendlyInputType xt <> ", expected " <> friendlyInputType yt

eqName :: Arg -> Arg -> Bool
eqName (Arg x _) (Arg y _) = x == y

friendlyInputType :: Type -> Text
friendlyInputType String      = "string"
friendlyInputType Date {}     = "date"
friendlyInputType Number      = "number"
friendlyInputType Select {}   = "string"
friendlyInputType Plural {}   = "number"
friendlyInputType Callback {} = "tag"

sameUnderlyingType :: Type -> Type -> Bool
sameUnderlyingType = (==) `on` friendlyInputType
