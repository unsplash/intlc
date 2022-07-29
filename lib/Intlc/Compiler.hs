module Intlc.Compiler (compileDataset, compileFlattened, flatten, expandPlurals, expandRules) where

import           Data.Foldable                     (elem)
import           Data.List.Extra                   (unionBy)
import qualified Data.Map                          as M
import qualified Data.Text                         as T
import           Data.These                        (These (..))
import           Intlc.Backend.JavaScript.Compiler as JS
import qualified Intlc.Backend.JSON.Compiler       as JSON
import qualified Intlc.Backend.TypeScript.Compiler as TS
import           Intlc.Core
import qualified Intlc.ICU                         as ICU
import           Prelude                           hiding (elem)

-- We'll `foldr` with `mempty`, avoiding `mconcat`, to preserve insertion order.
compileDataset :: Locale -> Dataset Translation -> Either (NonEmpty Text) Text
compileDataset l d = validateKeys d $>
  case stmts of
    []     -> JS.emptyModule
    stmts' -> T.intercalate "\n" stmts'
  where stmts = imports <> exports
        imports = maybeToList $ JS.buildReactImport d
        exports = M.foldrWithKey buildCompiledTranslations mempty d
        buildCompiledTranslations k v acc = compileTranslation l k v : acc

validateKeys :: Dataset Translation -> Either (NonEmpty Text) ()
validateKeys = toEither . lefts . fmap (uncurry validate) . M.toList
  where toEither []     = Right ()
        toEither (e:es) = Left $ e :| es
        validate k t = k & case backend t of
          TypeScript      -> TS.validateKey
          TypeScriptReact -> TS.validateKey

compileTranslation :: Locale -> Text -> Translation -> Text
compileTranslation l k (Translation v be _) = case be of
  TypeScript      -> TS.compileNamedExport TemplateLit l k v
  TypeScriptReact -> TS.compileNamedExport JSX         l k v

type ICUBool = (ICU.Stream, ICU.Stream)
type ICUSelect = These (NonEmpty ICU.SelectCase) ICU.SelectWildcard

compileFlattened :: Dataset Translation -> Text
compileFlattened = JSON.compileDataset . mapMsgs flatten

mapMsgs :: (ICU.Message -> ICU.Message) -> Dataset Translation -> Dataset Translation
mapMsgs f = fmap $ \x -> x { message = f (message x) }

-- Map every token, included those nested, in a `Stream`. Order is unspecified.
-- The children of a token, if any, will be traversed after the provided
-- function is applied.
mapTokens :: (ICU.Token -> ICU.Token) -> ICU.Stream -> ICU.Stream
mapTokens f = fmap $ f >>> \case
  ICU.Bool n xs ys  -> ICU.Bool n (f <$> xs) (f <$> ys)
  ICU.Plural n y    -> ICU.Plural n $ mapPluralStreams (fmap f) y
  ICU.Select n y    -> ICU.Select n $ mapSelectStreams (fmap f) y
  ICU.Callback n ys -> ICU.Callback n (f <$> ys)
  x                 -> x

flatten :: ICU.Message -> ICU.Message
flatten = ICU.Message . go [] . ICU.unMessage
  where go :: ICU.Stream -> ICU.Stream -> ICU.Stream
        go prev []                        = prev
        go prev (ICU.Bool n xs ys : next) = pure . uncurry (ICU.Bool n) $ mapBoolStreams   (around prev next) (xs, ys)
        go prev (ICU.Select n x : next)   = pure .         ICU.Select n $ mapSelectStreams (around prev next) x
        go prev (ICU.Plural n x : next)   = pure .         ICU.Plural n $ mapPluralStreams (around prev next) x
        go prev (curr : next)             = go (prev <> pure curr) next
        around ls rs = go [] . ICU.mergePlaintext . surround ls rs
        surround ls rs cs = ls <> cs <> rs

-- Expands any plural with a rule to contain every rule. This makes ICU plural
-- syntax usable on platforms which don't support ICU; translators can reuse
-- copy across unneeded plural rules.
--
-- Added plural rules inherit the content of the wildcard. Output order of
-- rules is unspecified.
expandPlurals :: ICU.Message -> ICU.Message
expandPlurals (ICU.Message xs) = ICU.Message . flip mapTokens xs $ \case
  ICU.Plural n p -> ICU.Plural n $ case p of
    ICU.CardinalExact {}               -> p
    ICU.CardinalInexact exacts rules w ->
      ICU.CardinalInexact exacts (toList $ expandRules rules w) w
    ICU.Ordinal exacts rules w         ->
      ICU.Ordinal exacts (toList $ expandRules rules w) w
  x -> x

expandRules :: (Functor f, Foldable f) => f (ICU.PluralCase ICU.PluralRule) -> ICU.PluralWildcard -> NonEmpty (ICU.PluralCase ICU.PluralRule)
-- `fromList` is a cheap way to promise the compiler that we'll return a
-- non-empty list. This is logically guaranteed by one of the inputs to
-- `unionBy` being non-empty, namely `extraCases` - though given the complexity
-- this is unit tested for confidence.
expandRules ys w = fromList $ unionBy ((==) `on` caseRule) (toList ys) extraCases
  where extraCases = flip ICU.PluralCase (wildContent w) <$> missingRules
        missingRules = filter (not . flip elem presentRules) allRules
        presentRules = caseRule <$> ys
        allRules = universe
        caseRule (ICU.PluralCase x _) = x
        wildContent (ICU.PluralWildcard x) = x

mapBoolStreams :: (ICU.Stream -> ICU.Stream) -> ICUBool -> ICUBool
mapBoolStreams f (xs, ys) = (f xs, f ys)

mapSelectStreams :: (ICU.Stream -> ICU.Stream) -> ICUSelect -> ICUSelect
mapSelectStreams f = bimap (fmap (mapSelectCase f)) (mapSelectWildcard f)

mapSelectCase :: (ICU.Stream -> ICU.Stream) -> ICU.SelectCase -> ICU.SelectCase
mapSelectCase f (ICU.SelectCase x ys) = ICU.SelectCase x (f ys)

mapSelectWildcard :: (ICU.Stream -> ICU.Stream) -> ICU.SelectWildcard -> ICU.SelectWildcard
mapSelectWildcard f (ICU.SelectWildcard xs) = ICU.SelectWildcard (f xs)

mapPluralStreams :: (ICU.Stream -> ICU.Stream) -> ICU.Plural -> ICU.Plural
mapPluralStreams f (ICU.CardinalExact xs)        =
  ICU.CardinalExact (mapPluralCase f <$> xs)
mapPluralStreams f (ICU.CardinalInexact xs ys w) =
  ICU.CardinalInexact (mapPluralCase f <$> xs) (mapPluralCase f <$> ys) (mapPluralWildcard f w)
mapPluralStreams f (ICU.Ordinal xs ys w)         =
  ICU.Ordinal (mapPluralCase f <$> xs) (mapPluralCase f <$> ys) (mapPluralWildcard f w)

mapPluralCase :: (ICU.Stream -> ICU.Stream) -> ICU.PluralCase a -> ICU.PluralCase a
mapPluralCase f (ICU.PluralCase x ys) = ICU.PluralCase x (f ys)

mapPluralWildcard :: (ICU.Stream -> ICU.Stream) -> ICU.PluralWildcard -> ICU.PluralWildcard
mapPluralWildcard f (ICU.PluralWildcard xs) = ICU.PluralWildcard (f xs)
