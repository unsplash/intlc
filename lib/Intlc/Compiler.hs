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
type ICUSelect = These (NonEmpty ICU.SelectCase) ICU.Stream

compileFlattened :: Dataset Translation -> Text
compileFlattened = JSON.compileDataset . mapMsgs flatten

mapMsgs :: (ICU.Message -> ICU.Message) -> Dataset Translation -> Dataset Translation
mapMsgs f = fmap $ \x -> x { message = f (message x) }

-- Map every `Node`, included those nested, in a `Stream`. Order is
-- unspecified. The children of a node, if any, will be traversed after the
-- provided function is applied.
mapNodes :: (ICU.Node -> ICU.Node) -> ICU.Stream -> ICU.Stream
mapNodes f = fmap $ f >>> \case
  ICU.Bool n xs ys  -> ICU.Bool n (f <$> xs) (f <$> ys)
  ICU.CardinalExact n xs        -> ICU.CardinalExact n (mapPluralCase (fmap f) <$> xs)
  ICU.CardinalInexact n xs ys w -> ICU.CardinalInexact n (mapPluralCase (fmap f) <$> xs) (mapPluralCase (fmap f) <$> ys) (fmap f w)
  ICU.Ordinal n xs ys w         -> ICU.Ordinal n (mapPluralCase (fmap f) <$> xs) (mapPluralCase (fmap f) <$> ys) (fmap f w)
  ICU.Select n x    -> ICU.Select n $ mapSelectStreams (fmap f) x
  ICU.Callback n xs -> ICU.Callback n (f <$> xs)
  x                 -> x

flatten :: ICU.Message -> ICU.Message
flatten = ICU.Message . go [] . ICU.unMessage
  where go :: ICU.Stream -> ICU.Stream -> ICU.Stream
        go prev []                        = prev
        go prev (ICU.Bool n xs ys : next) = pure . uncurry (ICU.Bool n) $ mapBoolStreams   (around prev next) (xs, ys)
        go prev (ICU.Select n x : next)   = pure .         ICU.Select n $ mapSelectStreams (around prev next) x
        go prev (ICU.CardinalExact n xs : next)        = pure $ ICU.CardinalExact n (mapPluralCase (around prev next) <$> xs)
        go prev (ICU.CardinalInexact n xs ys w : next) = pure $ ICU.CardinalInexact n (mapPluralCase (around prev next) <$> xs) (mapPluralCase (around prev next) <$> ys) (around prev next w)
        go prev (ICU.Ordinal n xs ys w : next)         = pure $ ICU.Ordinal n (mapPluralCase (around prev next) <$> xs) (mapPluralCase (around prev next) <$> ys) (around prev next w)
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
expandPlurals (ICU.Message xs) = ICU.Message . flip mapNodes xs $ \case
  p@(ICU.CardinalExact _ _    )        -> p
  ICU.CardinalInexact n exacts rules w ->
    ICU.CardinalInexact n exacts (toList $ expandRules rules w) w
  ICU.Ordinal n exacts rules w         ->
    ICU.Ordinal n exacts (toList $ expandRules rules w) w
  x -> x

expandRules :: (Functor f, Foldable f) => f (ICU.PluralCase ICU.PluralRule) -> ICU.Stream -> NonEmpty (ICU.PluralCase ICU.PluralRule)
-- `fromList` is a cheap way to promise the compiler that we'll return a
-- non-empty list. This is logically guaranteed by one of the inputs to
-- `unionBy` being non-empty, namely `extraCases` - though given the complexity
-- this is unit tested for confidence.
expandRules ys w = fromList $ unionBy ((==) `on` caseRule) (toList ys) extraCases
  where extraCases = (, w) <$> missingRules
        missingRules = filter (not . flip elem presentRules) allRules
        presentRules = caseRule <$> ys
        allRules = universe
        caseRule (x, _) = x

mapBoolStreams :: (ICU.Stream -> ICU.Stream) -> ICUBool -> ICUBool
mapBoolStreams f (xs, ys) = (f xs, f ys)

mapSelectStreams :: (ICU.Stream -> ICU.Stream) -> ICUSelect -> ICUSelect
mapSelectStreams f = bimap (fmap (mapSelectCase f)) f

mapSelectCase :: (ICU.Stream -> ICU.Stream) -> ICU.SelectCase -> ICU.SelectCase
mapSelectCase f (x, ys) = (x, f ys)

mapPluralCase :: (ICU.Stream -> ICU.Stream) -> ICU.PluralCase a -> ICU.PluralCase a
mapPluralCase f (x, ys) = (x, f ys)
