module Intlc.Compiler (compileDataset, compileFlattened, flatten, expandPlurals, expandRules) where

import           Control.Applicative.Combinators   (choice)
import           Data.Foldable                     (elem)
import           Data.List.Extra                   (firstJust, unionBy)
import qualified Data.Map                          as M
import qualified Data.Text                         as T
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
type ICUSelect = (NonEmpty ICU.SelectCase, Maybe ICU.SelectWildcard)

compileFlattened :: Dataset Translation -> Text
compileFlattened = JSON.compileDataset . mapMsgs flatten

mapMsgs :: (ICU.Message -> ICU.Message) -> Dataset Translation -> Dataset Translation
mapMsgs f = fmap $ \x -> x { message = f (message x) }

-- Map every token, included those nested, in a `Stream`. Order is unspecified.
-- The children of a token, if any, will be traversed after the provided
-- function is applied.
mapTokens :: (ICU.Token -> ICU.Token) -> ICU.Stream -> ICU.Stream
mapTokens f = fmap $ f >>> \case
  x@(ICU.Plaintext {})      -> x
  x@(ICU.Interpolation n t) -> case t of
    ICU.Bool xs ys   -> g $ ICU.Bool (h xs) (h ys)
    ICU.Plural y     -> g . ICU.Plural $ mapPluralStreams h y
    ICU.Select ys mz -> g . uncurry ICU.Select $ mapSelectStreams h (ys, mz)
    ICU.Callback ys  -> g . ICU.Callback . h $ ys
    _                -> x
    where g = ICU.Interpolation n
          h = fmap f

flatten :: ICU.Message -> ICU.Message
flatten (ICU.Message xs) = ICU.Message . flattenStream $ xs
  where flattenStream :: ICU.Stream -> ICU.Stream
        flattenStream ys = fromMaybe ys $ choice
          [ mapBool   <$> extractFirstBool ys
          , mapSelect <$> extractFirstSelect ys
          , mapPlural <$> extractFirstPlural ys
          ]
        mapBool (n, ls, boo, rs) = streamFromArg n . uncurry ICU.Bool $ mapBoolStreams (around ls rs) boo
        mapSelect (n, ls, sel, rs) = streamFromArg n . uncurry ICU.Select $ mapSelectStreams (around ls rs) sel
        mapPlural (n, ls, plu, rs) = streamFromArg n .         ICU.Plural $ mapPluralStreams (around ls rs) plu
        around ls rs = flattenStream . ICU.mergePlaintext . surround ls rs
        surround ls rs cs = ls <> cs <> rs
        streamFromArg n = pure . ICU.Interpolation n

-- Expands any plural with a rule to contain every rule. This makes ICU plural
-- syntax usable on platforms which don't support ICU; translators can reuse
-- copy across unneeded plural rules.
--
-- Added plural rules inherit the content of the wildcard. Output order of
-- rules is unspecified.
expandPlurals :: ICU.Message -> ICU.Message
expandPlurals (ICU.Message xs) = ICU.Message . flip mapTokens xs $ \case
  ICU.Interpolation n (ICU.Plural p) -> ICU.Interpolation n . ICU.Plural $ case p of
    ICU.Cardinal (ICU.LitPlural {})                -> p
    ICU.Cardinal (ICU.RulePlural rules w)          ->
      ICU.Cardinal $ ICU.RulePlural (expandRules rules w) w
    ICU.Cardinal (ICU.MixedPlural exacts rules w)  ->
      ICU.Cardinal $ ICU.MixedPlural exacts (expandRules rules w) w
    ICU.Ordinal (ICU.OrdinalPlural exacts rules w) ->
      ICU.Ordinal $ ICU.OrdinalPlural exacts (expandRules rules w) w
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

extractFirstBool :: ICU.Stream -> Maybe (Text, ICU.Stream, ICUBool, ICU.Stream)
extractFirstBool = extractFirstArg $ \case
  ICU.Bool x y -> Just (x, y)
  _            -> Nothing

extractFirstArg :: (ICU.Type -> Maybe a) -> ICU.Stream -> Maybe (Text, ICU.Stream, a, ICU.Stream)
extractFirstArg f xs = firstJust arg (zip [0..] xs)
  where arg (i, ICU.Interpolation n t) = (n, ls, , rs) <$> f t
          where (ls, _:rs) = splitAt i xs
        arg _ = Nothing

extractFirstSelect :: ICU.Stream -> Maybe (Text, ICU.Stream, ICUSelect, ICU.Stream)
extractFirstSelect = extractFirstArg $ \case
  ICU.Select xs y -> Just (xs, y)
  _               -> Nothing

extractFirstPlural :: ICU.Stream -> Maybe (Text, ICU.Stream, ICU.Plural, ICU.Stream)
extractFirstPlural = extractFirstArg $ \case
  ICU.Plural x -> Just x
  _            -> Nothing

mapBoolStreams :: (ICU.Stream -> ICU.Stream) -> ICUBool -> ICUBool
mapBoolStreams f (xs, ys) = (f xs, f ys)

mapSelectStreams :: (ICU.Stream -> ICU.Stream) -> ICUSelect -> ICUSelect
mapSelectStreams f (xs, mw) = (mapSelectCase f <$> xs, mapSelectWildcard f <$> mw)

mapSelectCase :: (ICU.Stream -> ICU.Stream) -> ICU.SelectCase -> ICU.SelectCase
mapSelectCase f (ICU.SelectCase x ys) = ICU.SelectCase x (f ys)

mapSelectWildcard :: (ICU.Stream -> ICU.Stream) -> ICU.SelectWildcard -> ICU.SelectWildcard
mapSelectWildcard f (ICU.SelectWildcard xs) = ICU.SelectWildcard (f xs)

mapPluralStreams :: (ICU.Stream -> ICU.Stream) -> ICU.Plural -> ICU.Plural
mapPluralStreams f (ICU.Cardinal (ICU.LitPlural xs mw))      = ICU.Cardinal $ ICU.LitPlural (mapPluralCase f <$> xs) (mapPluralWildcard f <$> mw)
mapPluralStreams f (ICU.Cardinal (ICU.RulePlural xs w))      = ICU.Cardinal $ ICU.RulePlural (mapPluralCase f <$> xs) (mapPluralWildcard f w)
mapPluralStreams f (ICU.Cardinal (ICU.MixedPlural xs ys w))  = ICU.Cardinal $ ICU.MixedPlural (mapPluralCase f <$> xs) (mapPluralCase f <$> ys) (mapPluralWildcard f w)
mapPluralStreams f (ICU.Ordinal (ICU.OrdinalPlural xs ys w)) = ICU.Ordinal $ ICU.OrdinalPlural (mapPluralCase f <$> xs) (mapPluralCase f <$> ys) (mapPluralWildcard f w)

mapPluralCase :: (ICU.Stream -> ICU.Stream) -> ICU.PluralCase a -> ICU.PluralCase a
mapPluralCase f (ICU.PluralCase x ys) = ICU.PluralCase x (f ys)

mapPluralWildcard :: (ICU.Stream -> ICU.Stream) -> ICU.PluralWildcard -> ICU.PluralWildcard
mapPluralWildcard f (ICU.PluralWildcard xs) = ICU.PluralWildcard (f xs)
