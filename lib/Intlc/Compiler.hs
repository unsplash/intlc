module Intlc.Compiler (compileDataset, compileFlattened, flatten, expandPlurals, expandRules) where

import           Data.Foldable                     (elem)
import           Data.Functor.Foldable             (cata, embed)
import           Data.List.Extra                   (unionBy)
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

compileFlattened :: Dataset Translation -> Text
compileFlattened = JSON.compileDataset . mapMsgs flatten

mapMsgs :: (ICU.Message -> ICU.Message) -> Dataset Translation -> Dataset Translation
mapMsgs f = fmap $ \x -> x { message = f (message x) }

flatten :: ICU.Message -> ICU.Message
flatten = ICU.Message . go mempty . ICU.unMessage
  where go :: ICU.Node -> ICU.Node -> ICU.Node
        go prev rest =
          let (curr, mnext) = ICU.sever rest
              next = fold mnext
              rec mid = go ICU.Fin (prev <> mid <> next)
           in case curr of
            ICU.Fin -> prev
            ICU.Bool n x y _ -> ICU.Bool n (rec x) (rec y) ICU.Fin
            ICU.CardinalExact n xs _        -> ICU.CardinalExact n (mapPluralCase rec <$> xs) ICU.Fin
            ICU.CardinalInexact n xs ys w _ -> ICU.CardinalInexact n (mapPluralCase rec <$> xs) (mapPluralCase rec <$> ys) (rec w) ICU.Fin
            ICU.Ordinal n xs ys w _         -> ICU.Ordinal n (mapPluralCase rec <$> xs) (mapPluralCase rec <$> ys) (rec w) ICU.Fin
            ICU.PluralRef n _ -> ICU.PluralRef n ICU.Fin
            ICU.SelectNamed n xs _       -> ICU.SelectNamed n (mapSelectCase rec <$> xs) ICU.Fin
            ICU.SelectWild n w _         -> ICU.SelectWild n (rec w) ICU.Fin
            ICU.SelectNamedWild n xs w _ -> ICU.SelectNamedWild n (mapSelectCase rec <$> xs) (rec w) ICU.Fin
            _ -> go (prev <> curr) next

-- Expands any plural with a rule to contain every rule. This makes ICU plural
-- syntax usable on platforms which don't support ICU; translators can reuse
-- copy across unneeded plural rules.
--
-- Added plural rules inherit the content of the wildcard. Output order of
-- rules is unspecified.
expandPlurals :: ICU.Message -> ICU.Message
expandPlurals = ICU.Message . cata f . ICU.unMessage
  where f (ICU.CardinalInexactF n exacts rules w y) =
            ICU.CardinalInexact n exacts (toList $ expandRules rules w) w y
        f (ICU.OrdinalF n exacts rules w y) =
            ICU.Ordinal n exacts (toList $ expandRules rules w) w y
        f y = embed y

expandRules :: (Functor f, Foldable f) => f (ICU.PluralCase ICU.PluralRule) -> ICU.Node -> NonEmpty (ICU.PluralCase ICU.PluralRule)
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

mapSelectCase :: (ICU.Node -> ICU.Node) -> ICU.SelectCase -> ICU.SelectCase
mapSelectCase = second

mapPluralCase :: (ICU.Node -> ICU.Node) -> ICU.PluralCase a -> ICU.PluralCase a
mapPluralCase = second
