module Intlc.Compiler (compileDataset, compileFlattened, flatten, expandPlurals, expandRules) where

import           Data.Foldable                     (elem)
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

-- Map every `Node`, included those nested. Order is unspecified. The children
-- of a node, if any, will be traversed after the provided function is applied.
mapNodes :: (ICU.Node -> ICU.Node) -> ICU.Node -> ICU.Node
mapNodes f = f >>> \case
  ICU.Fin -> ICU.Fin
  ICU.Char c x -> ICU.Char c (rec x)
  ICU.String n x -> ICU.String n (rec x)
  ICU.Number n x -> ICU.Number n (rec x)
  ICU.Date n x y -> ICU.Date n x (rec y)
  ICU.Time n x y -> ICU.Time n x (rec y)
  ICU.Bool n x y z -> ICU.Bool n (f x) (f y) (rec z)
  ICU.CardinalExact n xs y        -> ICU.CardinalExact n (mapPluralCase f <$> xs) (rec y)
  ICU.CardinalInexact n xs ys w z -> ICU.CardinalInexact n (mapPluralCase f <$> xs) (mapPluralCase f <$> ys) (f w) (rec z)
  ICU.Ordinal n xs ys w z         -> ICU.Ordinal n (mapPluralCase f <$> xs) (mapPluralCase f <$> ys) (f w) (rec z)
  ICU.PluralRef n x -> ICU.PluralRef n (rec x)
  ICU.SelectNamed n xs y       -> ICU.SelectNamed n (mapSelectCase f <$> xs) (rec y)
  ICU.SelectWild n w x         -> ICU.SelectWild n (f w) (rec x)
  ICU.SelectNamedWild n xs w y -> ICU.SelectNamedWild n (mapSelectCase f <$> xs) (f w) (rec y)
  ICU.Callback n x y -> ICU.Callback n (f x) (rec y)
  where rec = mapNodes f

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
expandPlurals (ICU.Message x) = ICU.Message . flip mapNodes x $ \case
  p@(ICU.CardinalExact {})               -> p
  ICU.CardinalInexact n exacts rules w y ->
    ICU.CardinalInexact n exacts (toList $ expandRules rules w) w y
  ICU.Ordinal n exacts rules w y         ->
    ICU.Ordinal n exacts (toList $ expandRules rules w) w y
  y -> y

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
