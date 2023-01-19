module Intlc.Compiler (compileDataset, compileToJSON, flatten, expandPlurals, expandRules) where

import           Data.Foldable                     (elem)
import           Data.Functor.Foldable             (cata, embed, project)
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
compileDataset :: Locale -> Dataset (Translation (ICU.Message ICU.Node)) -> Either (NonEmpty Text) Text
compileDataset l d = validateKeys d $>
  case stmts of
    []     -> JS.emptyModule
    stmts' -> T.intercalate "\n" stmts'
  where stmts = imports <> exports
        imports = maybeToList $ JS.buildReactImport d
        exports = M.foldrWithKey buildCompiledTranslations mempty d
        buildCompiledTranslations k v acc = compileTranslation l k v : acc

validateKeys :: Dataset (Translation (ICU.Message ICU.Node)) -> Either (NonEmpty Text) ()
validateKeys = toEither . lefts . fmap (uncurry validate) . M.toList
  where toEither []     = Right ()
        toEither (e:es) = Left $ e :| es
        validate k t = k & case backend t of
          TypeScript      -> TS.validateKey
          TypeScriptReact -> TS.validateKey

compileTranslation :: Locale -> Text -> Translation (ICU.Message ICU.Node) -> Text
compileTranslation l k (Translation v be _) = case be of
  TypeScript      -> TS.compileNamedExport TemplateLit l k v
  TypeScriptReact -> TS.compileNamedExport JSX         l k v

compileToJSON :: (ICU.Node -> ICU.Node) -> JSON.Formatting -> Dataset (Translation (ICU.Message ICU.Node)) -> Text
compileToJSON f fmt = JSON.compileDataset fmt . mapMsgs (fmap f)

mapMsgs :: (ICU.Message ICU.Node -> ICU.Message ICU.Node) -> Dataset (Translation (ICU.Message ICU.Node)) -> Dataset (Translation (ICU.Message ICU.Node))
mapMsgs f = fmap $ \x -> x { message = f x.message }

flatten :: ICU.Node -> ICU.Node
flatten = go mempty
  where go :: ICU.Node -> ICU.Node -> ICU.Node
        go prev rest =
          let (curr, mnext) = ICU.sever rest
              next = fold mnext
              rec mid = go (embed ICU.Fin) (prev <> mid <> next)
           in case project curr of
            ICU.Fin                         -> prev
            ICU.Bool n x y _                -> ICU.Bool' n (rec x) (rec y)
            ICU.CardinalExact n xs _        -> ICU.CardinalExact' n (mapPluralCase rec <$> xs)
            ICU.CardinalInexact n xs ys w _ -> ICU.CardinalInexact' n (mapPluralCase rec <$> xs) (mapPluralCase rec <$> ys) (rec w)
            ICU.Ordinal n xs ys w _         -> ICU.Ordinal' n (mapPluralCase rec <$> xs) (mapPluralCase rec <$> ys) (rec w)
            ICU.SelectNamed n xs _          -> ICU.SelectNamed' n (mapSelectCase rec <$> xs)
            ICU.SelectWild n w _            -> ICU.SelectWild' n (rec w)
            ICU.SelectNamedWild n xs w _    -> ICU.SelectNamedWild' n (mapSelectCase rec <$> xs) (rec w)
            _                               -> go (prev <> curr) next

-- Expands any plural with a rule to contain every rule. This makes ICU plural
-- syntax usable on platforms which don't support ICU; translators can reuse
-- copy across unneeded plural rules.
--
-- Added plural rules inherit the content of the wildcard. Output order of
-- rules is unspecified.
expandPlurals :: ICU.Node -> ICU.Node
expandPlurals = cata (embed . f)
  where f (ICU.CardinalInexact n exacts rules w y) =
            ICU.CardinalInexact n exacts (toList $ expandRules rules w) w y
        f (ICU.Ordinal n exacts rules w y) =
            ICU.Ordinal n exacts (toList $ expandRules rules w) w y
        f y = y

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

mapSelectCase :: (a -> a) -> ICU.SelectCaseF a -> ICU.SelectCaseF a
mapSelectCase = second

mapPluralCase :: (b -> b) -> ICU.PluralCaseF a b -> ICU.PluralCaseF a b
mapPluralCase = second
