module Intlc.EndToEndSpec (spec) where

import qualified Data.Text                  as T
import           Intlc.Backend.ICU.Compiler (Formatting (SingleLine),
                                             compileMsg)
import           Intlc.Compiler             (compileDataset, expandPlurals)
import           Intlc.Core                 (Locale (Locale), datasetSansAnn)
import           Intlc.ICU                  (sansAnn)
import           Intlc.Parser               (parseDataset)
import           Intlc.Parser.Error         (ParseFailure)
import           Intlc.Parser.ICU           (ParserState (endOfInput), annMsg,
                                             emptyState)
import           Prelude
import           System.FilePath            ((<.>), (</>))
import           Test.Hspec
import           Test.Hspec.Golden          (Golden (..), defaultGolden)
import           Text.Megaparsec            (eof, runParser)
import           Text.RawString.QQ          (r)

parseAndCompileDataset :: Text -> Either (NonEmpty Text) Text
parseAndCompileDataset = compileDataset (Locale "en-US") . datasetSansAnn <=< first (pure . show) . parseDataset "test"

parseAndExpandMsg :: Text -> Either ParseFailure Text
parseAndExpandMsg = fmap (compileMsg SingleLine . fmap (expandPlurals . sansAnn)) . parseMsg
  where parseMsg = runParser (runReaderT annMsg (emptyState { endOfInput = eof })) "test"

golden :: String -> Text -> Golden String
golden name in' = baseCfg
  { goldenFile = goldenFile baseCfg <.> "ts"
  , actualFile = actualFile baseCfg <&> (<.> "ts")
  }
  where baseCfg = defaultGolden fileName out
        fileName = "e2e" </> name
        out = T.unpack . fromErrs . parseAndCompileDataset $ in'
        fromErrs (Right x) = x
        fromErrs (Left es) = T.intercalate "\n" . toList $ es

spec :: Spec
spec = describe "end-to-end" $ do
  describe "compilation" $ do
    let x =*= y = parseAndCompileDataset x `shouldBe` Right y
    let withReactImport = ("import { ReactElement } from 'react'\n" <>)

    it "compiles to golden output" $ do
      golden "example" [r|{ "title": { "message": "Unsplash" }, "greeting": { "message": "Hello <bold>{name}</bold>, {age, number}!", "backend": "ts" } }|]

    it "compiles valid JS module format given empty input" $ do
      [r|{}|]
        =*= "export {}"

    it "parses and discards descriptions" $ do
      [r|{ "brand": { "message": "Unsplash", "description": "The company name" } }|]
        =*= "export const brand: () => string = () => `Unsplash`"

    it "outputs in alphabetical order" $ do
      [r|{ "x": { "message": "" }, "A": { "message": "" }, "z": { "message": "" } }|]
        =*= "export const A: () => string = () => ``\nexport const x: () => string = () => ``\nexport const z: () => string = () => ``"

    it "compiles bools" $ do
      [r|{ "f": { "message": "{x, boolean, true {y} false {z}}" } }|]
        =*= "export const f: (x: { x: boolean }) => string = x => `${(() => { switch (x.x as typeof x.x) { case true: return `y`; case false: return `z`; } })()}`"

    it "compiles plurals" $ do
      [r|{ "prop": { "message": "Age: {age, plural, =0 {newborn called {name}} =42 {magical} other {boring #}}", "backend": "ts" } }|]
        =*= "export const prop: (x: { age: number; name: string }) => string = x => `Age: ${(() => { switch (x.age as typeof x.age) { case 0: return `newborn called ${x.name}`; case 42: return `magical`; default: return `boring ${new Intl.NumberFormat('en-US').format(x.age)}`; } })()}`"
      [r|{ "prop": { "message": "Age: {age, plural, =0 {newborn called {name}} =42 {magical} other {boring #}}", "backend": "tsx" } }|]
        =*= withReactImport "export const prop: (x: { age: number; name: string }) => ReactElement = x => <>Age: {(() => { switch (x.age as typeof x.age) { case 0: return <>newborn called {x.name}</>; case 42: return <>magical</>; default: return <>boring {new Intl.NumberFormat('en-US').format(x.age)}</>; } })()}</>"
      [r|{ "f": { "message": "{n, plural, =0 {x} =42 {y}}", "backend": "ts" } }|]
        =*= "export const f: (x: { n: 0 | 42 }) => string = x => `${(() => { switch (x.n as typeof x.n) { case 0: return `x`; case 42: return `y`; } })()}`"
      [r|{ "f": { "message": "{n, plural, =0 {zero} many {many} other {#}}", "backend": "ts" } }|]
        =*= "export const f: (x: { n: number }) => string = x => `${(() => { switch (x.n as typeof x.n) { case 0: return `zero`; default: { switch (new Intl.PluralRules('en-US').select(x.n)) { case 'many': return `many`; default: return `${new Intl.NumberFormat('en-US').format(x.n)}`; } } } })()}`"
      [r|{ "f": { "message": "{n, plural, many {many} other {#}}", "backend": "ts" } }|]
        =*= "export const f: (x: { n: number }) => string = x => `${(() => { switch (new Intl.PluralRules('en-US').select(x.n)) { case 'many': return `many`; default: return `${new Intl.NumberFormat('en-US').format(x.n)}`; } })()}`"
      [r|{ "f": { "message": "{n, plural, =42 {#}}" } }|]
        =*= "export const f: (x: { n: 42 }) => string = x => `${(() => { switch (x.n as typeof x.n) { case 42: return `${new Intl.NumberFormat('en-US').format(x.n)}`; } })()}`"

    it "compiles select" $ do
      [r|{ "f": { "message": "{x, select, a {hi} b {yo}}", "backend": "ts" } }|]
        =*= "export const f: (x: { x: 'a' | 'b' }) => string = x => `${(() => { switch (x.x as typeof x.x) { case 'a': return `hi`; case 'b': return `yo`; } })()}`"
      [r|{ "f": { "message": "{x, select, a {hi} b {yo} other {ciao}}", "backend": "ts" } }|]
        =*= "export const f: (x: { x: string }) => string = x => `${(() => { switch (x.x as typeof x.x) { case 'a': return `hi`; case 'b': return `yo`; default: return `ciao`; } })()}`"

    it "compiles selectordinal" $ do
      [r|{ "f": { "message": "{x, selectordinal, one {foo} other {bar}}", "backend": "ts" } }|]
        =*= "export const f: (x: { x: number }) => string = x => `${(() => { switch (new Intl.PluralRules('en-US', { type: 'ordinal' }).select(x.x)) { case 'one': return `foo`; default: return `bar`; } })()}`"
      [r|{ "f": { "message": "{x, selectordinal, one {foo} =2 {bar} other {baz}}", "backend": "ts" } }|]
        =*= "export const f: (x: { x: number }) => string = x => `${(() => { switch (x.x as typeof x.x) { case 2: return `bar`; default: { switch (new Intl.PluralRules('en-US', { type: 'ordinal' }).select(x.x)) { case 'one': return `foo`; default: return `baz`; } } } })()}`"

    it "TypeScript backend" $ do
      [r|{ "f": { "message": "{x} <z>{y, number}</z> {y, number}", "backend": "ts" } }|]
        =*= "export const f: (x: { x: string; y: number; z: (x: string) => string }) => string = x => `${x.x} ${x.z(`${new Intl.NumberFormat('en-US').format(x.y)}`)} ${new Intl.NumberFormat('en-US').format(x.y)}`"

    it "TypeScriptReact backend" $ do
      [r|{ "f": { "message": "{x} <z>{y, number}</z>", "backend": "tsx" } }|]
        =*= withReactImport "export const f: (x: { x: string; y: number; z: (x: ReactElement) => ReactElement }) => ReactElement = x => <>{x.x} {x.z(<>{new Intl.NumberFormat('en-US').format(x.y)}</>)}</>"

  describe "plural expansion" $ do
    let x =*= y = parseAndExpandMsg x `shouldBe` Right y

    -- For the utmost confidence this was written by hand. Have run reading it!
    it "expands nested rule plurals" $ do
      "a {na, plural, zero {b} other {{nb, plural, two {c} many {d} other {e}}}} {f, number} {nc, plural, one {g} other {{h}}} i {nd, plural, =1 {j} one {k} other {l}}"
        =*= "a {na, plural, zero {b} one {{nb, plural, two {c} many {d} zero {e} one {e} few {e} other {e}}} two {{nb, plural, two {c} many {d} zero {e} one {e} few {e} other {e}}} few {{nb, plural, two {c} many {d} zero {e} one {e} few {e} other {e}}} many {{nb, plural, two {c} many {d} zero {e} one {e} few {e} other {e}}} other {{nb, plural, two {c} many {d} zero {e} one {e} few {e} other {e}}}} {f, number} {nc, plural, one {g} zero {{h}} two {{h}} few {{h}} many {{h}} other {{h}}} i {nd, plural, =1 {j} one {k} zero {l} two {l} few {l} many {l} other {l}}"
