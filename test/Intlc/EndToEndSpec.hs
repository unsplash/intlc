module Intlc.EndToEndSpec (spec) where

import           Data.ByteString.Lazy (ByteString)
import qualified Data.Text            as T
import           Intlc.Compiler       (compileDataset)
import           Intlc.Core           (Locale (Locale))
import           Intlc.Parser         (ParseFailure (..), parseDataset)
import           Prelude              hiding (ByteString)
import           System.FilePath      ((<.>), (</>))
import           Test.Hspec
import           Test.Hspec.Golden    (Golden (..), defaultGolden)
import           Text.RawString.QQ    (r)

parseAndCompileDataset :: ByteString -> Either (NonEmpty Text) Text
parseAndCompileDataset = fmap (compileDataset (Locale "en-US")) . first (pure . show) . parseDataset

golden :: String -> ByteString -> Golden String
golden name in' = baseCfg
  { goldenFile = goldenFile baseCfg <.> "ts"
  , actualFile = actualFile baseCfg <&> (<.> "ts")
  }
  where baseCfg = defaultGolden fileName out
        fileName = "e2e" </> name
        out = T.unpack . fromErrs . parseAndCompileDataset $ in'
        fromErrs (Right x) = x
        fromErrs (Left es) = T.intercalate "\n" . toList $ es

(=*=) :: ByteString -> Text -> IO ()
x =*= y = parseAndCompileDataset x `shouldBe` Right y

withReactImport :: Text -> Text
withReactImport = ("import React, { ReactElement } from 'react'\n" <>)

spec :: Spec
spec = describe "end-to-end" $ do
  it "example message" $ do
    golden "example" [r|{ "title": { "message": "Unsplash" }, "greeting": { "message": "Hello <bold>{name}</bold>, {age, number}!", "backend": "ts" } }|]

  it "validates that keys are alphanumeric" $ do
    parseDataset [r|{ "x bad key": { "message": "foo" }, "good_Key": { "message": "bar" }, "y-bad-key": { "message": "baz" }, "z_bad_key123": { "message": "biz" } }|] `shouldBe`
      Left (InvalidKeys $ "x bad key" :| ["y-bad-key", "z_bad_key123"])

  it "parses and discards descriptions" $ do
    [r|{ "brand": { "message": "Unsplash", "description": "The company name" } }|]
      =*= "export const brand: () => string = () => 'Unsplash'"

  it "outputs in alphabetical order" $ do
    [r|{ "x": { "message": "" }, "A": { "message": "" }, "z": { "message": "" } }|]
      =*= "export const A: () => string = () => ''\nexport const x: () => string = () => ''\nexport const z: () => string = () => ''"

  it "compiles plurals" $ do
    [r|{ "prop": { "message": "Age: {age, plural, =0 {newborn called {name}} =42 {magical} other {boring #}}", "backend": "ts" } }|]
      =*= "export const prop: (x: { age: number; name: string }) => string = x => `Age: ${(() => { switch (x.age) { case 0: return `newborn called ${x.name}`; case 42: return `magical`; default: return `boring ${new Intl.NumberFormat('en-US').format(x.age)}`; } })()}`"
    [r|{ "prop": { "message": "Age: {age, plural, =0 {newborn called {name}} =42 {magical} other {boring #}}", "backend": "tsx" } }|]
      =*= withReactImport "export const prop: (x: { age: number; name: string }) => ReactElement = x => <>Age: {(() => { switch (x.age) { case 0: return <>newborn called {x.name}</>; case 42: return <>magical</>; default: return <>boring {new Intl.NumberFormat('en-US').format(x.age)}</>; } })()}</>"
    [r|{ "f": { "message": "{n, plural, =0 {x} =42 {y}}", "backend": "ts" } }|]
      =*= "export const f: (x: { n: 0 | 42 }) => string = x => `${(() => { switch (x.n) { case 0: return `x`; case 42: return `y`; } })()}`"
    [r|{ "f": { "message": "{n, plural, =0 {zero} many {many} other {#}}", "backend": "ts" } }|]
      =*= "export const f: (x: { n: number }) => string = x => `${(() => { switch (x.n) { case 0: return `zero`; default: { switch (new Intl.PluralRules('en-US').select(x.n)) { case 'many': return `many`; default: return `${new Intl.NumberFormat('en-US').format(x.n)}`; } } } })()}`"
    [r|{ "f": { "message": "{n, plural, many {many} other {#}}", "backend": "ts" } }|]
      =*= "export const f: (x: { n: number }) => string = x => `${(() => { switch (new Intl.PluralRules('en-US').select(x.n)) { case 'many': return `many`; default: return `${new Intl.NumberFormat('en-US').format(x.n)}`; } })()}`"

  it "compiles select" $ do
    [r|{ "f": { "message": "{x, select, a {hi} b {yo}}", "backend": "ts" } }|]
      =*= "export const f: (x: { x: 'a' | 'b' }) => string = x => `${(() => { switch (x.x) { case 'a': return `hi`; case 'b': return `yo`; } })()}`"
    [r|{ "f": { "message": "{x, select, a {hi} b {yo} other {ciao}}", "backend": "ts" } }|]
      =*= "export const f: (x: { x: string }) => string = x => `${(() => { switch (x.x) { case 'a': return `hi`; case 'b': return `yo`; default: return `ciao`; } })()}`"

  it "compiles selectordinal" $ do
    [r|{ "f": { "message": "{x, selectordinal, one {foo} other {bar}}", "backend": "ts" } }|]
      =*= "export const f: (x: { x: number }) => string = x => `${(() => { switch (new Intl.PluralRules('en-US', { type: 'ordinal' }).select(x.x)) { case 'one': return `foo`; default: return `bar`; } })()}`"
    [r|{ "f": { "message": "{x, selectordinal, one {foo} =2 {bar} other {baz}}", "backend": "ts" } }|]
      =*= "export const f: (x: { x: number }) => string = x => `${(() => { switch (x.x) { case 2: return `bar`; default: { switch (new Intl.PluralRules('en-US', { type: 'ordinal' }).select(x.x)) { case 'one': return `foo`; default: return `baz`; } } } })()}`"

  it "TypeScript backend" $ do
    [r|{ "f": { "message": "{x} <z>{y, number}</z> {y, number}", "backend": "ts" } }|]
      =*= "export const f: (x: { x: string; y: number; z: (x: string) => string }) => string = x => `${x.x} ${x.z(`${new Intl.NumberFormat('en-US').format(x.y)}`)} ${new Intl.NumberFormat('en-US').format(x.y)}`"

  it "TypeScriptReact backend" $ do
    [r|{ "f": { "message": "{x} <z>{y, number}</z>", "backend": "tsx" } }|]
      =*= withReactImport "export const f: (x: { x: string; y: number; z: (x: ReactElement) => ReactElement }) => ReactElement = x => <>{x.x} {x.z(<>{new Intl.NumberFormat('en-US').format(x.y)}</>)}</>"
