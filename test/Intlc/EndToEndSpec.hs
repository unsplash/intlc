module Intlc.EndToEndSpec (spec) where

import           Data.ByteString.Lazy (ByteString)
import           Intlc.Compiler       (compileDataset)
import           Intlc.Core           (Locale (Locale))
import           Intlc.Parser         (parseDataset)
import           Prelude              hiding (ByteString)
import           Test.Hspec
import           Text.RawString.QQ    (r)

(=*=) :: ByteString -> Text -> IO ()
x =*= y = f x `shouldBe` Right y
  where f = compileDataset (Locale "en-US") <=< first (pure . show) . parseDataset

withReactImport :: Text -> Text
withReactImport = ("import React, { ReactElement } from 'React'\n" <>)

spec :: Spec
spec = describe "end-to-end" $ do
  it "example message" $ do
    [r|{ "title": { "message": "Unsplash" }, "greeting": { "message": "Hello <bold>{name}</bold>, {age, number}!", "backend": "ts" } }|]
      =*= withReactImport "export const greeting: (x: { bold: (x: string) => string; name: string; age: number }) => string = x => `Hello ${x.bold(`${x.name}`)}, ${new Intl.NumberFormat('en-US').format(x.age)}!`\nexport const title: string = 'Unsplash'"

  it "parses and discards descriptions" $ do
    [r|{ "brand": { "message": "Unsplash", "description": "The company name" } }|]
      =*= withReactImport "export const brand: string = 'Unsplash'"

  it "outputs in alphabetical order" $ do
    [r|{ "x": { "message": "" }, "A": { "message": "" }, "z": { "message": "" } }|]
      =*= withReactImport "export const A: string = ''\nexport const x: string = ''\nexport const z: string = ''"

  it "compiles plurals" $ do
    [r|{ "prop": { "message": "Age: {age, plural, =0 {newborn called {name}} =42 {magical} other {boring #}}", "backend": "ts" } }|]
      =*= withReactImport "export const prop: (x: { age: number; name: string }) => string = x => `Age: ${(() => { switch (x.age) { case 0: return `newborn called ${x.name}`; case 42: return `magical`; default: return `boring ${new Intl.NumberFormat('en-US').format(x.age)}`; } })()}`"
    [r|{ "prop": { "message": "Age: {age, plural, =0 {newborn called {name}} =42 {magical} other {boring #}}", "backend": "tsx" } }|]
      =*= withReactImport "export const prop: (x: { age: number; name: string }) => React.ReactElement = x => <>Age: {(() => { switch (x.age) { case 0: return <>newborn called {x.name}</>; case 42: return <>magical</>; default: return <>boring {new Intl.NumberFormat('en-US').format(x.age)}</>; } })()}</>"
    [r|{ "f": { "message": "{n, plural, =0 {x} =42 {y}}", "backend": "ts" } }|]
      =*= withReactImport "export const f: (x: { n: 0 | 42 }) => string = x => `${(() => { switch (x.n) { case 0: return `x`; case 42: return `y`; } })()}`"
    [r|{ "f": { "message": "{n, plural, =0 {zero} many {many} other {#}}", "backend": "ts" } }|]
      =*= withReactImport "export const f: (x: { n: number }) => string = x => `${(() => { switch (x.n) { case 0: return `zero`; default: { switch (new Intl.PluralRules('en-US').select(x.n)) { case 'many': return `many`; default: return `${new Intl.NumberFormat('en-US').format(x.n)}`; } } } })()}`"
    [r|{ "f": { "message": "{n, plural, many {many} other {#}}", "backend": "ts" } }|]
      =*= withReactImport "export const f: (x: { n: number }) => string = x => `${(() => { switch (new Intl.PluralRules('en-US').select(x.n)) { case 'many': return `many`; default: return `${new Intl.NumberFormat('en-US').format(x.n)}`; } })()}`"

  it "compiles select" $ do
    [r|{ "f": { "message": "{x, select, a {hi} b {yo}}", "backend": "ts" } }|]
      =*= withReactImport "export const f: (x: { x: 'a' | 'b' }) => string = x => `${(() => { switch (x.x) { case 'a': return `hi`; case 'b': return `yo`; } })()}`"
    [r|{ "f": { "message": "{x, select, a {hi} b {yo} other {ciao}}", "backend": "ts" } }|]
      =*= withReactImport "export const f: (x: { x: string }) => string = x => `${(() => { switch (x.x) { case 'a': return `hi`; case 'b': return `yo`; default: return `ciao`; } })()}`"

  it "compiles selectordinal" $ do
    [r|{ "f": { "message": "{x, selectordinal, one {foo} other {bar}}", "backend": "ts" } }|]
      =*= "export const f: (x: { x: number }) => string = x => `${(() => { switch (new Intl.PluralRules('en-US', { type: 'ordinal' }).select(x.x)) { case 'one': return `foo`; default: return `bar`; } })()}`"
    [r|{ "f": { "message": "{x, selectordinal, one {foo} =2 {bar} other {baz}}", "backend": "ts" } }|]
      =*= "export const f: (x: { x: number }) => string = x => `${(() => { switch (x.x) { case 2: return `bar`; default: { switch (new Intl.PluralRules('en-US', { type: 'ordinal' }).select(x.x)) { case 'one': return `foo`; default: return `baz`; } } } })()}`"

  it "TypeScript backend" $ do
    [r|{ "f": { "message": "{x} <z>{y, number}</z> {y, number}", "backend": "ts" } }|]
      =*= withReactImport "export const f: (x: { x: string; z: (x: string) => string; y: number }) => string = x => `${x.x} ${x.z(`${new Intl.NumberFormat('en-US').format(x.y)}`)} ${new Intl.NumberFormat('en-US').format(x.y)}`"

  it "TypeScriptReact backend" $ do
    [r|{ "f": { "message": "{x} <z>{y, number}</z>", "backend": "tsx" } }|]
      =*= withReactImport "export const f: (x: { x: string; z: (x: React.ReactElement) => React.ReactElement; y: number }) => React.ReactElement = x => <>{x.x} {x.z(<>{new Intl.NumberFormat('en-US').format(x.y)}</>)}</>"
