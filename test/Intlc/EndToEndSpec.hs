module Intlc.EndToEndSpec (spec) where

import           Data.ByteString.Lazy (ByteString)
import           Intlc.Compiler       (dataset)
import           Intlc.Parser         (parseDataset)
import           Prelude              hiding (ByteString)
import           Test.Hspec
import           Text.RawString.QQ    (r)

(=*=) :: ByteString -> Text -> IO ()
x =*= y = f x `shouldBe` Right y
  where f = dataset <=< first (pure . show) . parseDataset

spec :: Spec
spec = describe "end-to-end" $ do
  it "example message" $ do
    [r|{ "title": { "message": "Unsplash" }, "greeting": { "message": "Hello <bold>{name}</bold>, {age, number}!", "backend": "ts" } }|]
      =*= "export const title: string = 'Unsplash'\nexport const greeting: (x: { bold: (x: string) => string; name: string; age: number }) => string = x => `Hello ${x.bold(`${x.name}`)}, ${x.age}!`"

  it "parses and discards descriptions" $ do
    [r|{ "brand": { "message": "Unsplash", "description": "The company name" } }|]
      =*= "export const brand: string = 'Unsplash'"

  it "compiles plurals" $ do
    [r|{ "prop": { "message": "Age: {age, plural, =0 {newborn called {name}} =42 {magical} other {boring #}}", "backend": "ts" } }|]
      =*= "export const prop: (x: { age: number; name: string }) => string = x => `Age: ${(() => { switch (x.age) { case 0: return `newborn called ${x.name}`; case 42: return `magical`; default: return `boring ${x.age}`; } })()}`"
    [r|{ "prop": { "message": "Age: {age, plural, =0 {newborn called {name}} =42 {magical} other {boring #}}", "backend": "tsx" } }|]
      =*= "export const prop: (x: { age: number; name: string }) => ReactElement = x => <>Age: {(() => { switch (x.age) { case 0: return <>newborn called {x.name}</>; case 42: return <>magical</>; default: return <>boring {x.age}</>; } })()}</>"

  it "compiles select" $ do
    [r|{ "f": { "message": "{x, select, a {hi} b {yo}}", "backend": "ts" } }|]
      =*= "export const f: (x: { x: 'a' | 'b' }) => string = x => `${(() => { switch (x.x) { case 'a': return `hi`; case 'b': return `yo`; } })()}`"
    [r|{ "f": { "message": "{x, select, a {hi} b {yo} other {ciao}}", "backend": "ts" } }|]
      =*= "export const f: (x: { x: string }) => string = x => `${(() => { switch (x.x) { case 'a': return `hi`; case 'b': return `yo`; default: return `ciao`; } })()}`"

  it "TypeScript backend" $ do
    [r|{ "f": { "message": "{x} <z>{y, number}</z> {y, number}", "backend": "ts" } }|]
      =*= "export const f: (x: { x: string; z: (x: string) => string; y: number }) => string = x => `${x.x} ${x.z(`${x.y}`)} ${x.y}`"

  it "TypeScriptReact backend" $ do
    [r|{ "f": { "message": "{x} <z>{y, number}</z>", "backend": "tsx" } }|]
      =*= "export const f: (x: { x: string; z: (x: ReactElement) => ReactElement; y: number }) => ReactElement = x => <>{x.x} {x.z(<>{x.y}</>)}</>"
