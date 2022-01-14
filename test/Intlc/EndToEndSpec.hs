module Intlc.EndToEndSpec (spec) where

import           Data.ByteString.Lazy (ByteString)
import           Intlc.Compiler       (dataset)
import           Intlc.Parser         (parseDataset)
import           Prelude              hiding (ByteString)
import           Test.Hspec
import           Text.RawString.QQ    (r)

(=*=) :: ByteString -> Text -> IO ()
x =*= y = f x `shouldBe` Right y
  where f = fmap dataset . parseDataset

spec :: Spec
spec = describe "end-to-end" $ do
  it "example message" $ do
    [r|{ "title": { "message": "Unsplash" }, "greeting": { "message": "Hello <bold>{name}</bold>, {age, number}!", "backend": "ts" } }|]
      =*= "export const title = 'Unsplash'\nexport const greeting = (x: { bold: (x: string) => string; name: string; age: number }) => `Hello ${x.bold(`${x.name}`)}, ${x.age}!`"

  it "TypeScript backend" $ do
    [r|{ "f": { "message": "{x} <z>{y, number}</z>", "backend": "ts" } }|]
      =*= "export const f = (x: { x: string; z: (x: string) => string; y: number }) => `${x.x} ${x.z(`${x.y}`)}`"

  it "TypeScriptReact backend" $ do
    [r|{ "f": { "message": "{x} <z>{y, number}</z>", "backend": "tsx" } }|]
      =*= "export const f = (x: { x: ReactElement; z: (x: ReactElement) => ReactElement; y: number }) => <>{x.x} {x.z(<>{x.y}</>)}</>"
