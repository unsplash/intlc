module Intlc.EndToEndSpec (spec) where

import           Data.ByteString.Lazy (ByteString)
import           Intlc.Compiler       (dataset)
import           Intlc.Parser         (parseDataset)
import           Prelude              hiding (ByteString)
import           Test.Hspec

(=*=) :: ByteString -> Text -> IO ()
x =*= y = f x `shouldBe` Right y
  where f = fmap dataset . parseDataset

spec :: Spec
spec = describe "end-to-end" $ do
  it "example message" $ do
        "{ \"title\": { \"message\": \"Unsplash\" }, \"greeting\": { \"message\": \"Hello <bold>{name}</bold>, {age, number}!\", \"backend\": \"ts\" } }"
    =*= "export const greeting = (x: { bold: (x: string) => string; name: string; age: number }) => `Hello ${x.bold(`${x.name}`)}, ${x.age}!`\nexport const title = 'Unsplash'\n"
