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
        "{ \"title\": \"Unsplash\", \"greeting\": \"Hello, {forename} {surname}!\" }"
    =*= "export default {\n  greeting: ({ forename, surname }: { forename: string; surname: string }) => `Hello, ${forename} ${surname}!`,\n  title: 'Unsplash',\n}"
