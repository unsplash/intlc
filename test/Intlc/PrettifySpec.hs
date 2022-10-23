module Intlc.PrettifySpec where

import qualified Data.Text      as T
import           Intlc.ICU
import           Intlc.Prettify (prettify)
import           Prelude
import           Test.Hspec

spec :: Spec
spec = describe "prettify" $ do
  let f = prettify . Message

  it "compiles to ICU with multiline formatting" $ do
    let ast = mconcat
          [ Bool' "hasTags"
              (SelectNamed' "type"
                (fromList
                  [ ("overLimit", mconcat [Number' "upperLimit", "+ best free ", String' "formattedListOfTags", " photos on Unsplash"])
                  , ("belowLimit", mconcat [Number' "photoTotal", " best free ", String' "formattedListOfTags", " photos on Unsplash"])
                  ]
                )
              )
              (SelectNamed' "type"
                (fromList
                  [ ("overLimit", mconcat [Number' "upperLimit", "+ best free photos on Unsplash"])
                  , ("belowLimit", mconcat [Number' "photoTotal", " best free photos on Unsplash"])
                  ]
                )
              )
          , " "
          , SelectNamed' "sibling"
              (fromList
                [ ("a", String' "foo")
                , ("b", "bar")
                ]
              )
          ]
    let toTabs = T.replace "  " "\t"
    -- Can't use QuasiQuotes as stylish-haskell removes the trailing whitespace
    -- which exists in the current implementation.
    let expected = T.intercalate "\n"
          [ "{hasTags, boolean, "
          , "  true {{type, select, "
          , "    overLimit {{upperLimit, number}+ best free {formattedListOfTags} photos on Unsplash}"
          , "    belowLimit {{photoTotal, number} best free {formattedListOfTags} photos on Unsplash}"
          , "  }}"
          , "  false {{type, select, "
          , "    overLimit {{upperLimit, number}+ best free photos on Unsplash}"
          , "    belowLimit {{photoTotal, number} best free photos on Unsplash}"
          , "  }}"
          , "} {sibling, select, "
          , "  a {{foo}}"
          , "  b {bar}"
          , "}"
          ]
    -- Some trailing spaces are expected with the current implementation.
    f ast `shouldBe` toTabs expected
