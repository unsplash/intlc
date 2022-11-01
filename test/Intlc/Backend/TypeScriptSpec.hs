module Intlc.Backend.TypeScriptSpec (spec) where

import           Data.Functor.Foldable             (embed)
import qualified Data.Text                         as T
import           Intlc.Backend.JavaScript.Compiler (InterpStrat (..))
import           Intlc.Backend.TypeScript.Compiler (compileNamedExport,
                                                    compileTypeof)
import qualified Intlc.Backend.TypeScript.Language as TS
import           Intlc.Core                        (Locale (Locale))
import qualified Intlc.ICU                         as ICU
import           Prelude
import           System.FilePath                   ((<.>), (</>))
import           Test.Hspec
import           Test.Hspec.Golden                 (Golden (..), defaultGolden)

golden :: InterpStrat -> (ICU.Message ICU.Node -> Text) -> String -> ICU.Message ICU.Node -> Golden String
golden strat compiler name msg = baseCfg
  { goldenFile = goldenFile baseCfg <.> fileExt
  , actualFile = actualFile baseCfg <&> (<.> fileExt)
  }
  where baseCfg = defaultGolden fileName out
        fileName = "ts" </> name
        fileExt =
          case strat of
            TemplateLit -> "ts"
            JSX         -> "tsx"
        out = T.unpack . compiler $ msg

spec :: Spec
spec = describe "TypeScript compiler" $ do
  describe "golden" $ do
    let msg = ICU.Message . mconcat $
          [ "Hello "
          , ICU.Callback' "bold" (
              ICU.String' "name"
            )
          , "! You are "
          , ICU.CardinalInexact'
              "age"
              (pure (ICU.PluralExact "42", "very cool"))
              (pure (ICU.Zero, "new around here"))
            "not all that interesting"
          , ". Regardless, the magic number is most certainly "
          , ICU.Number' "magicNumber"
          , "! The date is "
          , ICU.Date' "todayDate" ICU.Short
          , ", and the time is "
          , ICU.Time' "currTime" ICU.Full
          , ". And just to recap, your name is "
          , ICU.SelectNamed' "name" . fromList $
              [ ("Sam", "undoubtedly excellent")
              , ("Ashley", "fairly good")
              ]
          , ". Finally, you are "
          , embed $ ICU.BoolF
            { ICU.nameF = "isDev"
            , ICU.trueCaseF = "a software engineer"
            , ICU.falseCaseF = "something less fun"
            , ICU.nextF = mempty
            }
          , ". Bonus: Some characters that might need escaping! ` ``"
          ]

    describe "with template literal strategy" $ do
      it "compiles correct type definitions" $ do
        -- Prefix output so it's a valid statement.
        let golden' = golden TemplateLit (("export type Test = " <>) . compileTypeof TemplateLit)

        golden' "typedef" msg

      it "compiles correct named exports" $ do
        -- Use dummy locale that can't realistically have been mistakenly
        -- hardcoded anywhere.
        let golden' = golden TemplateLit (compileNamedExport TemplateLit (Locale "te-ST") "test")

        golden' "named-export" msg

    describe "with JSX strategy" $ do
      it "compiles correct type definitions" $ do
        -- Prefix output so it's a valid statement.
        let golden' = golden JSX (("export type Test = " <>) . compileTypeof JSX)

        golden' "typedef" msg

      it "compiles correct named exports" $ do
        -- Use dummy locale that can't realistically have been mistakenly
        -- hardcoded anywhere.
        let golden' = golden JSX (compileNamedExport JSX (Locale "te-ST") "test")

        golden' "named-export" msg

    -- Typechecking happens externally.
    it "typechecks nested selects" $ golden TemplateLit (compileNamedExport TemplateLit (Locale "te-ST") "test") "nested-select" $
      ICU.Message (ICU.SelectNamed' "x" $ fromList
        [ ("a", mempty)
        , ("b", ICU.SelectNamed' "x" $ fromList
          [ ("a", mempty) -- <-- without a workaround, TypeScript will have narrowed and reject this case
          , ("b", mempty)
          ]
        )])

  describe "collects nested arguments" $ do
    let args (TS.Lambda xs _) = xs
    let fromNode = args . TS.fromMsg TS.TFragment . ICU.Message
    let fromArgs = fromList

    it "in select" $ do
      let x = ICU.SelectNamed' "x" . pure $ ("foo", ICU.String' "y")
      let ys =
              [ ("x", pure (TS.TStrLitUnion (pure "foo")))
              , ("y", pure TS.TStr)
              ]
      fromNode x `shouldBe` fromArgs ys

    it "in cardinal plural" $ do
      let x = ICU.CardinalExact' "x" . pure $
                (ICU.PluralExact "42", ICU.String' "y")
      let ys =
              [ ("x", pure (TS.TNumLitUnion (pure "42")))
              , ("y", pure TS.TStr)
              ]
      fromNode x `shouldBe` fromArgs ys

    it "in ordinal plural" $ do
      let x = ICU.Ordinal' "x"
                [(ICU.PluralExact "42", ICU.Date' "foo" ICU.Short)]
                (pure (ICU.Few, ICU.String' "bar"))
                (ICU.Number' "baz")
      let ys =
              [ ("x", pure TS.TNum)
              , ("foo", pure TS.TDate)
              , ("bar", pure TS.TStr)
              , ("baz", pure TS.TNum)
              ]
      fromNode x `shouldBe` fromArgs ys

    it "in boolean" $ do
      let x = ICU.Bool' "x"
                (ICU.String' "y")
                (ICU.Number' "z")
      let ys =
              [ ("x", pure TS.TBool)
              , ("y", pure TS.TStr)
              , ("z", pure TS.TNum)
              ]
      fromNode x `shouldBe` fromArgs ys
