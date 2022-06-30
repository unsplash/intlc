module Intlc.Backend.TypeScriptSpec (spec) where

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

golden :: InterpStrat -> (ICU.Message -> Text) -> String -> ICU.Message -> Golden String
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
    let msg = ICU.Message $
          [ ICU.Plaintext "Hello "
          , ICU.Interpolation "bold" (ICU.Callback (pure $
              ICU.Interpolation "name" ICU.String
            ))
          , ICU.Plaintext "! You are "
          , ICU.Interpolation "age" (ICU.Plural (ICU.Cardinal
              (ICU.MixedPlural
              (pure (ICU.PluralCase (ICU.PluralExact "42") (pure (ICU.Plaintext "very cool"))))
              (pure (ICU.PluralCase ICU.Zero (pure (ICU.Plaintext "new around here"))))
              (ICU.PluralWildcard (pure (ICU.Plaintext "not all that interesting")))
              )
            ))
          , ICU.Plaintext ". Regardless, the magic number is most certainly "
          , ICU.Interpolation "magicNumber" ICU.Number
          , ICU.Plaintext "! The date is "
          , ICU.Interpolation "todayDate" (ICU.Date ICU.Short)
          , ICU.Plaintext ", and the time is "
          , ICU.Interpolation "currTime" (ICU.Time ICU.Full)
          , ICU.Plaintext ". And just to recap, your name is "
          , ICU.Interpolation "name" (ICU.Select (fromList
              [ ICU.SelectCase "Sam" [ICU.Plaintext "undoubtedly excellent"]
              , ICU.SelectCase "Ashley" [ICU.Plaintext "fairly good"]
              ]
            ) Nothing)
          , ICU.Plaintext ". Finally, you are "
          , ICU.Interpolation "isDev" (ICU.Bool
            { ICU.trueCase = [ICU.Plaintext "a software engineer"]
            , ICU.falseCase = [ICU.Plaintext "something less fun"]
            })
          , ICU.Plaintext ". Bonus: Some characters that might need escaping! ` ``"
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
    it "typechecks nested selects" $ do
      golden TemplateLit (compileNamedExport TemplateLit (Locale "te-ST") "test") "nested-select" $
        ICU.Message [ICU.Interpolation "x" $ flip ICU.Select Nothing $ (fromList
          [ ICU.SelectCase "a" []
          , ICU.SelectCase "b" [ICU.Interpolation "x" $ flip ICU.Select Nothing $ (fromList
            [ ICU.SelectCase "a" [] -- <-- without a workaround, TypeScript will have narrowed and reject this case
            , ICU.SelectCase "b" []
            ])]
          ])]

  describe "collects nested arguments" $ do
    let args (TS.Lambda xs _) = xs
    let fromToken = args . TS.fromMsg TS.TFragment . ICU.Message . pure . ICU.Interpolation "x"
    let fromArgs = fromList

    it "in select" $ do
      let x = flip ICU.Select Nothing . pure $ ICU.SelectCase "foo" [ICU.Interpolation "y" ICU.String]
      let ys =
              [ ("x", pure (TS.TStrLitUnion (pure "foo")))
              , ("y", pure TS.TStr)
              ]
      fromToken x `shouldBe` fromArgs ys

    it "in cardinal plural" $ do
      let x = ICU.Plural . ICU.Cardinal . flip ICU.LitPlural Nothing . pure $
                ICU.PluralCase (ICU.PluralExact "42") [ICU.Interpolation "y" ICU.String]
      let ys =
              [ ("x", pure (TS.TNumLitUnion (pure "42")))
              , ("y", pure TS.TStr)
              ]
      fromToken x `shouldBe` fromArgs ys

    it "in ordinal plural" $ do
      let x = ICU.Plural . ICU.Ordinal $ ICU.OrdinalPlural
                [ICU.PluralCase (ICU.PluralExact "42") [ICU.Interpolation "foo" (ICU.Date ICU.Short)]]
                (pure $ ICU.PluralCase ICU.Few [ICU.Interpolation "bar" ICU.String])
                (ICU.PluralWildcard [ICU.Interpolation "baz" ICU.Number])
      let ys =
              [ ("x", pure TS.TNum)
              , ("foo", pure TS.TDate)
              , ("bar", pure TS.TStr)
              , ("baz", pure TS.TNum)
              ]
      fromToken x `shouldBe` fromArgs ys

    it "in boolean" $ do
      let x = ICU.Bool
                [ICU.Interpolation "y" ICU.String]
                [ICU.Interpolation "z" ICU.Number]
      let ys =
              [ ("x", pure TS.TBool)
              , ("y", pure TS.TStr)
              , ("z", pure TS.TNum)
              ]
      fromToken x `shouldBe` fromArgs ys
