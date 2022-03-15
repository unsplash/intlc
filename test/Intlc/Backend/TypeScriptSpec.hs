module Intlc.Backend.TypeScriptSpec (spec) where

import qualified Data.Text                         as T
import           Intlc.Backend.JavaScript.Compiler (InterpStrat (..))
import           Intlc.Backend.TypeScript.Compiler (compileNamedExport,
                                                    compileTypeof)
import           Intlc.Core                        (Locale (Locale))
import qualified Intlc.ICU                         as ICU
import           Prelude                           hiding (ByteString)
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
  let msg = ICU.Dynamic . fromList $
        [ ICU.Plaintext "Hello "
        , ICU.Interpolation (ICU.Arg "bold" (ICU.Callback (pure $
            ICU.Interpolation (ICU.Arg "name" ICU.String
          ))))
        , ICU.Plaintext "! You are "
        , ICU.Interpolation (ICU.Arg "age" (ICU.Plural (ICU.Cardinal
            (ICU.MixedPlural
            (pure (ICU.PluralCase (ICU.PluralExact "42") (pure (ICU.Plaintext "very cool"))))
            (pure (ICU.PluralCase ICU.Zero (pure (ICU.Plaintext "new around here"))))
            (ICU.PluralWildcard (pure (ICU.Plaintext "not all that interesting")))
            )
          )))
        , ICU.Plaintext ". Regardless, the magic number is most certainly "
        , ICU.Interpolation (ICU.Arg "magicNumber" ICU.Number)
        , ICU.Plaintext "! The date is "
        , ICU.Interpolation (ICU.Arg "todayDate" (ICU.Date ICU.Short))
        , ICU.Plaintext ", and the time is "
        , ICU.Interpolation (ICU.Arg "currTime" (ICU.Time ICU.Full))
        , ICU.Plaintext ". And just to recap, your name is "
        , ICU.Interpolation (ICU.Arg "name" (ICU.Select (fromList
            [ ICU.SelectCase "Sam" [ICU.Plaintext "undoubtedly excellent"]
            , ICU.SelectCase "Ashley" [ICU.Plaintext "fairly good"]
            ]
          ) Nothing))
        , ICU.Plaintext ". Finally, you are "
        , ICU.Interpolation (ICU.Arg "isDev" (ICU.Bool
          { ICU.trueCase = [ICU.Plaintext "a software engineer"]
          , ICU.falseCase = [ICU.Plaintext "something less fun"]
          }))
        , ICU.Plaintext "."
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


