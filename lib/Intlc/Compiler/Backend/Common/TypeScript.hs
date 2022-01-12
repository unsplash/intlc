module Intlc.Compiler.Backend.Common.TypeScript where

import           Intlc.Core
import           Prelude
import qualified Intlc.Compiler.Backend.Common.JavaScript as JS
import qualified Data.Text as T

argName :: Text
argName = JS.argName

typ :: Text -> ICUType -> Text
typ _ Number       = "number"
typ x (Callback _) = pure (argName, x) `lambda` x

namedExport :: Text -> Text -> Text
namedExport = JS.namedExport

str :: Text -> Text
str = JS.str

templateLits :: Text -> Text
templateLits = JS.templateLits

templateInterp :: Text -> Text
templateInterp = JS.templateInterp

lambda :: [(Text, Text)] -> Text -> Text
lambda = JS.lambda . fmap (uncurry typedArg)
  where typedArg n t = n <> ": " <> t

apply :: Text -> Text
apply = JS.apply

prop :: Text -> Text -> Text
prop = JS.prop

obj :: [(Text, Text)] -> Text
obj xs = "{ " <> T.intercalate "; " (uncurry prop' <$> xs) <> " }"
  where prop' n t = n <> ": " <> t
