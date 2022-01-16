module Intlc.Compiler.Backend.Common.TypeScript where

import qualified Data.Text                                as T
import qualified Intlc.Compiler.Backend.Common.JavaScript as JS
import           Intlc.Core
import           Prelude

argName :: Text
argName = JS.argName

typ :: Text -> ICUType -> Text
typ _ String      = "string"
typ _ Date {}     = "Date"
typ _ Number      = "number"
typ _ Plural {}   = "number"
typ x Callback {} = pure (argName, x) `lambda` x

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

iife :: Text -> Text -> Text -> Text
iife = JS.iife

switch :: Text -> Text -> Text
switch = JS.switch

shortSwitchCase :: Text -> Text -> Text
shortSwitchCase = JS.shortSwitchCase

shortSwitchDefault :: Text -> Text
shortSwitchDefault = JS.shortSwitchDefault

fmtDate :: DateFmt -> Text -> Text
fmtDate = JS.fmtDate