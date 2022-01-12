module Intlc.Compiler.Backend.Common.JavaScript where

import Prelude
import qualified Data.Text as T

argName :: Text
argName = "x"

namedExport :: Text -> Text -> Text
namedExport n v = "export const " <> n <> " = " <> v

str :: Text -> Text
str x = "'" <> x <> "'"

templateLits :: Text -> Text
templateLits x = "`" <> x <> "`"

templateInterp :: Text -> Text
templateInterp x = "${" <> x <> "}"

-- Always supply surrounding parentheses for free TypeScript support (because
-- TypeScript always requires parentheses around typed arguments, unlike untyped
-- unary JavaScript functions).
lambda :: [Text] -> Text -> Text
lambda is o = "(" <> T.intercalate ", " is <> ") => " <> o

apply :: Text -> Text
apply x = "(" <> x <> ")"

prop :: Text -> Text -> Text
o `prop` p = o <> "." <> p
