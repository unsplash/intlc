module Intlc.Compiler.Backend.Common.JavaScript where

import qualified Data.Text  as T
import           Intlc.Core
import           Prelude

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

applyMany :: [Text] -> Text
applyMany = apply . T.intercalate ", "

prop :: Text -> Text -> Text
o `prop` p = o <> "." <> p

iife :: Text -> Text -> Text -> Text
iife n b a = "(" <> n <> " => { " <> b <> " })(" <> a <> ")"

switch :: Text -> Text -> Text
switch n cs = "switch (" <> n <> ") { " <> cs <> " }"

shortSwitchCase :: Text -> Text -> Text
shortSwitchCase c r = "case " <> c <> ": return " <> r <> ";"

shortSwitchDefault :: Text -> Text
shortSwitchDefault r = "default: return " <> r <> ";"

fmtDate :: DateFmt -> Text -> Text
fmtDate d x = x `prop` "toLocaleString" <> applyMany [str "en-US", "{ dateStyle: " <> str (style d) <> " }"]
  where style Short  = "short"
        style Medium = "medium"
        style Long   = "long"
        style Full   = "full"
