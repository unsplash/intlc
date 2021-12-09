module Intlc.Compiler where

import           Intlc.Core
import           Prelude

translation :: Translation -> Text
translation (Static x)   = "'" <> x <> "'"
translation (Dynamic xs) = "`" <> foldMap token xs <> "`"

token :: Token -> Text
token (Plaintext x)           = x
token (Interpolation (Arg x)) = "${" <> x <> "}"
