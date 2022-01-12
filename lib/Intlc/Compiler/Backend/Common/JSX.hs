module Intlc.Compiler.Backend.Common.JSX where

import Prelude

fragment :: Text -> Text
fragment x = "<>" <> x <> "</>"

interpolate :: Text -> Text
interpolate x = "{" <> x <> "}"
