module Intlc.Printer where

import qualified Data.Text as T
import           Prelude

data IndentStyle
  = Tabs
  | Spaces Nat

indenter :: IndentStyle -> Nat -> Text
indenter Tabs       = flip T.replicate "\t" . fromEnum
indenter (Spaces n) = flip T.replicate " " . fromEnum . (* n)
