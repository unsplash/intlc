module Intlc.Prettify (prettify) where

import           Intlc.Backend.ICU.Compiler (Formatting (..), compileMsg)
import qualified Intlc.ICU                  as ICU
import           Intlc.Printer              (IndentStyle)
import           Prelude

prettify :: IndentStyle -> ICU.Message ICU.Node -> Text
prettify = compileMsg . MultiLine
