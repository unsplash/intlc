module Intlc.Prettify (prettify) where

import           Intlc.Backend.ICU.Compiler (Formatting (..), compileMsg)
import qualified Intlc.ICU                  as ICU
import           Prelude

prettify :: ICU.Message ICU.Node -> Text
prettify = compileMsg MultiLine
