-- We reuse Megaparsec's infrastructure to format arbitrary application errors
-- with source annotations.

module Intlc.Error (WithAnn, fmt) where

import qualified Data.Text                     as T
import           Prelude
import           Text.Megaparsec
import           Text.Megaparsec.Error.Builder

type WithAnn a = (Int, a)

fmt :: ShowErrorComponent a => FilePath -> Text -> NonEmpty (WithAnn a) -> Text
fmt path content lints = T.pack $ errorBundlePretty (buildParseErrBundle path content lints)

buildParseErrBundle :: FilePath -> Text -> NonEmpty (WithAnn a) -> ParseErrorBundle Text a
buildParseErrBundle path content lints = ParseErrorBundle (buildParseErr <$> lints) (buildPosState path content)

buildParseErr :: WithAnn a -> ParseError Text a
buildParseErr (i, x) = errFancy i . fancy . ErrorCustom $ x

-- This could probably be rewritten to be more efficient.
buildPosState :: FilePath -> Text -> PosState Text
buildPosState path content = PosState content 0 (initialPos path) defaultTabWidth mempty
