module CLI (Opts (..), getOpts) where

import qualified Intlc.Backend.JSON.Compiler as JSON
import           Options.Applicative
import           Prelude

data Opts
  -- Takes stdin.
  = ExpandPlurals JSON.Formatting

getOpts :: IO Opts
getOpts = execParser (info (opts <**> helper) (progDesc h))
  where h = "Additional tooling for Unsplash."

opts :: Parser Opts
opts = subparser . mconcat $
  [ command "expand-plurals" (info (expandPlurals <**> helper) mempty)
  ]

expandPlurals :: Parser Opts
expandPlurals = ExpandPlurals <$> minifyp

minifyp :: Parser JSON.Formatting
minifyp = flag JSON.Pretty JSON.Minified (long "minify")
