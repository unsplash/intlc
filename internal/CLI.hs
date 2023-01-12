module CLI (Opts (..), getOpts) where

import qualified Intlc.Backend.JSON.Compiler as JSON
import           Options.Applicative
import           Prelude

data Opts
  = Lint FilePath
  -- Takes stdin.
  | ExpandPlurals JSON.Formatting

getOpts :: IO Opts
getOpts = execParser (info (opts <**> helper) (progDesc h))
  where h = "Additional tooling for Unsplash."

opts :: Parser Opts
opts = subparser . mconcat $
  [ command "lint" (info (lint <**> helper) mempty)
  , command "expand-plurals" (info (expandPlurals <**> helper) mempty)
  ]

lint :: Parser Opts
lint = Lint <$> pathp

expandPlurals :: Parser Opts
expandPlurals = ExpandPlurals <$> minifyp

pathp :: Parser FilePath
pathp = argument str (metavar "filepath")

minifyp :: Parser JSON.Formatting
minifyp = flag JSON.Pretty JSON.Minified (long "minify")
