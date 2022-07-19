module CLI (Opts (..), getOpts) where

import           Options.Applicative
import           Prelude

data Opts
  = Lint FilePath
  -- Takes stdin.
  | ExpandPlurals

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
expandPlurals = pure ExpandPlurals

pathp :: Parser FilePath
pathp = argument str (metavar "filepath")
