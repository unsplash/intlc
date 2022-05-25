{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module CLI (Opts (..), getOpts) where

import           Options.Applicative
import           Prelude

data Opts
  = Lint FilePath

getOpts :: IO Opts
getOpts = execParser (info (opts <**> helper) (progDesc h))
  where h = "Additional tooling for Unsplash."

opts :: Parser Opts
opts = subparser
  ( command "lint" (info (lint <**> helper) mempty)
  )

lint :: Parser Opts
lint = Lint <$> pathp

pathp :: Parser FilePath
pathp = argument str (metavar "filepath")
