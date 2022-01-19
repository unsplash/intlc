module CLI (Opts (..), getOpts) where

import           Options.Applicative
import           Prelude

data Opts = Opts
  { path   :: FilePath
  }

getOpts :: IO Opts
getOpts = execParser (info opts mempty)

opts :: Parser Opts
opts = Opts <$> pathp

pathp :: Parser FilePath
pathp = argument str (metavar "filepath")
