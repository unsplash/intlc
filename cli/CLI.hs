module CLI (Opts (..), getOpts) where

import           Intlc.Core          (Locale (..))
import           Options.Applicative
import           Prelude

data Opts = Opts
  { path   :: FilePath
  , locale :: Locale
  }

getOpts :: IO Opts
getOpts = execParser (info opts mempty)

opts :: Parser Opts
opts = Opts <$> pathp <*> localep

pathp :: Parser FilePath
pathp = argument str (metavar "filepath")

localep :: Parser Locale
localep = Locale <$> strOption (short 'l' <> long "locale")
