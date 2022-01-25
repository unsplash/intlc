module CLI (Opts (..), getOpts) where

import           Intlc.Core          (Locale (..))
import           Options.Applicative
import           Prelude

data Opts
  = Compile FilePath Locale
  | Flatten FilePath

getOpts :: IO Opts
getOpts = execParser (info opts mempty)

opts :: Parser Opts
opts = subparser
  ( command "compile" (info compile mempty)
 <> command "flatten" (info flatten mempty)
  )

compile :: Parser Opts
compile = Compile <$> pathp <*> localep

flatten :: Parser Opts
flatten = Flatten <$> pathp

pathp :: Parser FilePath
pathp = argument str (metavar "filepath")

localep :: Parser Locale
localep = Locale <$> strOption (short 'l' <> long "locale")
