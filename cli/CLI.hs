module CLI (Opts (..), getOpts) where

import           Intlc.Core          (Locale (..))
import           Options.Applicative
import           Prelude

data Opts
  = Compile FilePath Locale
  | Flatten FilePath

getOpts :: IO Opts
getOpts = execParser (info (opts <**> helper) (progDesc h))
  where h = "Compile ICU messages into code."

opts :: Parser Opts
opts = subparser . mconcat $
  [ command "compile" (info (compile <**> helper) mempty)
  , command "flatten" (info (flatten <**> helper) mempty)
  ]

compile :: Parser Opts
compile = Compile <$> pathp <*> localep

flatten :: Parser Opts
flatten = Flatten <$> pathp

pathp :: Parser FilePath
pathp = argument str (metavar "filepath")

localep :: Parser Locale
localep = Locale <$> strOption (short 'l' <> long "locale")
