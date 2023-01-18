module CLI (Opts (..), getOpts, ICUModifiers (..)) where

import qualified Intlc.Backend.JSON.Compiler as JSON
import           Intlc.Core                  (Locale (..))
import           Intlc.Linter                (LintRuleset (..))
import           Options.Applicative
import           Prelude

data Opts
  = Compile FilePath Locale
  | Flatten FilePath JSON.Formatting [ICUModifiers]
  | Lint    FilePath LintRuleset
  | Prettify Text

data ICUModifiers
  = ExpandPlurals

getOpts :: IO Opts
getOpts = execParser (info (opts <**> helper) (progDesc h))
  where h = "Compile ICU messages into code."

opts :: Parser Opts
opts = subparser . mconcat $
  [ command "compile"  (info (compile  <**> helper) mempty)
  , command "flatten"  (info (flatten  <**> helper) mempty)
  , command "lint"     (info (lint     <**> helper) mempty)
  , command "prettify" (info (prettify <**> helper) mempty)
  ]

compile :: Parser Opts
compile = Compile <$> pathp <*> localep

flatten :: Parser Opts
flatten = Flatten <$> pathp <*> minifyp <*> expandp
  where expandp = flag mempty (pure ExpandPlurals) (long "expand-plurals" <> hidden)

lint :: Parser Opts
lint = Lint <$> pathp <*> internalp
  where internalp = flag ExternalLintsOnly AllLints (long "with-internal" <> hidden)

msgp :: Parser Text
msgp = argument str (metavar "message")

pathp :: Parser FilePath
pathp = argument str (metavar "filepath")

localep :: Parser Locale
localep = Locale <$> strOption (short 'l' <> long "locale")

minifyp :: Parser JSON.Formatting
minifyp = flag JSON.Pretty JSON.Minified (long "minify")

prettify :: Parser Opts
prettify = Prettify <$> msgp
