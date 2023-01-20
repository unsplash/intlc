module CLI (Opts (..), getOpts, ICUModifiers (..)) where

import qualified Intlc.Backend.JSON.Compiler as JSON
import           Intlc.Core                  (Locale (..))
import           Intlc.Linter                (LintRuleset (..))
import           Intlc.Printer               (IndentStyle (..), def)
import           Options.Applicative
import           Prelude

data Opts
  = Compile FilePath Locale
  | Flatten FilePath JSON.Formatting [ICUModifiers]
  | Lint    FilePath LintRuleset
  | Prettify Text IndentStyle

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
flatten = Flatten <$> pathp <*> jsonfmtp <*> expandp
  where expandp = flag mempty (pure ExpandPlurals) (long "expand-plurals" <> hidden)
        jsonfmtp = f <$> minifyp <*> indentp
          where f False x = JSON.Pretty x
                f True  _ = JSON.Minified
        minifyp = flag False True (long "minify")

lint :: Parser Opts
lint = Lint <$> pathp <*> internalp
  where internalp = flag ExternalLintsOnly AllLints (long "with-internal" <> hidden)

msgp :: Parser Text
msgp = argument str (metavar "message")

pathp :: Parser FilePath
pathp = argument str (metavar "filepath")

localep :: Parser Locale
localep = Locale <$> strOption (short 'l' <> long "locale")

prettify :: Parser Opts
prettify = Prettify <$> msgp <*> indentp

indentp :: Parser IndentStyle
indentp = option (eitherReader parseIndentation) (value def <> long "indent" <> metavar "NAT")
  where parseIndentation x
          | x == "tab" || x == "tabs" = Right Tabs
          | otherwise = maybe (Left e) (Right . Spaces) (readMaybe x)
          where e = "Requires a natural number of spaces or tabs."
