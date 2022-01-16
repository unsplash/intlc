module Intlc.Core where

import           Data.Aeson       (FromJSON (..), withObject, withText, (.!=),
                                   (.:), (.:?))
import qualified Data.Text        as T
import           Prelude

data ICUType
  = String
  | Number
  | Date DateFmt
  | Plural (NonEmpty PluralCase) PluralWildcard
  -- We diverge from icu4j by not requiring a wildcard case.
  | Select (NonEmpty SelectCase) (Maybe SelectWildcard)
  | Callback [Token]
  deriving (Show, Eq)

data DateFmt
  = Short
  | Medium
  | Long
  | Full
  deriving (Show, Eq)

data SelectCase = SelectCase Text [Token]
  deriving (Show, Eq)

newtype SelectWildcard = SelectWildcard [Token]
  deriving (Show, Eq)

-- `Text` here is our count. It's represented as a string so that we can dump
-- it back out without thinking about converting numeric types across languages.
data PluralCase = PluralCase Text [Token]
  deriving (Show, Eq)

newtype PluralWildcard = PluralWildcard [Token]
  deriving (Show, Eq)

data Arg = Arg Text ICUType
  deriving (Show, Eq)

-- | A token is either an interpolation - some sort of identifier for input -
-- or mere plaintext. A collection of tokens make up any message. A message
-- without any interpolation will be a single `Plaintext` token.
data Token
  = Plaintext Text
  | Interpolation Arg
  deriving (Show, Eq)

type UnparsedMessage = Text

data Message
  = Static Text
  | Dynamic [Token]
  deriving (Show, Eq)

data Backend
  = TypeScript
  | TypeScriptReact

instance FromJSON Backend where
  parseJSON = withText "Backend" decode
    where decode "ts"  = pure TypeScript
          decode "tsx" = pure TypeScriptReact
          decode x     = fail $ "Unknown backend: " <> T.unpack x

data UnparsedTranslation = UnparsedTranslation
  { umessage :: UnparsedMessage
  , ubackend :: Backend
  }

instance FromJSON UnparsedTranslation where
  parseJSON = withObject "UnparsedTranslation" decode
    where decode x = UnparsedTranslation
            <$> x .: "message"
            <*> x .:? "backend" .!= TypeScript

data Translation = Translation
  { message :: Message
  , backend :: Backend
  }

type Dataset = Map Text
