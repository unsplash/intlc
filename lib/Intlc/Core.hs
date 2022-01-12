module Intlc.Core where

import           Data.Aeson (FromJSON (..), withObject, withText, (.:), (.:?), (.!=))
import qualified Data.Text  as T
import           Prelude

data ICUType
  = Number
  | Callback [Token]
  deriving (Show, Eq)

data Arg = Arg Text (Maybe ICUType)
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

instance FromJSON Backend where
  parseJSON = withText "Backend" decode
    where decode "ts"  = pure TypeScript
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
