module Intlc.Core where

import           Data.Aeson (FromJSON (..), withObject, withText, (.!=), (.:),
                             (.:?))
import qualified Data.Text  as T
import           Intlc.ICU  (Message)
import           Prelude

-- Locales are too broad and too much of a moving target to validate, so this
-- is a source of unsafety for consumers.
newtype Locale = Locale Text
  deriving (Show, Eq)

type UnparsedMessage = Text

data Backend
  = TypeScript
  | TypeScriptReact
  deriving (Show, Eq)

instance FromJSON Backend where
  parseJSON = withText "Backend" decode
    where decode "ts"  = pure TypeScript
          decode "tsx" = pure TypeScriptReact
          decode x     = fail $ "Unknown backend: " <> T.unpack x

data UnparsedTranslation = UnparsedTranslation
  { umessage :: UnparsedMessage
  , ubackend :: Backend
  }
  deriving (Show, Eq)

instance FromJSON UnparsedTranslation where
  parseJSON = withObject "UnparsedTranslation" decode
    where decode x = UnparsedTranslation
            <$> x .: "message"
            <*> x .:? "backend" .!= TypeScript

data Translation = Translation
  { message :: Message
  , backend :: Backend
  }
  deriving (Show, Eq)

type Dataset = Map Text
