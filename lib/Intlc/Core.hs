module Intlc.Core where

import           Data.Aeson (FromJSON (..), withObject, withText, (.!=), (.:),
                             (.:?))
import qualified Data.Text  as T
import           Intlc.ICU  (Message)
import           Prelude

type UnparsedMessage = Text

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
