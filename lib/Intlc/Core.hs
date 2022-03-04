{-# LANGUAGE DeriveGeneric #-}

module Intlc.Core where

import           Data.Aeson          (FromJSON (..), ToJSON (toEncoding),
                                      withObject, withText, (.!=), (.:), (.:?),
                                      (.=))
import           Data.Aeson.Encoding (pairs, string)
import qualified Data.Text           as T
import           Intlc.ICU           (Message)
import           Prelude

-- Locales are too broad and too much of a moving target to validate, so this
-- is a source of unsafety for consumers.
newtype Locale = Locale Text
  deriving (Show, Eq)

type UnparsedMessage = Text

data Backend
  = TypeScript
  | TypeScriptReact
  deriving (Show, Eq, Generic)

instance FromJSON Backend where
  parseJSON = withText "Backend" decode
    where decode "ts"  = pure TypeScript
          decode "tsx" = pure TypeScriptReact
          decode x     = fail $ "Unknown backend: " <> T.unpack x

instance ToJSON Backend where
  toEncoding TypeScript      = string "ts"
  toEncoding TypeScriptReact = string "tsx"

data UnparsedTranslation = UnparsedTranslation
  { umessage :: UnparsedMessage
  , ubackend :: Backend
  , umdesc   :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON UnparsedTranslation where
  parseJSON = withObject "UnparsedTranslation" decode
    where decode x = UnparsedTranslation
            <$> x .: "message"
            <*> x .:? "backend" .!= TypeScript
            <*> x .:? "description"

instance ToJSON UnparsedTranslation where
  toEncoding (UnparsedTranslation msg be md) = pairs $
       "message"     .= msg
    <> "backend"     .= be
    <> "description" .= md

data Translation = Translation
  { message :: Message
  , backend :: Backend
  , mdesc   :: Maybe Text
  }
  deriving (Show, Eq)

type Dataset = Map Text
