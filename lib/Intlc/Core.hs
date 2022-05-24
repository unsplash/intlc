{-# LANGUAGE DeriveGeneric #-}

module Intlc.Core where

import           Data.Aeson          (ToJSON (toEncoding), (.=))
import           Data.Aeson.Encoding (pairs, string)
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

instance ToJSON Backend where
  toEncoding TypeScript      = string "ts"
  toEncoding TypeScriptReact = string "tsx"

data UnparsedTranslation = UnparsedTranslation
  { umessage :: UnparsedMessage
  , ubackend :: Backend
  , umdesc   :: Maybe Text
  }
  deriving (Show, Eq, Generic)

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
