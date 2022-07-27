{-# LANGUAGE DeriveGeneric #-}

module Intlc.Core where

import           Intlc.ICU (Message)
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

data UnparsedTranslation = UnparsedTranslation
  { umessage :: UnparsedMessage
  , ubackend :: Backend
  , umdesc   :: Maybe Text
  }
  deriving (Show, Eq, Generic)

data Translation = Translation
  { message :: Message
  , backend :: Backend
  , mdesc   :: Maybe Text
  }
  deriving (Show, Eq)

type Dataset = Map Text
