module Intlc.Core where

import           Intlc.ICU (AnnMessage, Message, sansAnnMsg)
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

data AnnTranslation = AnnTranslation
  { amessage :: AnnMessage
  , abackend :: Backend
  , amdesc   :: Maybe Text
  }
  deriving (Show, Eq)

data Translation = Translation
  { message :: Message
  , backend :: Backend
  , mdesc   :: Maybe Text
  }
  deriving (Show, Eq)

type Dataset = Map Text

datasetSansAnn :: Dataset AnnTranslation -> Dataset Translation
datasetSansAnn = fmap translationSansAnn

translationSansAnn :: AnnTranslation -> Translation
translationSansAnn x = Translation
  { message = sansAnnMsg x.amessage
  , backend = x.abackend
  , mdesc = x.amdesc
  }
