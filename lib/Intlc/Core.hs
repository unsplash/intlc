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

data Translation a = Translation
  { message :: a
  , backend :: Backend
  , mdesc   :: Maybe Text
  }
  deriving (Show, Eq)

type Dataset = Map Text

datasetSansAnn :: Dataset (Translation AnnMessage) -> Dataset (Translation Message)
datasetSansAnn = fmap translationSansAnn

translationSansAnn :: Translation AnnMessage -> Translation Message
translationSansAnn x = Translation
  { message = sansAnnMsg x.message
  , backend = x.backend
  , mdesc = x.mdesc
  }
