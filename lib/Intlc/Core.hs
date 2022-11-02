module Intlc.Core where

import           Intlc.ICU (AnnNode, Message, Node, sansAnn)
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

datasetSansAnn :: Dataset (Translation (Message AnnNode)) -> Dataset (Translation (Message Node))
datasetSansAnn = fmap translationSansAnn

translationSansAnn :: Translation (Message AnnNode) -> Translation (Message Node)
translationSansAnn x = x { message = sansAnn <$> x.message }
