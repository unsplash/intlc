module Intlc.Linter where
import           Data.Text (all)

import           Data.Char (isAscii)
import           Intlc.ICU
import           Prelude   hiding (Type)

data LintingError
  = TooManyInterpolations
  | EmojiDetected
  deriving (Eq, Show)

data Status
  = Success
  | Failure (NonEmpty LintingError)
  deriving (Eq, Show)

statusToMaybe :: Status -> Maybe (NonEmpty LintingError)
statusToMaybe Success      = Nothing
statusToMaybe (Failure xs) = Just xs

maybeToStatus :: Maybe (NonEmpty LintingError) -> Status
maybeToStatus Nothing   = Success
maybeToStatus (Just xs) = Failure xs

interpolationsRule :: Stream -> Maybe LintingError
interpolationsRule = go 0
  where
    go :: Int -> Stream -> Maybe LintingError
    go 2 _ = Just TooManyInterpolations
    go _ [] = Nothing
    go n (x : xs) = go n' $ maybeToMonoid mys <> xs
      where
        mys = getStream x
        n' = n + length mys

noEmojiRule :: Stream -> Maybe LintingError
noEmojiRule [] = Nothing
noEmojiRule (x : xs) = if checkAsci x then noEmojiRule $ maybeToMonoid mys <> xs else Just EmojiDetected
  where
    checkAsci :: Token -> Bool
    checkAsci (Plaintext t)       = Data.Text.all isAscii t
    checkAsci (Interpolation t _) = Data.Text.all isAscii t
    mys = getStream x

lint :: Message -> Status
lint (Message stream) = toStatus $ rules `flap` stream
  where
    toStatus = maybeToStatus . nonEmpty . catMaybes
    rules = [interpolationsRule, noEmojiRule]
