module Intlc.Linter where
import           Data.Text    (partition, unpack)


import           Data.Char    (isAscii)
import           Intlc.ICU
import           Prelude      hiding (Type)
import           Relude.Extra (Foldable1 (toNonEmpty))

data LintingError
  = TooManyInterpolations
  | EmojiDetected (NonEmpty Char)
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
noEmojiRule (x : xs) =  let lintingErrors = noEmojiRule $ maybeToMonoid mys <> xs
                        in case (getAscii x, lintingErrors) of
                          (Just asciiChars,Just (EmojiDetected char)) ->  Just $ EmojiDetected $ asciiChars <> char
                          (Just asciiChars,Nothing) ->  Just $ EmojiDetected asciiChars
                          (Nothing,_)         -> lintingErrors
                          where
                            getAscii :: Token -> Maybe (NonEmpty Char)
                            getAscii token
                              | Plaintext t <- token = getAsciiChars t
                              | Interpolation t _ <- token  = getAsciiChars t
                              where
                                  getAsciiChars :: Text -> Maybe (NonEmpty Char)
                                  getAsciiChars t = let (_, asciiChars) = Data.Text.partition isAscii t in
                                                    nonEmpty . Data.Text.unpack $ asciiChars
                            mys = getStream x

lint :: Message -> Status
lint (Message stream) = toStatus $ rules `flap` stream
  where
    toStatus = maybeToStatus . nonEmpty . catMaybes
    rules = [interpolationsRule, noEmojiRule]
