module Intlc.Linter where
import           Data.Char (isAscii)
import qualified Data.Text as T

import           Intlc.ICU
import           Prelude   hiding (Type)


import           Text.Show
data LintingError
  = TooManyInterpolations
  | InvalidNonAsciiCharacter (NonEmpty Char)
  deriving (Eq)

instance Show LintingError where
  show TooManyInterpolations = "Nested functions not allowed"
  show (InvalidNonAsciiCharacter chars)   =  "These non-ascii characters are not allowed: " <> Prelude.show (toList chars)



data Status
  = Success
  | Failure (NonEmpty LintingError)
  deriving (Eq)


instance Show Status where
  show Success          = "Success"
  show (Failure errors) = "Failed to Lint because of the following reasons:\n" <> T.unpack (a errors)
    where
      a:: NonEmpty LintingError -> Text
      a e =  T.unlines $ toList (("  " <>) . Prelude.show <$> e)



type Rule = Stream -> Maybe LintingError

statusToMaybe :: Status -> Maybe (NonEmpty LintingError)
statusToMaybe Success      = Nothing
statusToMaybe (Failure xs) = Just xs

maybeToStatus :: Maybe (NonEmpty LintingError) -> Status
maybeToStatus Nothing   = Success
maybeToStatus (Just xs) = Failure xs

interpolationsRule :: Rule
interpolationsRule = go 0
  where
    go :: Int -> Rule
    go 2 _ = Just TooManyInterpolations
    go _ [] = Nothing
    go n (x : xs) = go n' $ maybeToMonoid mys <> xs
      where
        mys = getStream x
        n' = n + length mys

noAsciiRule :: Rule
noAsciiRule [] = Nothing
noAsciiRule (x : xs) =  let lintingErrors = noAsciiRule $ maybeToMonoid mys <> xs
                        in case (getAscii x, lintingErrors) of
                          (Just asciiChars,Just (InvalidNonAsciiCharacter char)) ->  Just $ InvalidNonAsciiCharacter $ asciiChars <> char
                          (Just asciiChars,Nothing) ->  Just $ InvalidNonAsciiCharacter asciiChars
                          (_,_)         -> lintingErrors
                          where
                            getAscii :: Token -> Maybe (NonEmpty Char)
                            getAscii token
                              | Plaintext t <- token = getAsciiChars t
                              | Interpolation t _ <- token  = getAsciiChars t
                              where
                                  getAsciiChars :: Text -> Maybe (NonEmpty Char)
                                  getAsciiChars t = let (_, asciiChars) = T.partition isAscii t in
                                                    nonEmpty . T.unpack $ asciiChars
                            mys = getStream x

lintWithRules :: [Rule] -> Message -> Status
lintWithRules rules (Message stream) = toStatus $ rules `flap` stream
  where
    toStatus = maybeToStatus . nonEmpty . catMaybes

lint :: Message -> Status
lint message = lintWithRules [interpolationsRule,noAsciiRule] message
