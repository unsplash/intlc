module Intlc.Linter where

import qualified Data.Text as T

import           Intlc.ICU
import           Prelude   hiding (Type)



data LintingError
  = TooManyInterpolations
  | InvalidNonAsciiCharacter (NonEmpty Char)
  deriving (Eq,Show)


data Status
  = Success
  | Failure (NonEmpty LintingError)
  deriving (Eq,Show)


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


-- | Selects the first 128 characters of the Unicode character set,
-- corresponding to the ASCII character set.
-- and the special cases
acceptedChars :: String
acceptedChars = ['’','…','é','—','ƒ']

isAcceptedChar                 :: Char -> Bool
isAcceptedChar c               =  c <  '\x80' || c `elem` acceptedChars



noAsciiRule :: Rule
noAsciiRule = output . nonAscii where
  output = fmap InvalidNonAsciiCharacter . nonEmpty . T.unpack
  nonAscii :: Stream -> Text
  nonAscii []                      = mempty
  nonAscii (Plaintext x:ys)        = T.filter (not . isAcceptedChar) x <> nonAscii ys
  nonAscii (x@Interpolation {}:ys) = nonAscii (maybeToMonoid . getStream $ x) <> nonAscii ys

lintWithRules :: [Rule] -> Message -> Status
lintWithRules rules (Message stream) = toStatus $ rules `flap` stream
  where
    toStatus = maybeToStatus . nonEmpty . catMaybes

lint :: Message -> Status
lint = lintWithRules [interpolationsRule,noAsciiRule]

printLintingError :: LintingError -> IO ()
printLintingError TooManyInterpolations           = putStrLn "Nested functions are not allowed"
printLintingError (InvalidNonAsciiCharacter chars) = putStr "Following characters are not allowed: " >> printChars chars
  where
    printChars:: NonEmpty Char -> IO()
    printChars = putStrLn . intercalate "   " . toList . fmap return
