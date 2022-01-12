module Intlc.Compiler where

import           Control.Monad.Writer
import qualified Data.Map             as M
import qualified Data.Text            as T
import           Intlc.Core
import           Prelude

type Compiler = Writer [Arg]

dataset :: Dataset Translation -> Text
dataset = M.foldMapWithKey export
  where export k v = "export const " <> k <> " = " <> translation v <> newline

translation :: Translation -> Text
translation (Static x)   = "'" <> x <> "'"
translation (Dynamic xs) = lambda args <> "`" <> str <> "`"
  where (str, args) = runWriter $ foldMapM token xs

typedInput :: Maybe ICUType -> Text
typedInput Nothing             = "string"
typedInput (Just Number)       = "number"
typedInput (Just (Callback _)) = "(x: string) => string"

lambda :: [Arg] -> Text
lambda xs = "(x: { " <> typ <> " }) => "
  where typ = T.intercalate "; " (prop <$> xs)
        prop (Arg n mt) = n <> ": " <> typedInput mt

token :: Token -> Compiler Text
token (Plaintext x)               = pure x
token (Interpolation x@(Arg n (Just (Callback xs)))) = do
  tell . pure $ x
  children <- foldMapM token xs
  pure $ "${x." <> n <> "(`" <> children <> "`)}"
token (Interpolation x@(Arg n _)) = "${x." <> n <> "}" <$ tell (pure x)

newline :: Text
newline = "\n"
