module Intlc.Compiler.Backend.TypeScriptReact where

import           Control.Monad.Writer
import qualified Data.Text            as T
import           Intlc.Core
import           Prelude

type Compiler = Writer [Arg]

export :: Text -> Message -> Text
export k v = "export const " <> k <> " = " <> msg v <> newline

msg :: Message -> Text
msg (Static x)   = "'" <> x <> "'"
msg (Dynamic xs) = lambda args <> "<>" <> str <> "</>"
  where (str, args) = runWriter $ foldMapM token xs

typedInput :: Maybe ICUType -> Text
typedInput Nothing             = "ReactNode"
typedInput (Just Number)       = "number"
typedInput (Just (Callback _)) = "(x: ReactNode) => ReactNode"

lambda :: [Arg] -> Text
lambda xs = "(x: { " <> typ <> " }) => "
  where typ = T.intercalate "; " (prop <$> xs)
        prop (Arg n mt) = n <> ": " <> typedInput mt

token :: Token -> Compiler Text
token (Plaintext x)               = pure x
token (Interpolation x@(Arg n (Just (Callback xs)))) = do
  tell . pure $ x
  children <- foldMapM token xs
  pure $ "{x." <> n <> "(<>" <> children <> "</>)}"
token (Interpolation x@(Arg n _)) = "{x." <> n <> "}" <$ tell (pure x)

newline :: Text
newline = "\n"
