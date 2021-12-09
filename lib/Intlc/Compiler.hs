module Intlc.Compiler where

import           Control.Monad.Writer
import qualified Data.Map             as M
import qualified Data.Text            as T
import           Intlc.Core
import           Prelude

type Compiler = Writer [Arg]

dataset :: Dataset Translation -> Text
dataset xs = "export default {" <> newline <> exports <> newline <> "}"
  where exports = T.intercalate newline . M.foldlWithKey export mempty $ xs
        export acc k v = indent <> k <> ": " <> translation v <> "," : acc

translation :: Translation -> Text
translation (Static x)   = "'" <> x <> "'"
translation (Dynamic xs) = lambda args <> "`" <> str <> "`"
  where (str, args) = runWriter $ foldMapM token xs

lambda :: [Arg] -> Text
lambda xs = "({ " <> val <> " }: { " <> typ <> " }) => "
  where val = T.intercalate ", " xs'
        typ = T.intercalate "; " (xs' <&> (<> ": string"))
        xs' = coerce xs

token :: Token -> Compiler Text
token (Plaintext x)     = pure x
token (Interpolation x) = "${" <> coerce x <> "}" <$ tell (pure x)

indent :: Text
indent = "  "

newline :: Text
newline = "\n"
