module Intlc.Compiler.Backend.TypeScript where

import           Control.Monad.Writer
import           Intlc.Compiler.Backend.Common.TypeScript
import           Intlc.Core
import           Prelude

type Compiler = Writer [Arg]

export :: Text -> Message -> Text
export k v = namedExport k (msg v)

msg :: Message -> Text
msg (Static x)   = str x
msg (Dynamic xs) = args interps `lambda` templateLits ret
  where (ret, interps) = runWriter $ foldMapM token xs

argType :: ICUType -> Text
argType = typ "string"

args :: [Arg] -> [(Text, Text)]
args xs = pure (argName, obj (arg <$> xs))
  where arg (Arg n mt) = (n, argType mt)

token :: Token -> Compiler Text
token (Plaintext x)               = pure x
token (Interpolation x@(Arg n (Callback xs))) = do
  tell . pure $ x
  children <- foldMapM token xs
  pure $ templateInterp (argName `prop` n <> apply (templateLits children))
token (Interpolation x@(Arg n _)) = templateInterp (argName `prop` n) <$ tell (pure x)
