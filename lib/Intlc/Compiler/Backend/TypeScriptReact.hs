module Intlc.Compiler.Backend.TypeScriptReact where

import           Control.Monad.Writer
import           Intlc.Compiler.Backend.Common.JSX
import           Intlc.Compiler.Backend.Common.TypeScript
import           Intlc.Core
import           Prelude

type Compiler = Writer [Arg]

export :: Text -> Message -> Text
export k v = namedExport k (msg v)

msg :: Message -> Text
msg (Static x)   = str x
msg (Dynamic xs) = args interps `lambda` fragment ret
  where (ret, interps) = runWriter $ foldMapM token xs

argType :: Maybe ICUType -> Text
argType = typ "ReactElement"

args :: [Arg] -> [(Text, Text)]
args xs = pure (argName, obj (arg <$> xs))
  where arg (Arg n mt) = (n, argType mt)

token :: Token -> Compiler Text
token (Plaintext x)                                  = pure x
token (Interpolation x@(Arg n (Just (Callback xs)))) = do
  tell . pure $ x
  children <- foldMapM token xs
  pure . interpolate $ argName `prop` n <> apply (fragment children)
token (Interpolation x@(Arg n _))                    = interpolate (argName `prop` n) <$ tell (pure x)
