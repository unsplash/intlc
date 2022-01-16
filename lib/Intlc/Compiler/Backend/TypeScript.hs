module Intlc.Compiler.Backend.TypeScript where

import           Control.Monad.Writer
import           Intlc.Compiler.Backend.Common.TypeScript
import           Intlc.Compiler.Common
import           Intlc.Core
import           Prelude

type Compiler = Writer [Arg]

export :: Text -> Message -> Either (NonEmpty Text) Text
export k v = namedExport k <$> msg v

msg :: Message -> Either (NonEmpty Text) Text
msg (Static x)   = pure $ str x
msg (Dynamic xs) = do
  interps <- minterps
  pure $ args interps `lambda` templateLits ret
    where (ret, interpsRaw) = runWriter $ foldMapM token xs
          minterps = validateArgs interpsRaw

argType :: ICUType -> Text
argType = typ "string"

args :: [Arg] -> [(Text, Text)]
args xs = pure (argName, obj (arg <$> xs))
  where arg (Arg n mt) = (n, argType mt)

token :: Token -> Compiler Text
token (Plaintext x)                           = pure x
token (Interpolation x@(Arg n (Plural cs w))) = do
  tell . pure $ x
  cases <- (<>) <$> foldMapM (fmap (<> " ") . case') cs <*> def w
  pure . templateInterp $ iife "n" (switch "n" cases) (argName `prop` n)
    where case' (PluralCase v rs) = shortSwitchCase v . templateLits <$> foldMapM token rs
          def (PluralWildcard rs) = shortSwitchDefault . templateLits <$> foldMapM token rs
token (Interpolation x@(Arg n (Callback xs))) = do
  tell . pure $ x
  children <- foldMapM token xs
  pure $ templateInterp (argName `prop` n <> apply (templateLits children))
token (Interpolation x@(Arg n _))             = templateInterp (argName `prop` n) <$ tell (pure x)
