module Intlc.Compiler.Backend.TypeScriptReact where

import           Control.Monad.Writer
import           Intlc.Compiler.Backend.Common.JSX
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
  pure $ args interps `lambda` fragment ret
    where (ret, interpsRaw) = runWriter $ foldMapM token xs
          minterps = validateArgs interpsRaw

argType :: ICUType -> Text
argType = typ "ReactElement"

args :: [Arg] -> [(Text, Text)]
args xs = pure (argName, obj (arg <$> xs))
  where arg (Arg n mt) = (n, argType mt)

token :: Token -> Compiler Text
token (Plaintext x)               = pure x
token (Interpolation x@(Arg n t)) = tell (pure x) *> case t of
  String      -> pure std
  Number      -> pure std
  Date fmt    -> pure $ interpolate (fmtDate fmt n)
  Plural cs w -> do
    cases <- (<>) <$> foldMapM (fmap (<> " ") . case') cs <*> def w
    pure . interpolate $ iife "n" (switch "n" cases) (argName `prop` n)
    where case' (PluralCase v rs) = shortSwitchCase v . fragment <$> foldMapM token rs
          def (PluralWildcard rs) = shortSwitchDefault . fragment <$> foldMapM token rs
  Callback xs -> do
    children <- foldMapM token xs
    pure . interpolate $ argName `prop` n <> apply (fragment children)
  where std = interpolate (argName `prop` n)
