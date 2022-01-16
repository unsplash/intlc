module Intlc.Compiler.Backend.TypeScript where

import           Control.Monad.Writer
import           Intlc.Compiler.Backend.Common.TypeScript
import           Intlc.Compiler.Common
import           Intlc.ICU
import           Prelude

type Compiler = Writer [Arg]

type TypedArgs = [(Text, Text)]

rootType :: Text
rootType = "string"

export :: Text -> Message -> Either (NonEmpty Text) Text
export k v = do
  (as, r) <- msg v
  pure $ namedExport (k <> ": " <> type' as) r
    where type' [] = rootType
          type' xs = lambdaType xs rootType

msg :: Message -> Either (NonEmpty Text) (TypedArgs, Text)
msg (Static x)   = pure (mempty, str x)
msg (Dynamic xs) = do
  args' <- args <$> minterps
  pure . (args',) $ (fst <$> args') `lambda` templateLits ret
    where (ret, interpsRaw) = runWriter $ foldMapM token xs
          minterps = validateArgs interpsRaw

argType :: Intlc.ICU.Type -> Text
argType = typ rootType

args :: [Arg] -> [(Text, Text)]
args xs = pure (argName, obj (arg <$> xs))
  where arg (Arg n mt) = (n, argType mt)

token :: Token -> Compiler Text
token (Plaintext x)               = pure x
token (Interpolation x@(Arg n t)) = tell (pure x) *> case t of
  String       -> pure std
  Number       -> pure std
  Date fmt     -> pure $ templateInterp (fmtDate fmt n)
  Plural cs w  -> do
    cases <- (<>) <$> foldMapM (fmap (<> " ") . case') cs <*> def w
    pure . templateInterp $ iife "n" (switch "n" cases) (argName `prop` n)
    where case' (PluralCase v rs) = shortSwitchCase v . templateLits <$> foldMapM token rs
          def (PluralWildcard rs) = shortSwitchDefault . templateLits <$> foldMapM token rs
  Select cs mw -> do
    cases <- (<>) <$> foldMapM (fmap (<> " ") . case') cs <*> def mw
    pure . templateInterp $ iife "y" (switch "y" cases) (argName `prop` n)
    where case' (SelectCase v rs) = shortSwitchCase (str v) . templateLits <$> foldMapM token rs
          def (Just (SelectWildcard rs)) = shortSwitchDefault . templateLits <$> foldMapM token rs
          def Nothing                    = pure ""
  Callback xs  -> do
    children <- foldMapM token xs
    pure $ templateInterp (argName `prop` n <> apply (templateLits children))
  where std = templateInterp (argName `prop` n)
