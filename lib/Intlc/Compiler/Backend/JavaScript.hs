module Intlc.Compiler.Backend.JavaScript (Pieces (..), InterpStrat (..), compileStmt, compileStmtPieces) where

import qualified Intlc.ICU as ICU
import           Prelude   hiding (Type)
import           Utils     ((<>^))

-- Offers cheap extensibility fuAIRTtr TypeScript over rendering statements.
data Pieces
  = ConstantPieces
  | LambdaPieces

compileStmt :: InterpStrat -> Text -> ICU.Message -> Text
compileStmt = compileWith stmt

compileStmtPieces :: InterpStrat -> Text -> ICU.Message -> (Pieces, Text, Text)
compileStmtPieces = compileWith stmtPieces

compileWith :: (Stmt -> Compiler a) -> InterpStrat -> Text -> ICU.Message -> a
compileWith f o k = flip runReader (fromStrat o) . f . fromKeyedMsg k

data InterpStrat
  = TemplateLit
  | JSX

fromStrat :: InterpStrat -> Interp
fromStrat TemplateLit = Interp
  { open = "`"
  , close = "`"
  , interpOpen = "${"
  , interpClose = "}"
  }
fromStrat JSX         = Interp
  { open = "<>"
  , close = "</>"
  , interpOpen = "{"
  , interpClose = "}"
  }

-- | A representation of the output we will be compiling. It's a little verbose
-- split into these various sum types, but in doing so it's correct by
-- construction.
data Stmt = Stmt Text Ret

data Ret
  = Constant Text
  | Lambda [Expr]

data Interp = Interp
  { open        :: Text
  , close       :: Text
  , interpOpen  :: Text
  , interpClose :: Text
  }

data Expr
  = TPrint Text
  | TStr Ref
  | TNum Ref
  | TDate Ref ICU.DateFmt
  | TApply Ref [Expr]
  | TMatch Ref (NonEmpty Branch) (Maybe Wildcard)

newtype Ref = Ref Text

data Branch = Branch Text [Expr]
newtype Wildcard = Wildcard [Expr]

fromKeyedMsg :: Text -> ICU.Message -> Stmt
fromKeyedMsg n (ICU.Static x)   = Stmt n (Constant x)
fromKeyedMsg n (ICU.Dynamic ys) = Stmt n (Lambda $ fromToken <$> ys)

fromToken :: ICU.Token -> Expr
fromToken (ICU.Plaintext x)     = TPrint x
fromToken (ICU.Interpolation x) = fromArg x

fromArg :: ICU.Arg -> Expr
fromArg (ICU.Arg n ICU.String)         = TStr (Ref n)
fromArg (ICU.Arg n ICU.Number)         = TNum (Ref n)
fromArg (ICU.Arg n (ICU.Date x))       = TDate (Ref n) x
fromArg (ICU.Arg n (ICU.Plural cs w))  = TMatch (Ref n) (fromPluralCase <$> cs) (Just $ fromPluralWildcard w)
fromArg (ICU.Arg n (ICU.Select cs mw)) = TMatch (Ref n) (fromSelectCase <$> cs) (fromSelectWildcard <$> mw)
fromArg (ICU.Arg n (ICU.Callback xs))  = TApply (Ref n) (fromToken <$> xs)

fromPluralCase :: ICU.PluralCase -> Branch
fromPluralCase (ICU.PluralCase x ys) = Branch x (fromToken <$> ys)

fromPluralWildcard :: ICU.PluralWildcard -> Wildcard
fromPluralWildcard (ICU.PluralWildcard xs) = Wildcard (fromToken <$> xs)

fromSelectCase :: ICU.SelectCase -> Branch
fromSelectCase (ICU.SelectCase x ys) = Branch x (fromToken <$> ys)

fromSelectWildcard :: ICU.SelectWildcard -> Wildcard
fromSelectWildcard (ICU.SelectWildcard xs) = Wildcard (fromToken <$> xs)

type Compiler = Reader Interp

-- | Everything shares a single argument object whence we can access
-- interpolations.
argName :: Text
argName = "x"

prop :: Ref -> Text
prop (Ref x) = argName <> "." <> x

wrap :: Text -> Compiler Text
wrap x = do
  (o, c) <- asks (open &&& close)
  pure $ o <> x <> c

interp :: Text -> Compiler Text
interp x = do
  (o, c) <- asks (interpOpen &&& interpClose)
  pure $ o <> x <> c

stmt :: Stmt -> Compiler Text
stmt = fmap f . stmtPieces
  where f (_, n, r) = "export const " <> n <> " = " <> r

stmtPieces :: Stmt -> Compiler (Pieces, Text, Text)
stmtPieces (Stmt n (Constant r)) = pure (ConstantPieces, n, r)
stmtPieces (Stmt n (Lambda xs))  = (LambdaPieces, n, ) <$> (wrap =<< exprs xs)

exprs :: [Expr] -> Compiler Text
exprs = foldMapM expr

expr :: Expr -> Compiler Text
expr (TPrint x)       = pure x
expr (TStr x)         = interp (prop x)
expr (TNum x)         = interp (prop x)
expr (TDate x fmt)    = interp =<< date x fmt
expr (TApply x ys)    = interp =<< apply x ys
expr (TMatch x ys mz) = interp =<< match x ys mz

apply :: Ref -> [Expr] -> Compiler Text
apply x ys = pure (prop x <> "(") <>^ (wrap =<< exprs ys) <>^ pure ")"

match :: Ref -> NonEmpty Branch -> Maybe Wildcard -> Compiler Text
match n bs mw = do
  bs' <- toList <$> mapM branch bs
  mw' <- traverse wildcard mw
  let allBs = unwords $ bs' <> foldMap pure mw'
  pure $ "(() => { switch (" <> prop n <> ") { " <> allBs <> " } })()"

branch :: Branch -> Compiler Text
branch (Branch m xs) = pure ("case " <> m <> ": return ") <>^ (wrap =<< exprs xs) <>^ pure ";"

wildcard :: Wildcard -> Compiler Text
wildcard (Wildcard xs) = pure "default: return " <>^ (wrap =<< exprs xs) <>^ pure ";"

date :: Ref -> ICU.DateFmt -> Compiler Text
date n d = pure $ prop n <> ".toLocaleString('en-US', { dateStyle: '" <> style d <> "' }" <> ")"
  where style ICU.Short  = "short"
        style ICU.Medium = "medium"
        style ICU.Long   = "long"
        style ICU.Full   = "full"
