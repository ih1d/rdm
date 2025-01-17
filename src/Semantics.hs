{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Semantics where

import Expressions
import Control.Monad.Except
import Data.Text.Lazy (Text)
import Control.Monad.Reader
import Data.IORef
import Error

type Env = [(Text, (Value, Type))]

data Stack = Stack
    { procs :: IORef [(Text, Proc)]
    , env :: IORef Env
    }

newtype SemanticsM a = SemanticsM {unSemantics :: ExceptT EvalError (ReaderT Stack IO) a}
    deriving (Functor, Applicative, Monad, MonadReader Stack, MonadIO, MonadError EvalError)

semanticsEval :: Stack -> SemanticsM a -> IO (Either String a)
semanticsEval s a = do
    result <- runReaderT (runExceptT (unSemantics a)) s
    pure $ either (Left . errorMessage) Right result

class (Monad m) => MonadStack m where
    updateStack :: Text -> Proc -> m ()
    updateEnv :: Text -> Value -> Type -> m ()
    lookupStack :: Text -> m Proc
    lookupEnv :: Text -> m (Value, Type)

instance MonadStack SemanticsM where
    updateStack name proc = do
        p <- asks procs
        liftIO $ modifyIORef' p ((name, proc) :)
    updateEnv name val typ = do
        e <- asks env
        environment <- liftIO $ readIORef e
        case lookup name environment of
            Just _ -> throwError $ RedeclaredVariable name
            Nothing -> liftIO $ modifyIORef' e ((name, (val, typ)) :)
    lookupStack name = do
        p <- asks procs
        procedures <- liftIO $ readIORef p
        case lookup name procedures of
            Nothing -> throwError $ UndeclaredProc name
            Just proc -> pure proc
    lookupEnv name = do
        e <- asks env
        environment <- liftIO $ readIORef e
        case lookup name environment of
            Nothing -> throwError $ UndeclaredVariable name
            Just v -> pure v

initStack :: IO Stack
initStack = do
    e <- newIORef []
    ps <- newIORef []
    pure $ Stack ps e

analyzeProgram :: Program -> SemanticsM ()
analyzeProgram [] = pure ()
analyzeProgram (p : ps) = analyzeProc p >> analyzeProgram ps

analyzeProc :: Proc -> SemanticsM ()
analyzeProc p@(Proc pname params locals exprs) = do
    updateStack pname p
    mapM_ (\(v, t) -> updateEnv v None t) params
    mapM_ (\(v, t) -> updateEnv v None t) locals
    mapM_ analyzeExpr exprs

analyzeExpr :: Expr -> SemanticsM ()
analyzeExpr (I64E _) = pure ()
analyzeExpr (I32E _) = pure ()
analyzeExpr (F64E _) = pure ()
analyzeExpr (F32E _) = pure ()
analyzeExpr (U64E _) = pure ()
analyzeExpr (U32E _) = pure ()
analyzeExpr (BoolE _) = pure ()
analyzeExpr (StrE _) = pure ()
analyzeExpr (AppE name exprs) = do
    _ <- lookupStack name
    mapM_ analyzeExpr exprs
analyzeExpr (IdE var) = do
    _ <- lookupEnv var
    pure ()
analyzeExpr (BinOpE op e1 e2) = do
    t1 <- getType e1
    t2 <- getType e2
    _ <- binaryAnalysis op t1 t2
    pure ()
analyzeExpr _ = undefined

getType :: Expr -> SemanticsM Type
getType (IdE n) = snd <$> lookupEnv n
getType (I64E _) = pure I64T
getType (I32E _) = pure I32T
getType (U64E _) = pure U64T
getType (U32E _) = pure U32T
getType (F64E _) = pure F64T
getType (CharE _) = pure CharT
getType (StrE _) = pure StrT
getType (BoolE _) = pure BoolT
getType (F32E _) = pure F32T
getType (BinOpE op e1 e2) = do
    t1 <- getType e1
    t2 <- getType e2
    binaryAnalysis op t1 t2
getType _ = undefined
    

-- pa despues
-- valueOf :: Expr -> Value
-- valueOf (I64E x) = (I64E
binaryAnalysis :: BinOp -> (Type -> Type -> SemanticsM Type)
binaryAnalysis Add = add'
binaryAnalysis _ = undefined
{-binaryAnalysis Sub = sub'
binaryAnalysis Mul = mul'
binaryAnalysis Div = div'
binaryAnalysis Mod = mod''
binaryAnalysis Exp = exp'
binaryAnalysis And = and'
binaryAnalysis Or = or'
binaryAnalysis Gt = gt'
binaryAnalysis GtEq = gte'
binaryAnalysis Lt = lt'
binaryAnalysis LtEq = lte'
binaryAnalysis Equal = eq'
binaryAnalysis NotEqual = neq'
-}

add' :: Type -> Type -> SemanticsM Type
add' I64T I64T = pure I64T
add' I32T I32T = pure I32T
add' U64T U64T = pure U64T
add' U32T U32T = pure U32T
add' F64T F64T = pure F64T
add' F32T F32T = pure F32T
add' _ _ = throwError $ GeneralError "+ expects number"

{-
sub' :: Expr -> Expr -> SemanticsM ()
sub' (I64E _) (I64E _) = pure ()
sub' (I32E _) (I32E _) = pure ()
sub' (U64E _) (U64E _) = pure ()
sub' (U32E _) (U32E _) = pure ()
sub' (F64E _) (F64E _) = pure ()
sub' (F32E _) (F32E _) = pure ()
sub' _ _ = throwError $ GeneralError "- expects number"

mul' :: Expr -> Expr -> SemanticsM ()
mul' (I64E _) (I64E _) = pure ()
mul' (I32E _) (I32E _) = pure ()
mul' (U64E _) (U64E _) = pure ()
mul' (U32E _) (U32E _) = pure ()
mul' (F64E _) (F64E _) = pure ()
mul' (F32E _) (F32E _) = pure ()
mul' _ _ = throwError $ GeneralError "* expects number"

div' :: Expr -> Expr -> SemanticsM ()
div' (I64E _) (I64E _) = pure ()
div' (I32E _) (I32E _) = pure ()
div' (U64E _) (U64E _) = pure ()
div' (U32E _) (U32E _) = pure ()
div' (F64E _) (F64E _) = pure ()
div' (F32E _) (F32E _) = pure ()
div' _ _ = throwError $ GeneralError "/ expects number"

exp' :: Expr -> Expr -> SemanticsM ()
exp' (I64E _) (I64E _) = pure ()
exp' (I32E _) (I32E _) = pure ()
exp' (U64E _) (U64E _) = pure ()
exp' (U32E _) (U32E _) = pure ()
exp' (F64E _) (F64E _) = pure ()
exp' (F32E _) (F32E _) = pure ()
exp' _ _ = throwError $ GeneralError "% expects number"

mod'' :: Expr -> Expr -> SemanticsM ()
mod'' (I64E _) (I64E _) = pure ()
mod'' (I32E _) (I32E _) = pure ()
mod'' (U64E _) (U64E _) = pure ()
mod'' (U32E _) (U32E _) = pure ()
mod'' (F64E _) (F64E _) = pure ()
mod'' (F32E _) (F32E _) = pure ()
mod'' _ _ = throwError $ GeneralError "% expects number"

and' :: Expr -> Expr -> SemanticsM ()
and' (BoolE _) (BoolE _) = pure ()
and' _ _ = throwError $ GeneralError "/\\ expects boolean"

or' :: Expr -> Expr -> SemanticsM ()
or' (BoolE _) (BoolE _) = pure ()
or' _ _ = throwError $ GeneralError "\\/ expects boolean"

xor' :: Expr -> Expr -> SemanticsM ()
xor' (BoolE _) (BoolE _) = pure ()
xor' _ _ = throwError $ GeneralError "^ expects boolean"

gt' :: Expr -> Expr -> SemanticsM ()
gt' (I64E _) (I64E _) = pure ()
gt' (I32E _) (I32E _) = pure ()
gt' (U64E _) (U64E _) = pure ()
gt' (U32E _) (U32E _) = pure ()
gt' (F64E _) (F64E _) = pure ()
gt' (F32E _) (F32E _) = pure ()
gt' _ _ = throwError $ GeneralError "> expects number"

gte' :: Expr -> Expr -> SemanticsM ()
gte' (I64E _) (I64E _) = pure ()
gte' (I32E _) (I32E _) = pure ()
gte' (U64E _) (U64E _) = pure ()
gte' (U32E _) (U32E _) = pure ()
gte' (F64E _) (F64E _) = pure ()
gte' (F32E _) (F32E _) = pure ()
gte' _ _ = throwError $ GeneralError ">= expects number"

lt' :: Expr -> Expr -> SemanticsM ()
lt' (I64E _) (I64E _) = pure ()
lt' (I32E _) (I32E _) = pure ()
lt' (U64E _) (U64E _) = pure ()
lt' (U32E _) (U32E _) = pure ()
lt' (F64E _) (F64E _) = pure ()
lt' (F32E _) (F32E _) = pure ()
lt' _ _ = throwError $ GeneralError "< expects number"

lte' :: Expr -> Expr -> SemanticsM ()
lte' (I64E _) (I64E _) = pure ()
lte' (I32E _) (I32E _) = pure ()
lte' (U64E _) (U64E _) = pure ()
lte' (U32E _) (U32E _) = pure ()
lte' (F64E _) (F64E _) = pure ()
lte' (F32E _) (F32E _) = pure ()
lte' _ _ = throwError $ GeneralError "<= expects number"

eq' :: Expr -> Expr -> SemanticsM ()
eq' (I64E _) (I64E _) = pure ()
eq' (I32E _) (I32E _) = pure ()
eq' (U64E _) (U64E _) = pure ()
eq' (U32E _) (U32E _) = pure ()
eq' (F64E _) (F64E _) = pure ()
eq' (F32E _) (F32E _) = pure ()
eq' (BoolE _) (BoolE _) = pure ()
eq' (StrE _) (StrE _) = pure ()
eq' _ _ = throwError $ GeneralError "= expects number"

neq' :: Type -> Type -> SemanticsM ()
neq' I64T I64T = pure ()
neq' (I32E _) (I32E _) = pure ()
neq' (U64E _) (U64E _) = pure ()
neq' (U32E _) (U32E _) = pure ()
neq' (F64E _) (F64E _) = pure ()
neq' (F32E _) (F32E _) = pure ()
neq' (BoolE _) (BoolE _) = pure ()
neq' (StrE _) (StrE _) = pure ()
neq' _ _ = throwError $ GeneralError "!= types do not match"
-}
