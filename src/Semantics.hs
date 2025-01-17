{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Semantics where

import Expressions
import Control.Monad.Except
import Data.Text.Lazy (Text)
import Control.Monad.Reader
import Data.IORef
import Error

data ScopeInfo = ScopeInfo
    { level :: Int
    , info :: (Text, (Value, Type)) 
    }

type Env = [ScopeInfo]

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
    updateEnv :: ScopeInfo -> m ()
    lookupStack :: Text -> m Proc
    lookupEnv :: Text -> m (Value, Type)

instance MonadStack SemanticsM where
    updateStack name proc = do
        p <- asks procs
        procedures <- liftIO $ readIORef p
        case lookup name procedures of
            Nothing -> liftIO $ modifyIORef' p ((name, proc) :)
            Just _ -> throwError $ RedeclaredProc name 
    updateEnv scopeInfo = do
        e <- asks env
        environment <- liftIO $ readIORef e
        updateEnvIter scopeInfo environment 
      where
        updateEnvIter s [] = do
            e <- asks env
            liftIO $ modifyIORef' e (s:)
        updateEnvIter s@(ScopeInfo lvl1 (var1, (_, _))) ((ScopeInfo lvl2 (var2, (_, _))): scopes) = do
            if lvl1 == lvl2 && var1 == var2 
                then throwError $ RedeclaredVariable var1
                else updateEnvIter s scopes
    lookupStack name = do
        p <- asks procs
        procedures <- liftIO $ readIORef p
        case lookup name procedures of
            Nothing -> throwError $ UndeclaredProc name
            Just proc -> pure proc
    lookupEnv name = do
        e <- asks env
        environment <- liftIO $ readIORef e
        lookupEnvIter name environment
        where
            lookupEnvIter n [] = throwError $ UndeclaredVariable n
            lookupEnvIter n (scope : scopes) = 
                case lookupScope n scope of
                    Nothing -> lookupEnvIter n scopes
                    Just v -> pure v

lookupScope :: Text -> ScopeInfo -> Maybe (Value, Type)
lookupScope var (ScopeInfo _ (n, (val, typ))) = if var == n then Just (val, typ) else Nothing

initStack :: IO Stack
initStack = do
    e <- newIORef []
    ps <- newIORef []
    pure $ Stack ps e

analyzeProgram :: Program -> SemanticsM ()
analyzeProgram prog = analyzeProgramIter prog 0
    where
        analyzeProgramIter [] _ = pure ()
        analyzeProgramIter (p : ps) n = analyzeProc p n >> analyzeProgramIter ps (n+1)

analyzeProc :: Proc -> Int -> SemanticsM ()
analyzeProc p@(Proc pname params locals exprs) lvl = do
    updateStack pname p
    mapM_ (\(v, t) -> updateEnv (ScopeInfo lvl (v, (None, t)))) params
    mapM_ (\(v, t) -> updateEnv (ScopeInfo lvl (v, (None, t)))) locals
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
binaryAnalysis Sub = sub'
binaryAnalysis Mul = mul'
binaryAnalysis Div = div'
binaryAnalysis Mod = mod''
binaryAnalysis Exp = exp'
binaryAnalysis _ = undefined
{-binaryAnalysis And = and'
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

sub' :: Type -> Type -> SemanticsM Type
sub' I64T I64T = pure I64T
sub' I32T I32T = pure I32T
sub' U64T U64T = pure U64T
sub' U32T U32T = pure U32T
sub' F64T F64T = pure F64T
sub' F32T F32T = pure F32T
sub' _ _ = throwError $ GeneralError "- expects number"

mul' :: Type -> Type -> SemanticsM Type
mul' I64T I64T = pure I64T
mul' I32T I32T = pure I32T
mul' U64T U64T = pure U64T
mul' U32T U32T = pure U32T
mul' F64T F64T = pure F64T
mul' F32T F32T = pure F32T
mul' _ _ = throwError $ GeneralError "* expects number"

div' :: Type -> Type -> SemanticsM Type
div' I64T I64T = pure I64T
div' I32T I32T = pure I32T
div' U64T U64T = pure U64T
div' U32T U32T = pure U32T
div' F64T F64T = pure F64T
div' F32T F32T = pure F32T
div' _ _ = throwError $ GeneralError "/ expects number"

exp' :: Type -> Type -> SemanticsM Type
exp' I64T I64T = pure I64T
exp' I32T I32T = pure I32T
exp' U64T U64T = pure U64T
exp' U32T U32T = pure U32T
exp' F64T F64T = pure F64T
exp' F32T F32T = pure F32T
exp' _ _ = throwError $ GeneralError "** expects number"

mod'' :: Type -> Type -> SemanticsM Type
mod'' I64T I64T = pure I64T
mod'' I32T I32T = pure I32T
mod'' U64T U64T = pure U64T
mod'' U32T U32T = pure U32T
mod'' F64T F64T = pure F64T
mod'' _ _ = throwError $ GeneralError "% expects number"

{-
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
