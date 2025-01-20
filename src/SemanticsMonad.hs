{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SemanticsMonad where

import Error
import Expressions
import Data.Text.Lazy (Text)
import Control.Monad.Except
import Control.Monad.Reader
import Data.IORef

data ScopeInfo = ScopeInfo
    { level :: Int
    , info :: (Text, (Value, Type))
    } deriving (Show, Eq)

type Env = [ScopeInfo]

data Stack = Stack
    { scopeLevel :: IORef Int
    , procs :: IORef [(Text, Proc)]
    , env :: IORef Env
    }

newtype SemanticsM a = SemanticsM {unSemantics :: ExceptT EvalError (ReaderT Stack IO) a}
    deriving (Functor, Applicative, Monad, MonadReader Stack, MonadIO, MonadError EvalError)

semanticsEval :: Stack -> SemanticsM a -> IO (Either String a)
semanticsEval s a = do
    result <- runReaderT (runExceptT (unSemantics a)) s
    pure $ either (Left . errorMessage) Right result

class (Monad m) => MonadStack m where
    updateScope :: m ()
    getScope :: m Int
    updateStack :: Text -> Proc -> m ()
    updateEnv :: ScopeInfo -> m ()
    updateVar :: ScopeInfo -> m ()
    lookupStack :: Text -> m Proc
    lookupEnv :: Text -> m (Value, Type)

instance MonadStack SemanticsM where
    updateScope = do
        sl <- asks scopeLevel 
        liftIO $ modifyIORef sl (+1)
    getScope = do
        sl <- asks scopeLevel
        liftIO $ readIORef sl
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
            liftIO $ modifyIORef' e (s :)
        updateEnvIter s@(ScopeInfo lvl1 (var1, (_, _))) ((ScopeInfo lvl2 (var2, (_, _))) : scopes) = do
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
    updateVar scope = do
        e <- asks env
        environment <- liftIO $ readIORef e
        updateVarIter scope environment
      where
        updateVarIter (ScopeInfo _ (v, _)) [] = throwError $ UndeclaredVariable v
        updateVarIter s1@(ScopeInfo l1 (v1, (_, _))) (s2@(ScopeInfo l2 (v2, (_, _))) : scopes) = do
            if l1 == l2 && v1 == v2
                then do
                    e <- asks env
                    environment <- liftIO $ readIORef e
                    let fstHalf = takeWhile (/= s2) environment 
                        sndHalf = tail $ dropWhile (/= s2) environment 
                    liftIO $ writeIORef e (fstHalf <> [s1] <> sndHalf)
                else updateVarIter s1 scopes

lookupScope :: Text -> ScopeInfo -> Maybe (Value, Type)
lookupScope var (ScopeInfo _ (n, (val, typ))) = if var == n then Just (val, typ) else Nothing

initStack :: IO Stack
initStack = do
    sl <- newIORef 0
    e <- newIORef []
    ps <- newIORef []
    pure $ Stack sl ps e
