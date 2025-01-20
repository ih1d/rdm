{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module EvalMonad where

import Control.Monad.Except
import Control.Monad.Reader
import Data.IORef
import Data.Text.Lazy (Text)
import Error
import Expressions

type Env = [(Text, Value)]

type Heap = [Value]

data Stack = Stack
    { scopeLevel :: IORef Int
    , procs :: IORef [(Text, Proc)]
    , env :: IORef Env
    , heap :: IORef Heap
    }

newtype EvalM a = EvalM {unEval :: ExceptT EvalError (ReaderT Stack IO) a}
    deriving (Functor, Applicative, Monad, MonadReader Stack, MonadIO, MonadError EvalError)

runEval :: Stack -> EvalM a -> IO (Either String a)
runEval s a = do
    result <- runReaderT (runExceptT (unEval a)) s
    pure $ either (Left . errorMessage) Right result

class (Monad m) => EvalMonad m where
    lookupEnv :: Text -> m Value
    updateEnv :: (Text, Value) -> m ()
    getScope :: m Int
    updateHeap :: Value -> m ()
    getHeap :: m Heap

instance EvalMonad EvalM where
    getScope = do
        sl <- asks scopeLevel
        liftIO $ readIORef sl
    getHeap = do
        h <- asks heap
        liftIO $ readIORef h
    updateHeap v = do
        h <- asks heap
        liftIO $ modifyIORef h (++ [v])
    lookupEnv var = do
        e <- asks env
        environment <- liftIO $ readIORef e
        case lookup var environment of
            Nothing -> throwError $ UndeclaredVariable var
            Just val -> pure val
