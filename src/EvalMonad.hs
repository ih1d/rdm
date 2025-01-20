{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module EvalMonad where

import Control.Monad.Except
import Control.Monad.Reader
import Data.IORef
import Data.Text.Lazy (Text)
import Error
import Expressions

type Env = [(String, Value)]

type Heap = IORef [Value]

data Stack = Stack
    { scopeLevel :: IORef Int
    , procs :: IORef [(Text, Proc)]
    , env :: IORef Env
    , heap :: Heap
    }

newtype EvalM a = EvalM {unEval :: ExceptT EvalError (ReaderT Stack IO) a}
    deriving (Functor, Applicative, Monad, MonadReader Stack, MonadIO, MonadError EvalError)

class (Monad m) => EvalMonad m where
    lookupEnv :: Text -> m Value
    updateEnv :: Text -> m ()
    lookupStack :: Text -> m Proc
    updateStack :: Proc -> m ()
    getScope :: m Int
