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

data Stack = Stack
    { scopeLevel :: IORef Int
    , procs :: IORef [(Text, Proc)]
    , env :: IORef Env
    }

newtype EvalM a = EvalM {unEval :: ExceptT EvalError (ReaderT Stack IO) a}
    deriving (Functor, Applicative, Monad, MonadReader Stack, MonadIO, MonadError EvalError)
