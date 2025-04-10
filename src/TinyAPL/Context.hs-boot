{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module TinyAPL.Context where

import TinyAPL.Error
import TinyAPL.Util
import {-# SOURCE #-} TinyAPL.Noun

import Control.DeepSeq
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.IORef

data Scope

scopeLookupNoun :: Bool -> String -> Scope -> St (Maybe Noun)

data Context

instance NFData Context

contextScope :: Context -> IORef Scope

type St = StateT Context (ExceptT Error IO)

instance MonadShow St ScalarValue

runWithContext :: Context -> St a -> St a

getContext :: St Context

getsContext :: (Context -> a) -> St a

putContext :: Context -> St ()

putScope :: IORef Scope -> St ()

createRef :: a -> St (IORef a)

readRef :: IORef a -> St a

writeRef :: IORef a -> a -> St ()

modifyRef :: IORef a -> (a -> a) -> St ()
