{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module TinyAPL.Conjunction where

import TinyAPL.Util
import {-# SOURCE #-} TinyAPL.Context
import {-# SOURCE #-} TinyAPL.Function
import {-# SOURCE #-} TinyAPL.Noun

import Control.DeepSeq

data Conjunction

instance Eq Conjunction
instance Ord Conjunction
instance NFData Conjunction
instance (Monad m, MonadShow m ScalarValue) => MonadShow m Conjunction

callOnNounAndNoun :: Conjunction -> ExtraArgs -> Noun -> Noun -> St Function

callOnNounAndFunction :: Conjunction -> ExtraArgs -> Noun -> Function -> St Function

callOnFunctionAndNoun :: Conjunction -> ExtraArgs -> Function -> Noun -> St Function

callOnFunctionAndFunction :: Conjunction -> ExtraArgs -> Function -> Function -> St Function