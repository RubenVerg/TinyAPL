{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module TinyAPL.Adverb where

import TinyAPL.Util
import {-# SOURCE #-} TinyAPL.Context
import {-# SOURCE #-} TinyAPL.Function
import {-# SOURCE #-} TinyAPL.Noun

import Control.DeepSeq

data Adverb

instance Eq Adverb
instance Ord Adverb
instance NFData Adverb
instance (Monad m, MonadShow m ScalarValue) => MonadShow m Adverb

callOnNoun :: Adverb -> ExtraArgs -> Noun -> St Function

callOnFunction :: Adverb -> ExtraArgs -> Function -> St Function