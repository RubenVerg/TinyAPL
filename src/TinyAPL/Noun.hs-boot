{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module TinyAPL.Noun where

import TinyAPL.Util

import Control.DeepSeq

data ScalarValue

data Noun

instance Eq Noun
instance Ord Noun
instance NFData Noun
instance (Monad m, MonadShow m ScalarValue) => MonadShow m Noun
