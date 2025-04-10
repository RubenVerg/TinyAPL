{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module TinyAPL.Function where

import TinyAPL.Util
import {-# SOURCE #-} TinyAPL.Noun

import Control.DeepSeq

type ExtraArgs = [(ScalarValue, ScalarValue)]

data Function

instance Eq Function
instance Ord Function
instance NFData Function
instance (Monad m, MonadShow m ScalarValue) => MonadShow m Function