{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FunctionalDependencies #-}
module TinyAPL.Pretty where

import Control.DeepSeq
import Control.Monad.Reader

data Doc

class Monad m => PrettyPrint m s a | a -> s where
  prettyM :: a -> ReaderT s m Doc

runPretty :: (Monad m, PrettyPrint m s a) => s -> a -> m String

data PrettyConfig
instance NFData PrettyConfig
