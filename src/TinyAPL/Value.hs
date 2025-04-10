{-# LANGUAGE DeriveGeneric, DeriveAnyClass, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module TinyAPL.Value
  ( Value(..)
  , unwrapNoun
  , unwrapFunction
  , unwrapAdverb
  , unwrapConjunction
  , callOnValue
  , callOnValueAndValue ) where

import TinyAPL.Error
import TinyAPL.Util
import {-# SOURCE #-} TinyAPL.Adverb
import {-# SOURCE #-} TinyAPL.Conjunction
import {-# SOURCE #-} TinyAPL.Context
import {-# SOURCE #-} TinyAPL.Function
import {-# SOURCE #-} TinyAPL.Noun

import GHC.Generics
import Control.DeepSeq

data Value
  = VNoun Noun
  | VFunction Function
  | VAdverb Adverb
  | VConjunction Conjunction
  deriving (Eq, Ord, Generic, NFData)

instance (Monad m, MonadShow m ScalarValue) => MonadShow m Value where
  showM (VNoun n) = showM n
  showM (VFunction fn) = showM fn
  showM (VAdverb adv) = showM adv
  showM (VConjunction conj) = showM conj

unwrapNoun :: Error -> Value -> St Noun
unwrapNoun _ (VNoun val) = return val
unwrapNoun e _           = throwError e

unwrapFunction :: Error -> Value -> St Function
unwrapFunction _ (VFunction val) = return val
unwrapFunction e _               = throwError e

unwrapAdverb :: Error -> Value -> St Adverb
unwrapAdverb _ (VAdverb val) = return val
unwrapAdverb e _             = throwError e

unwrapConjunction :: Error -> Value -> St Conjunction
unwrapConjunction _ (VConjunction val) = return val
unwrapConjunction e _                  = throwError e

callOnValue :: Adverb -> ExtraArgs -> Value -> St Function
callOnValue adv ea (VNoun x) = callOnNoun adv ea x
callOnValue adv ea (VFunction x) = callOnFunction adv ea x
callOnValue _ _ _ = throwError $ DomainError "Invalid type to adverb call"

callOnValueAndValue :: Conjunction -> ExtraArgs -> Value -> Value -> St Function
callOnValueAndValue conj ea (VNoun x) (VNoun y) = callOnNounAndNoun conj ea x y
callOnValueAndValue conj ea (VNoun x) (VFunction y) = callOnNounAndFunction conj ea x y
callOnValueAndValue conj ea (VFunction x) (VNoun y) = callOnFunctionAndNoun conj ea x y
callOnValueAndValue conj ea (VFunction x) (VFunction y) = callOnFunctionAndFunction conj ea x y
callOnValueAndValue _ _ _ _ = throwError $ DomainError "Invalid type to conjunction call"
