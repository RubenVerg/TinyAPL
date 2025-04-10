{-# LANGUAGE DeriveGeneric, DeriveAnyClass, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module TinyAPL.Conjunction
  ( Conjunction(..)
  , callOnNounAndNoun
  , callOnNounAndFunction
  , callOnFunctionAndNoun
  , callOnFunctionAndFunction ) where

import TinyAPL.Context
import TinyAPL.Error
import TinyAPL.Function
import TinyAPL.Noun
import TinyAPL.Util
import TinyAPL.Value
import qualified TinyAPL.Glyphs as G

import GHC.Generics
import Control.DeepSeq
import Control.Applicative (liftA)
import Data.List (intercalate)

data Conjunction
  = DefinedConjunction
    { conjOnNounNoun         :: Maybe (ExtraArgs -> Noun     -> Noun     -> St Function)
    , conjOnNounFunction     :: Maybe (ExtraArgs -> Noun     -> Function -> St Function)
    , conjOnFunctionNoun     :: Maybe (ExtraArgs -> Function -> Noun     -> St Function)
    , conjOnFunctionFunction :: Maybe (ExtraArgs -> Function -> Function -> St Function)
    , conjRepr               :: String
    , conjContext            :: Maybe Context
    , definedConjunctionId   :: Integer }
  | PrimitiveConjunction
    { conjOnNounNoun         :: Maybe (ExtraArgs -> Noun     -> Noun     -> St Function)
    , conjOnNounFunction     :: Maybe (ExtraArgs -> Noun     -> Function -> St Function)
    , conjOnFunctionNoun     :: Maybe (ExtraArgs -> Function -> Noun     -> St Function)
    , conjOnFunctionFunction :: Maybe (ExtraArgs -> Function -> Function -> St Function)
    , conjRepr               :: String
    , conjContext            :: Maybe Context }
  | TrainConjunction
    { conjOnNounNoun         :: Maybe (ExtraArgs -> Noun     -> Noun     -> St Function)
    , conjOnNounFunction     :: Maybe (ExtraArgs -> Noun     -> Function -> St Function)
    , conjOnFunctionNoun     :: Maybe (ExtraArgs -> Function -> Noun     -> St Function)
    , conjOnFunctionFunction :: Maybe (ExtraArgs -> Function -> Function -> St Function)
    , conjContext            :: Maybe Context
    , trainConjunctionTines  :: [Maybe Value] }
  | ExtraArgsConjunction
    { conjOnNounNoun         :: Maybe (ExtraArgs -> Noun     -> Noun     -> St Function)
    , conjOnNounFunction     :: Maybe (ExtraArgs -> Noun     -> Function -> St Function)
    , conjOnFunctionNoun     :: Maybe (ExtraArgs -> Function -> Noun     -> St Function)
    , conjOnFunctionFunction :: Maybe (ExtraArgs -> Function -> Function -> St Function)
    , conjContext            :: Maybe Context
    , extraArgsConjunctionExtraArgs :: ExtraArgs
    , extraArgsConjunctionConjunction :: Conjunction }
  deriving (Generic, NFData)

instance Eq Conjunction where
  DefinedConjunction { definedConjunctionId = a } == DefinedConjunction { definedConjunctionId = b } = a == b
  PrimitiveConjunction { conjRepr = a } == PrimitiveConjunction { conjRepr = b } = a == b
  TrainConjunction { trainConjunctionTines = a } == TrainConjunction { trainConjunctionTines = b } = a == b
  ExtraArgsConjunction { extraArgsConjunctionExtraArgs = a, extraArgsConjunctionConjunction = b } == ExtraArgsConjunction { extraArgsConjunctionExtraArgs = c, extraArgsConjunctionConjunction = d } = a == c && b == d
  _ == _ = False

instance Ord Conjunction where
  DefinedConjunction { definedConjunctionId = a } `compare` DefinedConjunction { definedConjunctionId = b } = a `compare` b
  DefinedConjunction {} `compare` _ = LT
  PrimitiveConjunction {} `compare` DefinedConjunction {} = GT
  PrimitiveConjunction { conjRepr = a } `compare` PrimitiveConjunction { conjRepr = b } = a `compare` b
  PrimitiveConjunction {} `compare` _ = LT
  TrainConjunction {} `compare` DefinedConjunction {} = GT
  TrainConjunction {} `compare` PrimitiveConjunction {} = GT
  TrainConjunction { trainConjunctionTines = a } `compare` TrainConjunction { trainConjunctionTines = b } = b `compare` a
  TrainConjunction {} `compare` _ = LT
  ExtraArgsConjunction {} `compare` DefinedConjunction {} = GT
  ExtraArgsConjunction {} `compare` PrimitiveConjunction {} = GT
  ExtraArgsConjunction {} `compare` TrainConjunction {} = GT
  ExtraArgsConjunction { extraArgsConjunctionExtraArgs = a } `compare` ExtraArgsConjunction { extraArgsConjunctionExtraArgs = b } = a `compare` b

instance (Monad m, MonadShow m ScalarValue) => MonadShow m Conjunction where
  showM DefinedConjunction { conjRepr = repr } = pure repr
  showM PrimitiveConjunction { conjRepr = repr } = pure repr
  showM TrainConjunction { trainConjunctionTines = tines } = liftA (\tines' -> [G.underscore, fst G.train] ++ intercalate [' ', G.separator, ' '] tines' ++ [snd G.train, G.underscore]) (mapM showTine tines)
  showM ExtraArgsConjunction { extraArgsConjunctionExtraArgs = args, extraArgsConjunctionConjunction = conj } = liftA2 (\conj' args' -> [fst G.parens] ++ conj' ++ [snd G.parens, fst G.extraArgs] ++ intercalate [' ', G.separator, ' '] args' ++ [snd G.extraArgs]) (showM conj) (mapM (\(k1, v1) -> liftA2 (\k v -> k ++ [' ', G.guard, ' '] ++ v) (showM k1) (showM v1)) args)

callOnNounAndNoun :: Conjunction -> ExtraArgs -> Noun -> Noun -> St Function
callOnNounAndNoun conj ea x y = case conjOnNounNoun conj of
  Just f -> case conjContext conj of
    Just ctx -> runWithContext ctx $ f ea x y
    Nothing -> f ea x y
  Nothing -> showM conj >>= (\str -> throwError $ DomainError $ "Conjunction " ++ str ++ " cannot be applied to two nouns.")

callOnNounAndFunction :: Conjunction -> ExtraArgs -> Noun -> Function -> St Function
callOnNounAndFunction conj ea x y = case conjOnNounFunction conj of
  Just f -> case conjContext conj of
    Just ctx -> runWithContext ctx $ f ea x y
    Nothing -> f ea x y
  Nothing -> showM conj >>= (\str -> throwError $ DomainError $ "Conjunction " ++ str ++ " cannot be applied to an array and a function.")

callOnFunctionAndNoun :: Conjunction -> ExtraArgs -> Function -> Noun -> St Function
callOnFunctionAndNoun conj ea x y = case conjOnFunctionNoun conj of
  Just f -> case conjContext conj of
    Just ctx -> runWithContext ctx $ f ea x y
    Nothing -> f ea x y
  Nothing -> showM conj >>= (\str -> throwError $ DomainError $ "Conjunction " ++ str ++ " cannot be applied to a function and an array.")

callOnFunctionAndFunction :: Conjunction -> ExtraArgs -> Function -> Function -> St Function
callOnFunctionAndFunction conj ea x y = case conjOnFunctionFunction conj of
  Just f -> case conjContext conj of
    Just ctx -> runWithContext ctx $ f ea x y
    Nothing -> f ea x y
  Nothing -> showM conj >>= (\str -> throwError $ DomainError $ "Conjunction " ++ str ++ " cannot be applied to two functions.")
