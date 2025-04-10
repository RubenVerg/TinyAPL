{-# LANGUAGE DeriveGeneric, DeriveAnyClass, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module TinyAPL.Adverb
  ( Adverb(..)
  , callOnNoun
  , callOnFunction ) where

import TinyAPL.Conjunction
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

data Adverb
  = DefinedAdverb
    { adverbOnNoun             :: Maybe (ExtraArgs -> Noun     -> St Function)
    , adverbOnFunction         :: Maybe (ExtraArgs -> Function -> St Function)
    , adverbRepr               :: String
    , adverbContext            :: Maybe Context
    , definedAdverbId          :: Integer }
  | PrimitiveAdverb
    { adverbOnNoun             :: Maybe (ExtraArgs -> Noun     -> St Function)
    , adverbOnFunction         :: Maybe (ExtraArgs -> Function -> St Function)
    , adverbRepr               :: String
    , adverbContext            :: Maybe Context }
  | PartialAdverb
    { adverbOnNoun             :: Maybe (ExtraArgs -> Noun     -> St Function)
    , adverbOnFunction         :: Maybe (ExtraArgs -> Function -> St Function)
    , adverbContext            :: Maybe Context
    , partialAdverbConjunction :: Conjunction
    , partialAdverbRight       :: Value }
  | TrainAdverb
    { adverbOnNoun             :: Maybe (ExtraArgs -> Noun     -> St Function)
    , adverbOnFunction         :: Maybe (ExtraArgs -> Function -> St Function)
    , adverbContext            :: Maybe Context
    , trainAdverbTines         :: [Maybe Value] }
  | ExtraArgsAdverb
    { adverbOnNoun             :: Maybe (ExtraArgs -> Noun     -> St Function)
    , adverbOnFunction         :: Maybe (ExtraArgs -> Function -> St Function)
    , adverbContext            :: Maybe Context
    , extraArgsAdverbExtraArgs :: ExtraArgs
    , extraArgsAdverbAdverb    :: Adverb }
  deriving (Generic, NFData)

instance Eq Adverb where
  DefinedAdverb { definedAdverbId = a } == DefinedAdverb { definedAdverbId = b } = a == b
  PrimitiveAdverb { adverbRepr = a } == PrimitiveAdverb { adverbRepr = b } = a == b
  PartialAdverb { partialAdverbConjunction = ac, partialAdverbRight = ar } == PartialAdverb { partialAdverbConjunction = bc, partialAdverbRight = br } = ac == bc && ar == br
  TrainAdverb { trainAdverbTines = a } == TrainAdverb { trainAdverbTines = b } = a == b
  ExtraArgsAdverb { extraArgsAdverbExtraArgs = a, extraArgsAdverbAdverb = b } == ExtraArgsAdverb { extraArgsAdverbExtraArgs = c, extraArgsAdverbAdverb = d } = a == c && b == d
  _ == _ = False

instance Ord Adverb where
  DefinedAdverb { definedAdverbId = a } `compare` DefinedAdverb { definedAdverbId = b } = a `compare` b
  DefinedAdverb {} `compare` _ = LT
  PrimitiveAdverb {} `compare` DefinedAdverb {} = GT
  PrimitiveAdverb { adverbRepr = a } `compare` PrimitiveAdverb { adverbRepr = b } = a `compare` b
  PrimitiveAdverb {} `compare` _ = LT
  PartialAdverb {} `compare` DefinedAdverb {} = GT
  PartialAdverb {} `compare` PrimitiveAdverb {} = GT
  PartialAdverb { partialAdverbConjunction = ac, partialAdverbRight = ar } `compare` PartialAdverb { partialAdverbConjunction = bc, partialAdverbRight = br } = ac `compare` bc <> ar `compare` br
  PartialAdverb {} `compare` _ = LT
  TrainAdverb {} `compare` DefinedAdverb {} = GT
  TrainAdverb {} `compare` PrimitiveAdverb {} = GT
  TrainAdverb {} `compare` PartialAdverb {} = GT
  TrainAdverb { trainAdverbTines = a } `compare` TrainAdverb { trainAdverbTines = b } = b `compare` a
  TrainAdverb {} `compare` _ = LT
  ExtraArgsAdverb {} `compare` DefinedAdverb {} = GT
  ExtraArgsAdverb {} `compare` PrimitiveAdverb {} = GT
  ExtraArgsAdverb {} `compare` PartialAdverb {} = GT
  ExtraArgsAdverb {} `compare` TrainAdverb {} = GT
  ExtraArgsAdverb { extraArgsAdverbExtraArgs = a } `compare` ExtraArgsAdverb { extraArgsAdverbExtraArgs = b } = a `compare` b

instance (Monad m, MonadShow m ScalarValue) => MonadShow m Adverb where
  showM DefinedAdverb { adverbRepr = repr } = pure repr
  showM PrimitiveAdverb { adverbRepr = repr } = pure repr
  showM PartialAdverb { partialAdverbConjunction = conj, partialAdverbRight = n } = liftA2 (\conj' n' -> conj' ++ [fst G.parens] ++ n' ++ [snd G.parens]) (showM conj) (showM n)
  showM TrainAdverb { trainAdverbTines = tines } = liftA (\tines' -> [G.underscore, fst G.train] ++ intercalate [' ', G.separator, ' '] tines' ++ [snd G.train]) (mapM showTine tines)
  showM ExtraArgsAdverb { extraArgsAdverbExtraArgs = args, extraArgsAdverbAdverb = adv } = liftA2 (\adv' args' -> [fst G.parens] ++ adv' ++ [snd G.parens, fst G.extraArgs] ++ intercalate [' ', G.separator, ' '] args' ++ [snd G.extraArgs]) (showM adv) (mapM (\(k1, v1) -> liftA2 (\k v -> k ++ [' ', G.guard, ' '] ++ v) (showM k1) (showM v1)) args)

callOnNoun :: Adverb -> ExtraArgs -> Noun -> St Function
callOnNoun adv ea x = case adverbOnNoun adv of
  Just f -> case adverbContext adv of
    Just ctx -> runWithContext ctx $ f ea x
    Nothing -> f ea x
  Nothing -> showM adv >>= (\str -> throwError $ DomainError $ "Adverb " ++ str ++ " does not take array operands.")

callOnFunction :: Adverb -> ExtraArgs -> Function -> St Function
callOnFunction adv ea x = case adverbOnFunction adv of
  Just f -> case adverbContext adv of
    Just ctx -> runWithContext ctx $ f ea x
    Nothing -> f ea x
  Nothing -> showM adv >>= (\str -> throwError $ DomainError $ "Adverb " ++ str ++ " does not take functions operands.")
