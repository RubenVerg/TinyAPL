{-# LANGUAGE DeriveGeneric, DeriveAnyClass, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module TinyAPL.Function
  ( ExtraArgs
  , Function(..)
  , showTine
  , callMonad
  , callDyad
  , callUn
  , callAnti
  , callContra
  , callDis
  , callBi
  , callAna ) where

import TinyAPL.Context
import TinyAPL.Error
import TinyAPL.Noun
import TinyAPL.Util
import TinyAPL.Value
import {-# SOURCE #-} TinyAPL.Adverb
import {-# SOURCE #-} TinyAPL.Conjunction
import qualified TinyAPL.Glyphs as G

import GHC.Generics
import Control.DeepSeq
import Control.Applicative (liftA3, liftA)
import Data.List (intercalate)

type ExtraArgs = [(ScalarValue, ScalarValue)]

data Function
  = DefinedFunction
    { functionMonad :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionDyad  :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionUn :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionAnti :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionContra :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionDis :: Maybe (ExtraArgs -> Noun -> St (Noun, Noun))
    , functionBi :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionAna :: Maybe (ExtraArgs -> Noun -> Noun -> St [Noun])
    , functionRepr  :: String
    , functionContext :: Maybe Context
    , definedFunctionId :: Integer }
  | PrimitiveFunction
    { functionMonad :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionDyad  :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionUn :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionAnti :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionContra :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionDis :: Maybe (ExtraArgs -> Noun -> St (Noun, Noun))
    , functionBi :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionAna :: Maybe (ExtraArgs -> Noun -> Noun -> St [Noun])
    , functionRepr  :: String
    , functionContext :: Maybe Context }
  | PartialFunction
    { functionMonad :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionDyad  :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionUn :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionAnti :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionContra :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionDis :: Maybe (ExtraArgs -> Noun -> St (Noun, Noun))
    , functionBi :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionAna :: Maybe (ExtraArgs -> Noun -> Noun -> St [Noun])
    , functionContext :: Maybe Context
    , partialFunctionFunction :: Function
    , partialFunctionLeft :: Noun }
  | DerivedFunctionNoun
    { functionMonad :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionDyad  :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionUn :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionAnti :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionContra :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionDis :: Maybe (ExtraArgs -> Noun -> St (Noun, Noun))
    , functionBi :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionAna :: Maybe (ExtraArgs -> Noun -> Noun -> St [Noun])
    , functionContext :: Maybe Context
    , derivedFunctionAdverb :: Adverb
    , derivedFunctionNounLeft :: Noun }
  | DerivedFunctionFunction
    { functionMonad :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionDyad  :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionUn :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionAnti :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionContra :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionDis :: Maybe (ExtraArgs -> Noun -> St (Noun, Noun))
    , functionBi :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionAna :: Maybe (ExtraArgs -> Noun -> Noun -> St [Noun])
    , functionContext :: Maybe Context
    , derivedFunctionAdverb :: Adverb
    , derivedFunctionFunctionLeft :: Function }
  | DerivedFunctionNounNoun
    { functionMonad :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionDyad  :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionUn :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionAnti :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionContra :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionDis :: Maybe (ExtraArgs -> Noun -> St (Noun, Noun))
    , functionBi :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionAna :: Maybe (ExtraArgs -> Noun -> Noun -> St [Noun])
    , functionContext :: Maybe Context
    , derivedFunctionConjunction :: Conjunction
    , derivedFunctionNounLeft :: Noun
    , derivedFunctionNounRight :: Noun }
  | DerivedFunctionNounFunction
    { functionMonad :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionDyad  :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionUn :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionAnti :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionContra :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionDis :: Maybe (ExtraArgs -> Noun -> St (Noun, Noun))
    , functionBi :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionAna :: Maybe (ExtraArgs -> Noun -> Noun -> St [Noun])
    , functionContext :: Maybe Context
    , derivedFunctionConjunction :: Conjunction
    , derivedFunctionNounLeft :: Noun
    , derivedFunctionFunctionRight :: Function }
  | DerivedFunctionFunctionNoun
    { functionMonad :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionDyad  :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionUn :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionAnti :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionContra :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionDis :: Maybe (ExtraArgs -> Noun -> St (Noun, Noun))
    , functionBi :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionAna :: Maybe (ExtraArgs -> Noun -> Noun -> St [Noun])
    , functionContext :: Maybe Context
    , derivedFunctionConjunction :: Conjunction
    , derivedFunctionFunctionLeft :: Function
    , derivedFunctionNounRight :: Noun }
  | DerivedFunctionFunctionFunction
    { functionMonad :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionDyad  :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionUn :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionAnti :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionContra :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionDis :: Maybe (ExtraArgs -> Noun -> St (Noun, Noun))
    , functionBi :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionAna :: Maybe (ExtraArgs -> Noun -> Noun -> St [Noun])
    , functionContext :: Maybe Context
    , derivedFunctionConjunction :: Conjunction
    , derivedFunctionFunctionLeft :: Function
    , derivedFunctionFunctionRight :: Function }
  | UnwrapArrayFunction
    { functionMonad :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionDyad  :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionUn :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionAnti :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionContra :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionDis :: Maybe (ExtraArgs -> Noun -> St (Noun, Noun))
    , functionBi :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionAna :: Maybe (ExtraArgs -> Noun -> Noun -> St [Noun])
    , functionContext :: Maybe Context
    , unwrapFunctionArray :: Noun }
  | TrainFunction
    { functionMonad :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionDyad  :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionUn :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionAnti :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionContra :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionDis :: Maybe (ExtraArgs -> Noun -> St (Noun, Noun))
    , functionBi :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionAna :: Maybe (ExtraArgs -> Noun -> Noun -> St [Noun])
    , functionContext :: Maybe Context
    , trainFunctionTines :: [Maybe Value] }
  | ExtraArgsFunction
    { functionMonad :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionDyad  :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionUn :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionAnti :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionContra :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionDis :: Maybe (ExtraArgs -> Noun -> St (Noun, Noun))
    , functionBi :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionAna :: Maybe (ExtraArgs -> Noun -> Noun -> St [Noun])
    , functionContext :: Maybe Context
    , extraArgsFunctionExtraArgs :: ExtraArgs
    , extraArgsFunctionFunction :: Function }
  deriving (Generic, NFData)

instance Eq Function where
  DefinedFunction { definedFunctionId = a } == DefinedFunction { definedFunctionId = b } = a == b
  PrimitiveFunction { functionRepr = a } == PrimitiveFunction { functionRepr = b } = a == b
  PartialFunction { partialFunctionFunction = af, partialFunctionLeft = al } == PartialFunction { partialFunctionFunction = bf, partialFunctionLeft = bl } = af == bf && al == bl
  DerivedFunctionNoun { derivedFunctionAdverb = aadv, derivedFunctionNounLeft = aa } == DerivedFunctionNoun { derivedFunctionAdverb = badv, derivedFunctionNounLeft = ba } = aadv == badv && aa == ba
  DerivedFunctionFunction { derivedFunctionAdverb = aadv, derivedFunctionFunctionLeft = aa } == DerivedFunctionFunction { derivedFunctionAdverb = badv, derivedFunctionFunctionLeft = ba } = aadv == badv && aa == ba
  DerivedFunctionNounNoun { derivedFunctionConjunction = aconj, derivedFunctionNounLeft = aa, derivedFunctionNounRight = ab } == DerivedFunctionNounNoun { derivedFunctionConjunction = bconj, derivedFunctionNounLeft = ba, derivedFunctionNounRight = bb } = aconj == bconj && aa == ba && ab == bb
  DerivedFunctionNounFunction { derivedFunctionConjunction = aconj, derivedFunctionNounLeft = aa, derivedFunctionFunctionRight = ab } == DerivedFunctionNounFunction { derivedFunctionConjunction = bconj, derivedFunctionNounLeft = ba, derivedFunctionFunctionRight = bb } = aconj == bconj && aa == ba && ab == bb
  DerivedFunctionFunctionNoun { derivedFunctionConjunction = aconj, derivedFunctionFunctionLeft = aa, derivedFunctionNounRight = ab } == DerivedFunctionFunctionNoun { derivedFunctionConjunction = bconj, derivedFunctionFunctionLeft = ba, derivedFunctionNounRight = bb } = aconj == bconj && aa == ba && ab == bb
  DerivedFunctionFunctionFunction { derivedFunctionConjunction = aconj, derivedFunctionFunctionLeft = aa, derivedFunctionFunctionRight = ab } == DerivedFunctionFunctionFunction { derivedFunctionConjunction = bconj, derivedFunctionFunctionLeft = ba, derivedFunctionFunctionRight = bb } = aconj == bconj && aa == ba && ab == bb
  UnwrapArrayFunction { unwrapFunctionArray = a } == UnwrapArrayFunction { unwrapFunctionArray = b } = a == b
  TrainFunction { trainFunctionTines = a } == TrainFunction { trainFunctionTines = b } = a == b
  ExtraArgsFunction { extraArgsFunctionExtraArgs = a, extraArgsFunctionFunction = b } == ExtraArgsFunction { extraArgsFunctionExtraArgs = c, extraArgsFunctionFunction = d } = a == c && b == d
  _ == _ = False

instance Ord Function where
  DefinedFunction { definedFunctionId = a } `compare` DefinedFunction { definedFunctionId = b } = a `compare` b
  DefinedFunction {} `compare` _ = LT
  PrimitiveFunction {} `compare` DefinedFunction {} = GT
  PrimitiveFunction { functionRepr = a } `compare` PrimitiveFunction { functionRepr = b } = a `compare` b
  PrimitiveFunction {} `compare` _ = LT
  PartialFunction {} `compare` DefinedFunction {} = GT
  PartialFunction {} `compare` PrimitiveFunction {} = GT
  PartialFunction { partialFunctionLeft = a } `compare` PartialFunction { partialFunctionLeft = b } = a `compare` b
  PartialFunction {} `compare` _ = LT
  DerivedFunctionNoun {} `compare` DefinedFunction {} = GT
  DerivedFunctionNoun {} `compare` PrimitiveFunction {} = GT
  DerivedFunctionNoun {} `compare` PartialFunction {} = GT
  DerivedFunctionNoun { derivedFunctionAdverb = aadv, derivedFunctionNounLeft = aa } `compare` DerivedFunctionNoun { derivedFunctionAdverb = badv, derivedFunctionNounLeft = ba } = aadv `compare` badv <> aa `compare` ba
  DerivedFunctionNoun {} `compare` _ = LT
  DerivedFunctionFunction {} `compare` DefinedFunction {} = GT
  DerivedFunctionFunction {} `compare` PrimitiveFunction {} = GT
  DerivedFunctionFunction {} `compare` PartialFunction {} = GT
  DerivedFunctionFunction {} `compare` DerivedFunctionNoun {} = GT
  DerivedFunctionFunction { derivedFunctionAdverb = aadv, derivedFunctionFunctionLeft = aa } `compare` DerivedFunctionFunction { derivedFunctionAdverb = badv, derivedFunctionFunctionLeft = ba } = aadv `compare` badv <> aa `compare` ba
  DerivedFunctionFunction {} `compare` _ = LT
  DerivedFunctionNounNoun {} `compare` DefinedFunction {} = GT
  DerivedFunctionNounNoun {} `compare` PrimitiveFunction {} = GT
  DerivedFunctionNounNoun {} `compare` PartialFunction {} = GT
  DerivedFunctionNounNoun {} `compare` DerivedFunctionNoun {} = GT
  DerivedFunctionNounNoun {} `compare` DerivedFunctionFunction {} = GT
  DerivedFunctionNounNoun { derivedFunctionConjunction = aconj, derivedFunctionNounLeft = aa, derivedFunctionNounRight = ab }
    `compare` DerivedFunctionNounNoun { derivedFunctionConjunction = bconj, derivedFunctionNounLeft = ba, derivedFunctionNounRight = bb } = aconj `compare` bconj <> aa `compare` ba <> ab `compare` bb
  DerivedFunctionNounNoun {} `compare` _ = LT
  DerivedFunctionNounFunction {} `compare` DefinedFunction {} = GT
  DerivedFunctionNounFunction {} `compare` PrimitiveFunction {} = GT
  DerivedFunctionNounFunction {} `compare` PartialFunction {} = GT
  DerivedFunctionNounFunction {} `compare` DerivedFunctionNoun {} = GT
  DerivedFunctionNounFunction {} `compare` DerivedFunctionFunction {} = GT
  DerivedFunctionNounFunction {} `compare` DerivedFunctionNounNoun {} = GT
  DerivedFunctionNounFunction { derivedFunctionConjunction = aconj, derivedFunctionNounLeft = aa, derivedFunctionFunctionRight = ab }
    `compare` DerivedFunctionNounFunction { derivedFunctionConjunction = bconj, derivedFunctionNounLeft = ba, derivedFunctionFunctionRight = bb } = aconj `compare` bconj <> aa `compare` ba <> ab `compare` bb
  DerivedFunctionNounFunction {} `compare` _ = LT
  DerivedFunctionFunctionNoun {} `compare` DefinedFunction {} = GT
  DerivedFunctionFunctionNoun {} `compare` PrimitiveFunction {} = GT
  DerivedFunctionFunctionNoun {} `compare` PartialFunction {} = GT
  DerivedFunctionFunctionNoun {} `compare` DerivedFunctionNoun {} = GT
  DerivedFunctionFunctionNoun {} `compare` DerivedFunctionFunction {} = GT
  DerivedFunctionFunctionNoun {} `compare` DerivedFunctionNounNoun {} = GT
  DerivedFunctionFunctionNoun {} `compare` DerivedFunctionNounFunction {} = GT
  DerivedFunctionFunctionNoun { derivedFunctionConjunction = aconj, derivedFunctionFunctionLeft = aa, derivedFunctionNounRight = ab }
    `compare` DerivedFunctionFunctionNoun { derivedFunctionConjunction = bconj, derivedFunctionFunctionLeft = ba, derivedFunctionNounRight = bb } = aconj `compare` bconj <> aa `compare` ba <> ab `compare` bb
  DerivedFunctionFunctionNoun {} `compare` _ = LT
  DerivedFunctionFunctionFunction {} `compare` DefinedFunction {} = GT
  DerivedFunctionFunctionFunction {} `compare` PrimitiveFunction {} = GT
  DerivedFunctionFunctionFunction {} `compare` PartialFunction {} = GT
  DerivedFunctionFunctionFunction {} `compare` DerivedFunctionNoun {} = GT
  DerivedFunctionFunctionFunction {} `compare` DerivedFunctionFunction {} = GT
  DerivedFunctionFunctionFunction {} `compare` DerivedFunctionNounNoun {} = GT
  DerivedFunctionFunctionFunction {} `compare` DerivedFunctionNounFunction {} = GT
  DerivedFunctionFunctionFunction {} `compare` DerivedFunctionFunctionNoun {} = GT
  DerivedFunctionFunctionFunction { derivedFunctionConjunction = aconj, derivedFunctionFunctionLeft = aa, derivedFunctionFunctionRight = ab }
    `compare` DerivedFunctionFunctionFunction { derivedFunctionConjunction = bconj, derivedFunctionFunctionLeft = ba, derivedFunctionFunctionRight = bb } = aconj `compare` bconj <> aa `compare` ba <> ab `compare` bb
  DerivedFunctionFunctionFunction {} `compare` _ = LT
  UnwrapArrayFunction {} `compare` DefinedFunction {} = GT
  UnwrapArrayFunction {} `compare` PrimitiveFunction {} = GT
  UnwrapArrayFunction {} `compare` PartialFunction {} = GT
  UnwrapArrayFunction {} `compare` DerivedFunctionNoun {} = GT
  UnwrapArrayFunction {} `compare` DerivedFunctionFunction {} = GT
  UnwrapArrayFunction {} `compare` DerivedFunctionNounNoun {} = GT
  UnwrapArrayFunction {} `compare` DerivedFunctionNounFunction {} = GT
  UnwrapArrayFunction {} `compare` DerivedFunctionFunctionNoun {} = GT
  UnwrapArrayFunction {} `compare` DerivedFunctionFunctionFunction {} = GT
  UnwrapArrayFunction { unwrapFunctionArray = a } `compare` UnwrapArrayFunction { unwrapFunctionArray = b } = a `compare` b
  UnwrapArrayFunction {} `compare` _ = LT
  TrainFunction {} `compare` DefinedFunction {} = GT
  TrainFunction {} `compare` PrimitiveFunction {} = GT
  TrainFunction {} `compare` PartialFunction {} = GT
  TrainFunction {} `compare` DerivedFunctionNoun {} = GT
  TrainFunction {} `compare` DerivedFunctionFunction {} = GT
  TrainFunction {} `compare` DerivedFunctionNounNoun {} = GT
  TrainFunction {} `compare` DerivedFunctionNounFunction {} = GT
  TrainFunction {} `compare` DerivedFunctionFunctionNoun {} = GT
  TrainFunction {} `compare` DerivedFunctionFunctionFunction {} = GT
  TrainFunction {} `compare` UnwrapArrayFunction {} = GT
  TrainFunction { trainFunctionTines = a } `compare` TrainFunction { trainFunctionTines = b } = b `compare` a
  TrainFunction {} `compare` _ = LT
  ExtraArgsFunction {} `compare` DefinedFunction {} = GT
  ExtraArgsFunction {} `compare` PrimitiveFunction {} = GT
  ExtraArgsFunction {} `compare` PartialFunction {} = GT
  ExtraArgsFunction {} `compare` DerivedFunctionNoun {} = GT
  ExtraArgsFunction {} `compare` DerivedFunctionFunction {} = GT
  ExtraArgsFunction {} `compare` DerivedFunctionNounNoun {} = GT
  ExtraArgsFunction {} `compare` DerivedFunctionNounFunction {} = GT
  ExtraArgsFunction {} `compare` DerivedFunctionFunctionNoun {} = GT
  ExtraArgsFunction {} `compare` DerivedFunctionFunctionFunction {} = GT
  ExtraArgsFunction {} `compare` UnwrapArrayFunction {} = GT
  ExtraArgsFunction {} `compare` TrainFunction {} = GT
  ExtraArgsFunction { extraArgsFunctionExtraArgs = a } `compare` ExtraArgsFunction { extraArgsFunctionExtraArgs = b } = a `compare` b

showTine :: (Monad m, MonadShow m ScalarValue) => Maybe Value -> m String
showTine Nothing = pure ""
showTine (Just x) = showM x

instance (Monad m, MonadShow m ScalarValue) => MonadShow m Function where
  showM (DefinedFunction { functionRepr = repr }) = pure repr
  showM (PrimitiveFunction { functionRepr = repr }) = pure repr
  showM (PartialFunction { partialFunctionFunction = fn, partialFunctionLeft = n }) = liftA2 (\n' fn' -> [fst G.parens] ++ n' ++ [snd G.parens] ++ fn') (showM n) (showM fn)
  showM (DerivedFunctionNoun { derivedFunctionAdverb = adv, derivedFunctionNounLeft = n }) = liftA2 (\n' adv' -> [fst G.parens] ++ n' ++ [snd G.parens] ++ adv') (showM n) (showM adv)
  showM (DerivedFunctionFunction { derivedFunctionAdverb = adv, derivedFunctionFunctionLeft = u }) = liftA2 (\u' adv' -> [fst G.parens] ++ u' ++ [snd G.parens] ++ adv') (showM u) (showM adv)
  showM (DerivedFunctionNounNoun { derivedFunctionConjunction = conj, derivedFunctionNounLeft = n, derivedFunctionNounRight = m }) = liftA3 (\n' conj' m' -> [fst G.parens] ++ n' ++ [snd G.parens] ++ conj' ++ [fst G.parens] ++ m' ++ [snd G.parens]) (showM n) (showM conj) (showM m)
  showM (DerivedFunctionNounFunction { derivedFunctionConjunction = conj, derivedFunctionNounLeft = n, derivedFunctionFunctionRight = v }) = liftA3 (\n' conj' v' -> [fst G.parens] ++ n' ++ [snd G.parens] ++ conj' ++ [fst G.parens] ++ v' ++ [snd G.parens]) (showM n) (showM conj) (showM v)
  showM (DerivedFunctionFunctionNoun { derivedFunctionConjunction = conj, derivedFunctionFunctionLeft = u, derivedFunctionNounRight = m }) = liftA3 (\u' conj' m' -> [fst G.parens] ++ u' ++ [snd G.parens] ++ conj' ++ [fst G.parens] ++ m' ++ [snd G.parens]) (showM u) (showM conj) (showM m)
  showM (DerivedFunctionFunctionFunction { derivedFunctionConjunction = conj, derivedFunctionFunctionLeft = u, derivedFunctionFunctionRight = v }) = liftA3 (\u' conj' v' -> [fst G.parens] ++ u' ++ [snd G.parens] ++ conj' ++ [fst G.parens] ++ v' ++ [snd G.parens]) (showM u) (showM conj) (showM v)
  showM (UnwrapArrayFunction { unwrapFunctionArray = arr }) = liftA (\arr' -> [G.unwrap, fst G.parens] ++ arr' ++ [snd G.parens]) (showM arr)
  showM (TrainFunction { trainFunctionTines = tines }) = liftA (\tines' -> [fst G.train] ++ intercalate [' ', G.separator, ' '] tines' ++ [snd G.train]) (mapM showTine tines)
  showM (ExtraArgsFunction { extraArgsFunctionExtraArgs = args, extraArgsFunctionFunction = fn }) = liftA2 (\fn' args' -> [fst G.parens] ++ fn' ++ [snd G.parens, fst G.extraArgs] ++ intercalate [' ', G.separator, ' '] args' ++ [snd G.extraArgs]) (showM fn) (mapM (\(k1, v1) -> liftA2 (\k v -> k ++ [' ', G.guard, ' '] ++ v) (showM k1) (showM v1)) args)

callMonad :: Function -> ExtraArgs -> Noun -> St Noun
callMonad f ea x = case functionMonad f of
  Just m -> case functionContext f of
    Just ctx -> runWithContext ctx $ m ea x
    Nothing -> m ea x
  Nothing -> showM f >>= (\str -> throwError $ DomainError $ "Function " ++ str ++ " cannot be called monadically")

callDyad :: Function -> ExtraArgs -> Noun -> Noun -> St Noun
callDyad f ea a b = case functionDyad f of
  Just d -> case functionContext f of
    Just ctx -> runWithContext ctx $ d ea a b
    Nothing -> d ea a b
  Nothing -> showM f >>= (\str -> throwError $ DomainError $ "Function " ++ str ++ " cannot be called dyadically")

callUn :: Function -> ExtraArgs -> Noun -> St Noun
callUn f ea x = case functionUn f of
  Just u -> case functionContext f of
    Just ctx -> runWithContext ctx $ u ea x
    Nothing -> u ea x
  Nothing -> showM f >>= (\str -> throwError $ DomainError $ "Function " ++ str ++ " has no un-inverse")

callAnti :: Function -> ExtraArgs -> Noun -> Noun -> St Noun
callAnti f ea a b = case functionAnti f of
  Just t -> case functionContext f of
    Just ctx -> runWithContext ctx $ t ea a b
    Nothing -> t ea a b
  Nothing -> showM f >>= (\str -> throwError $ DomainError $ "Function " ++ str ++ " has no anti-inverse")

callContra :: Function -> ExtraArgs -> Noun -> Noun -> St Noun
callContra f ea a b = case functionContra f of
  Just c -> case functionContext f of
    Just ctx -> runWithContext ctx $ c ea a b
    Nothing -> c ea a b
  Nothing -> showM f >>= (\str -> throwError $ DomainError $ "Function " ++ str ++ " has no contra-inverse")

callDis :: Function -> ExtraArgs -> Noun -> St (Noun, Noun)
callDis f ea x = case functionDis f of
  Just d -> case functionContext f of
    Just ctx -> runWithContext ctx $ d ea x
    Nothing -> d ea x
  Nothing -> showM f >>= (\str -> throwError $ DomainError $ "Function " ++ str ++ " has no dis-inverse")

callBi :: Function -> ExtraArgs -> Noun -> St Noun
callBi f ea x = case functionBi f of
  Just i -> case functionContext f of
    Just ctx -> runWithContext ctx $ i ea x
    Nothing -> i ea x
  Nothing -> showM f >>= (\str -> throwError $ DomainError $ "Function " ++ str ++ " has no bi-inverse")

callAna :: Function -> ExtraArgs -> Noun -> Noun -> St [Noun]
callAna f ea a b = case functionAna f of
  Just n -> case functionContext f of
    Just ctx -> runWithContext ctx $ n ea a b
    Nothing -> n ea a b
  Nothing -> showM f >>= (\str -> throwError $ DomainError $ "Function " ++ str ++ " has no ana-inverse")
