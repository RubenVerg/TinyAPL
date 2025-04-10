{-# LANGUAGE DeriveGeneric, DeriveAnyClass, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module TinyAPL.Quads
  ( Nilad(..)
  , getNilad
  , setNilad
  , Quads(..)
  , quadsFromReprs ) where

import TinyAPL.Adverb
import TinyAPL.Conjunction
import TinyAPL.Error
import TinyAPL.Function
import TinyAPL.Noun
import TinyAPL.Util
import {-# SOURCE #-} TinyAPL.Context

import Control.DeepSeq
import GHC.Generics

data Nilad = Nilad
  { niladGet :: Maybe (St Noun)
  , niladSet :: Maybe (Noun -> St ())
  , niladRepr :: String
  , niladContext :: Maybe Context }

instance NFData Nilad where
  rnf (Nilad g s r c) = rwhnf g `seq` rwhnf s `seq` rnf r `seq` rnf c `seq` ()

instance Monad m => MonadShow m Nilad where
  showM Nilad{ niladRepr = s } = pure s

getNilad :: Nilad -> St Noun
getNilad (Nilad (Just g) _ _ (Just ctx)) = runWithContext ctx g
getNilad (Nilad (Just g) _ _ Nothing) = g
getNilad (Nilad Nothing _ repr _) = throwError $ DomainError $ "Nilad " ++ repr ++ " cannot be get"

setNilad :: Nilad -> Noun -> St ()
setNilad (Nilad _ (Just s) _ (Just ctx)) x = runWithContext ctx $ s x
setNilad (Nilad _ (Just s) _ Nothing) x = s x
setNilad (Nilad _ Nothing repr _) _ = throwError $ DomainError $ "Nilad " ++ repr ++ " cannot be set"

data Quads = Quads
  { quadArrays :: [(String, Nilad)]
  , quadFunctions :: [(String, Function)]
  , quadAdverbs :: [(String, Adverb)]
  , quadConjunctions :: [(String, Conjunction)] }
  deriving (Generic, NFData)

instance Semigroup Quads where
  (Quads aAr aFn aAd aCn) <> (Quads bAr bFn bAd bCn) = Quads (aAr ++ bAr) (aFn ++ bFn) (aAd ++ bAd) (aCn ++ bCn)

instance Monoid Quads where
  mempty = Quads [] [] [] []

quadsFromReprs :: [Nilad] -> [Function] -> [Adverb] -> [Conjunction] -> Quads
quadsFromReprs ns fs as cs = Quads ((\x -> (niladRepr x, x)) <$> ns) ((\x -> (functionRepr x, x)) <$> fs) ((\x -> (adverbRepr x, x)) <$> as) ((\x -> (conjRepr x, x)) <$> cs)
