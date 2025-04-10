{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module TinyAPL.Tolerant
  ( comparisonTolerance
  , realEqual'
  , realEqual
  , complexEqual'
  , complexEqual
  , isReal'
  , isReal
  , isInt'
  , isInt
  , tolerantEquals'
  , tolerantEquals
  , tolerantCompare'
  , tolerantCompare
  , TolerantOrd(..)
  , TolerantL(..)
  , unTolerantL ) where

import TinyAPL.Complex

comparisonTolerance :: Double
comparisonTolerance = 1e-14

realEqual' :: Double -> Double -> Double -> Bool
realEqual' t a b
  | t == 0 || isInfinite a || isInfinite b = a == b
  | t > 0 = abs (a - b) <= t * (abs a `max` abs b)
  | otherwise = abs (a - b) <= negate t

realEqual :: Double -> Double -> Bool
realEqual = realEqual' comparisonTolerance

complexEqual' :: Double -> Complex Double -> Complex Double -> Bool
complexEqual' t a@(ar :+ ai) b@(br :+ bi)
  | t == 0 || isInfinite ar || isInfinite ai || isInfinite br || isInfinite bi = a == b
  | t > 0 = magnitude (a - b) <= t * (magnitude a `max` magnitude b)
  | otherwise = magnitude (a - b) <= negate t

complexEqual :: Complex Double -> Complex Double -> Bool
complexEqual = complexEqual' comparisonTolerance

isReal' :: Double -> Complex Double -> Bool
isReal' t (_ :+ b) = realEqual' t 0 b -- A number is real if its imaginary part compares equal to zero.

isReal :: Complex Double -> Bool
isReal = isReal' comparisonTolerance

isInt' :: Double -> Double -> Bool
isInt' t = realEqual' t <*> (fromInteger . round)

isInt :: Double -> Bool
isInt = isInt' comparisonTolerance

tolerantEquals' :: Double -> Complex Double -> Complex Double -> Bool
tolerantEquals' t a b
  | isReal' t a && isReal' t b = realEqual' t (realPart a) (realPart b)
  | otherwise = complexEqual' t a b

tolerantEquals :: Complex Double -> Complex Double -> Bool
tolerantEquals = tolerantEquals' comparisonTolerance

tolerantCompare' :: Double -> Complex Double -> Complex Double -> Ordering
tolerantCompare' t (ar :+ ai) (br :+ bi)
  | realEqual' t ar br && realEqual' t ai bi = EQ
  | realEqual' t ar br = ai `compare` bi
  | otherwise = ar `compare` br

tolerantCompare :: Complex Double -> Complex Double -> Ordering
tolerantCompare = tolerantCompare' comparisonTolerance

class TolerantOrd c a where
  compareT :: c -> a -> a -> Ordering
  equalsT :: c -> a -> a -> Bool
  equalsT tolerance a b = compareT tolerance a b == EQ
  notEqualT :: c -> a -> a -> Bool
  notEqualT tolerance a b = not $ equalsT tolerance a b
  lessT :: c -> a -> a -> Bool
  lessT tolerance a b = compareT tolerance a b == LT
  lessEqualT :: c -> a -> a -> Bool
  lessEqualT tolerance a b = compareT tolerance a b /= GT
  greaterEqualT :: c -> a -> a -> Bool
  greaterEqualT tolerance a b = compareT tolerance a b /= LT
  greaterT :: c -> a -> a -> Bool
  greaterT tolerance a b = compareT tolerance a b == GT

data TolerantL c a = TolerantL c a

unTolerantL :: TolerantL c a -> a
unTolerantL (TolerantL _ a) = a

instance TolerantOrd c a => Eq (TolerantL c a) where
  (TolerantL tolerance a) == (TolerantL _ b) = equalsT tolerance a b
  (TolerantL tolerance a) /= (TolerantL _ b) = notEqualT tolerance a b

instance TolerantOrd c a => Ord (TolerantL c a) where
  (TolerantL tolerance a) `compare` (TolerantL _ b) = compareT tolerance a b
  (TolerantL tolerance a) < (TolerantL _ b) = lessT tolerance a b
  (TolerantL tolerance a) <= (TolerantL _ b) = lessEqualT tolerance a b
  (TolerantL tolerance a) > (TolerantL _ b) = greaterT tolerance a b
  (TolerantL tolerance a) >= (TolerantL _ b) = greaterEqualT tolerance a b

instance TolerantOrd Double Double where
  compareT t a b = if realEqual' t a b then EQ else a `compare` b

instance TolerantOrd Double (Complex Double) where
  compareT = tolerantCompare'
  equalsT = tolerantEquals'

instance TolerantOrd c a => TolerantOrd c [a] where
  compareT _ [] [] = EQ
  compareT _ [] _ = LT
  compareT _ _ [] = GT
  compareT tolerance (a:as) (b:bs) = compareT tolerance a b <> compareT tolerance as bs

instance (TolerantOrd c a, TolerantOrd c b) => TolerantOrd c (a, b) where
  compareT tolerance (a, b) (c, d) = compareT tolerance a c <> compareT tolerance b d
