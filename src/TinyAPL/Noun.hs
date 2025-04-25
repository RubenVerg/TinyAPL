{-# LANGUAGE DeriveGeneric, DeriveAnyClass, LambdaCase, TupleSections, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module TinyAPL.Noun
  ( ScalarValue(..)
  , Noun(..)
  , arrayRank
  , arrayDepth
  , box
  , fromScalar
  , toScalar
  , scalar
  , vector
  , matrix
  , dictionary
  , arrayOf
  , arrayReshaped
  , arrayReshapedNE
  , majorCells
  , fromMajorCells
  , fillArray
  , fromMajorCellsFilled
  , fromMajorCellsMaybeFilled
  , Repr(..)
  , isNumber
  , isCharacter
  , boolToScalar
  , asWrap
  , asAdverbWrap
  , asConjunctionWrap
  , asStruct
  , asBool
  , asNumber
  , asCharacter
  , asCharacter'
  , asReal
  , likePositiveInfinity
  , likeNegativeInfinity
  , asInt'
  , asInt
  , asNat'
  , asNat
  , asString
  , asStrings
  , asArrayOfStrings
  , isScalar
  , asScalar
  , isEmpty
  , asVector
  , asMatrix
  , onMajorCells
  , complexFloor'
  , complexCeiling'
  , complexRemainder'
  , complexGCD'
  , complexLCM'
  , asAnIntegerIfItIs
  , scalarMonad
  , scalarDyad ) where

import TinyAPL.Complex
import TinyAPL.Error
import TinyAPL.Tolerant
import TinyAPL.Util
import {-# SOURCE #-} TinyAPL.Function
import {-# SOURCE #-} TinyAPL.Adverb
import {-# SOURCE #-} TinyAPL.Conjunction
import {-# SOURCE #-} TinyAPL.Context
import qualified TinyAPL.Glyphs as G

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Numeric.Natural (Natural)
import qualified Data.Matrix as M
import Data.List.NonEmpty (NonEmpty, toList)
import Control.Monad.Error.Class (MonadError)
import Data.List
import Data.Maybe (fromJust, fromMaybe, mapMaybe, listToMaybe)
import Control.Monad
import Data.Tuple (swap)
import Data.Functor.Identity (Identity (runIdentity))

data ScalarValue
  = Number (Complex Double)
  | Character Char
  | Box Noun
  | Wrap Function
  | AdverbWrap Adverb
  | ConjunctionWrap Conjunction
  | Struct Context
  deriving (Generic, NFData)

data Noun
  = Array
    { arrayShape :: [Natural]
    , arrayContents :: [ScalarValue] }
  | Dictionary
    { dictKeys :: [ScalarValue]
    , dictValues :: [ScalarValue] }
  deriving (Generic, NFData)

arrayRank :: Noun -> Natural
arrayRank (Array sh _) = genericLength sh
arrayRank (Dictionary _ _) = 1

arrayDepth :: Noun -> Natural
arrayDepth (Array [] [Box xs]) = 1 + arrayDepth xs
arrayDepth (Array [] _) = 0
arrayDepth (Array _ []) = 1
arrayDepth (Array _ xs) = 1 + maximum (arrayDepth . fromScalar <$> xs)
arrayDepth (Dictionary _ vs) = 1 + maximum (arrayDepth . fromScalar <$> vs)

box :: Noun -> ScalarValue
box b@(Array [] [Box _]) = Box b
box (Array [] [x]) = x
box arr = Box arr

fromScalar :: ScalarValue -> Noun
fromScalar (Box arr) = arr
fromScalar sc        = scalar sc

toScalar :: Noun -> ScalarValue
toScalar (Array [] [x]) = x
toScalar arr            = box arr

scalar :: ScalarValue -> Noun
scalar x = Array [] [x]

vector :: [ScalarValue] -> Noun
vector xs = Array [genericLength xs] xs

matrix :: M.Matrix ScalarValue -> Noun
matrix mat = Array [toEnum $ M.nrows mat, toEnum $ M.ncols mat] $ M.toList mat

dictionary :: [(ScalarValue, ScalarValue)] -> Noun
dictionary xs = uncurry Dictionary $ unzip $ nubBy ((==) `on` fst) xs

arrayOf :: [Natural] -> [ScalarValue] -> Maybe Noun
arrayOf sh cs
  | product sh == genericLength cs = Just $ Array sh cs
  | otherwise = Nothing

arrayReshaped :: [Natural] -> [ScalarValue] -> Maybe Noun
arrayReshaped sh cs =
  if null cs then
    if 0 `elem` sh
    then Just $ Array sh []
    else Nothing
  else Just $ Array sh $ genericTake (product sh) $ cycle cs

arrayReshapedNE :: [Natural] -> NonEmpty ScalarValue -> Noun
arrayReshapedNE sh cs = fromJust $ arrayReshaped sh $ toList cs

majorCells :: Noun -> [Noun]
majorCells a@(Array [] _) = [a]
majorCells (Array (_:sh) cs) = mapMaybe (arrayOf sh) $ chunk (product sh) cs where
  chunk _ [] = []
  chunk l xs = genericTake l xs : chunk l (genericDrop l xs)
majorCells (Dictionary ks vs) = zipWith (\a b -> vector [a, b]) ks vs

fromMajorCells :: MonadError Error m => [Noun] -> m Noun
fromMajorCells [] = pure $ Array [0] []
fromMajorCells (c:cs) = let
  impl = fromJust $ arrayReshaped (1 + genericLength cs : arrayShape c) $ concatMap arrayContents $ c : cs
  in if all (\case
    Array sh _ -> sh == arrayShape c
    Dictionary _ _ -> False) cs then pure impl else throwError $ DomainError "fromMajorCells: mismatched shapes or dictionary"

fillArray :: MonadError Error m => [Natural] -> ScalarValue -> Noun -> m Noun
fillArray _ _ (Dictionary _ _) = throwError $ DomainError "Fill dictionary"
fillArray [] _ a = pure a
fillArray (m:ms) f a = do
  inner <- mapM (fillArray ms f) (majorCells a) >>= fromMajorCells
  let (_:tl) = arrayShape inner
  pure $ Array (m : tl) (genericTake (m * product tl) $ arrayContents inner ++ repeat f)

fromMajorCellsFilled :: MonadError Error m => ScalarValue -> [Noun] -> m Noun
fromMajorCellsFilled _ xs | any (\case Array{} -> False; _ -> True) xs = throwError $ DomainError "fromMajorCellsFilled: dictionary cell"
fromMajorCellsFilled _ [] = pure $ Array [0] []
fromMajorCellsFilled f cs = do
  let shapes = arrayShape <$> cs
  let maxShape = foldr (\a b -> zipWith max a b) (const 0 <$> headPromise shapes) shapes
  cells <- mapM (fillArray maxShape f) cs
  fromMajorCells cells

fromMajorCellsMaybeFilled :: MonadError Error m => Maybe ScalarValue -> [Noun] -> m Noun
fromMajorCellsMaybeFilled Nothing = fromMajorCells
fromMajorCellsMaybeFilled (Just f) = fromMajorCellsFilled f

-- * Total ordering for scalars and arrays

instance Eq ScalarValue where
  (Character a) == (Character b) = a == b
  (Box as) == (Box bs) = as == bs
  (Number a) == (Number b) = tolerantEquals a b
  (Wrap a) == (Wrap b) = a == b
  (AdverbWrap a) == (AdverbWrap b) = a == b
  (ConjunctionWrap a) == (ConjunctionWrap b) = a == b
  _ == _ = False

{-|
  Order:
   * numbers, in lexicographical order (real then imaginary)
   * characters, in codepoint order
   * boxes, ordered by their contents
-}
instance Ord ScalarValue where
  (Number a) `compare` (Number b) = tolerantCompare a b
  (Number _) `compare` _ = LT
  (Character _) `compare` (Number _) = GT
  (Character a) `compare` (Character b) = a `compare` b
  (Character _) `compare` _ = LT
  (Box _) `compare` (Number _) = GT
  (Box _) `compare` (Character _) = GT
  (Box as) `compare` (Box bs) = as `compare` bs
  (Box _) `compare` _ = LT
  (Wrap _) `compare` (Number _) = GT
  (Wrap _) `compare` (Character _) = GT
  (Wrap _) `compare` (Box _) = GT
  (Wrap a) `compare` (Wrap b) = a `compare` b
  (Wrap _) `compare` _ = LT
  (AdverbWrap _) `compare` (Number _) = GT
  (AdverbWrap _) `compare` (Character _) = GT
  (AdverbWrap _) `compare` (Box _) = GT
  (AdverbWrap _) `compare` (Wrap _) = GT
  (AdverbWrap a) `compare` (AdverbWrap b) = a `compare` b
  (AdverbWrap _) `compare` _ = GT
  (ConjunctionWrap _) `compare` (Number _) = GT
  (ConjunctionWrap _) `compare` (Character _) = GT
  (ConjunctionWrap _) `compare` (Box _) = GT
  (ConjunctionWrap _) `compare` (Wrap _) = GT
  (ConjunctionWrap _) `compare` (AdverbWrap _) = GT
  (ConjunctionWrap a) `compare` (ConjunctionWrap b) = a `compare` b
  (ConjunctionWrap _) `compare` _ = GT
  (Struct _) `compare` (Number _) = GT
  (Struct _) `compare` (Character _) = GT
  (Struct _) `compare` (Box _) = GT
  (Struct _) `compare` (Wrap _) = GT
  (Struct _) `compare` (AdverbWrap _) = GT
  (Struct _) `compare` (ConjunctionWrap _) = GT
  (Struct _) `compare` (Struct _) = LT

instance TolerantOrd Double ScalarValue where
  compareT tolerance (Number a) (Number b) = compareT tolerance a b
  compareT _ a b = a `compare` b

instance Eq Noun where
  -- Two arrays are equal iff both their shapes and their ravels are equal.
  (Array ash as) == (Array bsh bs) = (ash, as) == (bsh, bs)
  (Dictionary aks avs) == (Dictionary bks bvs) = sortOn fst (zip aks avs) == sortOn fst (zip bks bvs)
  _ == _ = False

instance TolerantOrd Double Noun where
  compareT tolerance (Array [] [a]) (Array [] [b]) = compareT tolerance a b
  compareT _ (Array [_] []) (Array [_] []) = EQ
  compareT _ (Array [_] []) (Array [_] _) = LT
  compareT _ (Array [_] _) (Array [_] []) = GT
  compareT tolerance (Array [at] (a:as)) (Array [bt] (b:bs)) = compareT tolerance a b <> compareT tolerance (Array [at - 1] as) (Array [bt - 1] bs) <> at `compare` bt
  compareT tolerance a@(Array ash acs) b@(Array bsh bcs)
    | arrayRank a < arrayRank b = compareT tolerance (Array (genericReplicate (arrayRank b - arrayRank a) 1 ++ ash) acs) b <> LT
    | arrayRank a > arrayRank b = compareT tolerance a (Array (genericReplicate (arrayRank a - arrayRank b) 1 ++ bsh) bcs) <> GT
    | otherwise = mconcat (zipWith (compareT tolerance) (majorCells a) (majorCells b)) <> fromMaybe 1 (listToMaybe ash) `compare` fromMaybe 1 (listToMaybe bsh)
  compareT tolerance (Dictionary aks avs) (Dictionary bks bvs) = compareT tolerance (sortOn fst (zip aks avs)) (sortOn fst (zip bks bvs))
  compareT _ (Array _ _) (Dictionary _ _) = LT
  compareT _ (Dictionary _ _) (Array _ _) = GT

instance Ord Noun where
  compare = compareT comparisonTolerance

-- * @Show@ for scalars and arrays

showComplex :: Complex Double -> String
showComplex (a :+ b)
  | b `realEqual` 0 = showAplDouble a
  | otherwise = showAplDouble a ++ [G.imaginary] ++ showAplDouble b

instance MonadShow Identity ScalarValue where
  showM (Number x) = pure $ showComplex x
  showM (Character x) = pure [x]
  showM (Box xs) = (G.enclose :) <$> showM xs
  showM (Wrap fn) = (\x -> [G.wrap, fst G.parens] ++ x ++ [snd G.parens]) <$> showM fn
  showM (AdverbWrap adv) = (\x -> [G.wrap, fst G.parens] ++ x ++ [snd G.parens]) <$> showM adv
  showM (ConjunctionWrap conj) = (\x -> [G.wrap, fst G.parens] ++ x ++ [snd G.parens]) <$> showM conj
  showM (Struct _) = pure $ [fst G.struct] ++ "..." ++ [snd G.struct]

showElementM :: (Monad m, MonadShow m ScalarValue) => ScalarValue -> m String
showElementM (Box xs) = showM xs
showElementM (Wrap fn) = showM fn
showElementM (AdverbWrap adv) = showM adv
showElementM (ConjunctionWrap conj) = showM conj
showElementM x = showM x

instance (Monad m, MonadShow m ScalarValue) => MonadShow m Noun where
  showM (Dictionary [] _)                  = pure [fst G.vector, G.guard, snd G.vector]
  showM (Dictionary ks vs)                 = (\ss -> [fst G.vector] ++ intercalate [' ', G.separator, ' '] ss ++ [snd G.vector]) <$> (zipWithM (\k v -> liftA2 (\k' v' -> k' ++ [G.guard, ' '] ++ v') (showElementM k) (showElementM v)) ks vs)
  showM (Array [] [s])                     = showM s
  showM (Array [_] xs)
    | not (null xs) && all isCharacter xs  = pure $ xs >>= runIdentity . showM
    | otherwise                            = (\ss -> [fst G.vector] ++ intercalate [' ', G.separator, ' '] ss ++ [snd G.vector]) <$> mapM showElementM xs
  showM arr                                = (\ss -> [fst G.highRank] ++ intercalate [' ', G.separator, ' '] ss ++ [snd G.highRank]) <$> mapM showM (majorCells arr)

newtype Repr a = Repr { represented :: a }

charRepr :: Char -> (String, Bool)
charRepr c = case lookup c (swap <$> G.escapes) of
  Just e -> ([G.stringEscape, e], True)
  Nothing -> ([c], False)

instance MonadShow Identity (Repr ScalarValue) where
  showM (Repr (Number x)) = pure $ showComplex x
  showM (Repr (Character x)) = case charRepr x of
    (e, True) -> pure $ [G.first, G.stringDelimiter] ++ e ++ [G.stringDelimiter]
    (c, False) -> pure $ [G.charDelimiter] ++ c ++ [G.charDelimiter]
  showM (Repr (Box x)) = (G.enclose :) <$> showM (Repr x)
  showM (Repr (Wrap fn)) = pure $ runIdentity $ showM fn
  showM (Repr (AdverbWrap adv)) = pure $ runIdentity $ showM adv
  showM (Repr (ConjunctionWrap conj)) = pure $ runIdentity $ showM conj
  showM (Repr (Struct _)) = pure $ [fst G.struct] ++ "..." ++ [snd G.struct]

instance MonadShow St (Repr ScalarValue) where
  -- TODO
  showM x = pure $ runIdentity $ showM x

stringRepr :: [Char] -> String
stringRepr str = [G.stringDelimiter] ++ concatMap (fst . charRepr) str ++ [G.stringDelimiter]

instance (Monad m, MonadShow m (Repr ScalarValue)) => MonadShow m (Repr Noun) where
  showM (Repr (Array [] [s])) = showM $ Repr s
  showM (Repr (Array [_] xs))
    | not (null xs) && all isCharacter xs = pure $ stringRepr $ asCharacter' <$> xs
    | otherwise = (\x -> [fst G.vector] ++ intercalate [' ', G.separator, ' '] x ++ [snd G.vector]) <$> mapM (showM . Repr . fromScalar) xs
  showM (Repr arr@Array{}) = (\x -> [fst G.highRank] ++ intercalate [' ', G.separator, ' '] x ++ [snd G.highRank]) <$> mapM (showM . Repr) (majorCells arr)
  showM (Repr (Dictionary [] _)) = pure [fst G.vector, G.guard, snd G.vector]
  showM (Repr (Dictionary ks vs)) = (\x -> [fst G.vector] ++ intercalate [' ', G.separator, ' '] x ++ [snd G.vector]) <$> zipWithM (\k v -> liftA2 (\x y -> x ++ [G.guard, ' '] ++ y) (showM $ Repr $ fromScalar k) (showM $ Repr $ fromScalar v)) ks vs

-- * Conversions

isNumber :: ScalarValue -> Bool
isNumber (Number _) = True
isNumber _ = False

isCharacter :: ScalarValue -> Bool
isCharacter (Character _) = True
isCharacter _ = False

boolToScalar :: Bool -> ScalarValue
boolToScalar True = Number 1
boolToScalar False = Number 0

asWrap :: MonadError Error m => Error -> ScalarValue -> m Function
asWrap _ (Wrap fn) = pure fn
asWrap e _ = throwError e

asAdverbWrap :: MonadError Error m => Error -> ScalarValue -> m Adverb
asAdverbWrap _ (AdverbWrap adv) = pure adv
asAdverbWrap e _ = throwError e

asConjunctionWrap :: MonadError Error m => Error -> ScalarValue -> m Conjunction
asConjunctionWrap _ (ConjunctionWrap conj) = pure conj
asConjunctionWrap e _ = throwError e

asStruct :: MonadError Error m => Error -> ScalarValue -> m Context
asStruct _ (Struct ctx) = pure ctx
asStruct e _ = throwError e

asBool :: MonadError Error m => Error -> ScalarValue -> m Bool
asBool _ (Number 0) = pure False
asBool _ (Number 1) = pure True
asBool e _ = throwError e

asNumber :: MonadError Error m => Error -> ScalarValue -> m (Complex Double)
asNumber _ (Number x) = pure x
asNumber e _ = throwError e

asCharacter :: MonadError Error m => Error -> ScalarValue -> m Char
asCharacter _ (Character x) = pure x
asCharacter e _ = throwError e

asCharacter' :: ScalarValue -> Char
asCharacter' (Character x) = x
asCharacter' _ = error "asCharacter': not a character"

asReal :: MonadError Error m => Error -> Complex Double -> m Double
asReal e x
  | isReal x = pure $ realPart x
  | otherwise = throwError e

-- These need to be somewhat large
likePositiveInfinity :: Integral num => num
likePositiveInfinity = fromInteger $ toInteger (maxBound `div` 2 :: Int)

likeNegativeInfinity :: Integral num => num
likeNegativeInfinity = fromInteger $ toInteger (minBound `div` 2 :: Int)

asInt' :: MonadError Error m => Integral num => Error -> Double -> m num
asInt' e x
  | isInfinite x && x > 0 = pure likePositiveInfinity
  | isInfinite x && x < 0 = pure likeNegativeInfinity
  | isInt x = pure $ fromInteger $ floor x
  | otherwise = throwError e

asInt :: (MonadError Error m, Integral num) => Error -> Complex Double -> m num
asInt e = asInt' e <=< asReal e

asNat' :: (MonadError Error m, Integral num) => Error -> num -> m Natural
asNat' e x
  | x >= 0 = pure $ toEnum $ fromEnum x
  | otherwise = throwError e

asNat :: MonadError Error m => Error -> Complex Double -> m Natural
asNat e = asNat' e <=< asInt e

asString :: MonadError Error m => Error -> Noun -> m String
asString err = asVector err >=> mapM (asCharacter err)

asStrings :: MonadError Error m => Error -> Noun -> m [String]
asStrings _ (Array [] [Character x]) = pure [[x]]
asStrings _ (Array [_] vec) | all isCharacter vec = pure [asCharacter' <$> vec]
asStrings err (Array [_] vec) = mapM (asString err . fromScalar) vec
asStrings err _ = throwError err

asArrayOfStrings :: MonadError Error m => Error -> Noun -> m ([Natural], [String])
asArrayOfStrings _ (Array [] [Character x]) = pure ([], [[x]])
asArrayOfStrings _ (Array [_] vec) | all isCharacter vec = pure ([], [asCharacter' <$> vec])
asArrayOfStrings err (Array sh cs) = (sh ,) <$> mapM (asString err . fromScalar) cs
asArrayOfStrings err _ = throwError err

isScalar :: Noun -> Bool
isScalar (Array [] _) = True
isScalar _ = False

asScalar :: MonadError Error m => Error -> Noun -> m ScalarValue
asScalar _ (Array _ [x]) = pure x
asScalar e _ = throwError e

isEmpty :: Noun -> Bool
isEmpty (Array sh _) = 0 `elem` sh
isEmpty (Dictionary ks _) = null ks

asVector :: MonadError Error m => Error -> Noun -> m [ScalarValue]
asVector _ (Array [] scalar) = pure scalar
asVector _ (Array [_] vec)   = pure vec
asVector e _                 = throwError e

asMatrix :: MonadError Error m => Error -> Noun -> m (M.Matrix ScalarValue)
asMatrix _ (Array [] scalar)        = pure $ M.fromList 1 1 scalar
asMatrix _ (Array [cols] vec)       = pure $ M.fromList 1 (fromEnum cols) vec
asMatrix _ (Array [rows, cols] mat) = pure $ M.fromList (fromEnum rows) (fromEnum cols) mat
asMatrix e _                        = throwError e

onMajorCells :: MonadError Error m =>
  ([Noun] -> m [Noun])
  -> Noun -> m Noun
onMajorCells f x@(Array _ _) = do
  result <- f $ majorCells x
  case arrayReshaped (arrayShape x) $ concatMap arrayContents result of
    Nothing -> throwError $ DomainError ""
    Just rs -> return rs
onMajorCells _ (Dictionary _ _) = throwError $ DomainError "Dictionary not allowed here"

-- * functions that depend on tolerance

-- https://aplwiki.com/wiki/Complex_floor
complexFloor' :: Double -> Complex Double -> Complex Double
complexFloor' t (r :+ i) = let
  b = componentFloor $ r :+ i
  x = fracPart r
  y = fracPart i
  in
    if lessT t ((x + y) :+ 0) 1 then b
    else if greaterEqualT t (x :+ 0) (y :+ 0) then b + 1
    else b + (0 :+ 1)

complexCeiling' :: Double -> Complex Double -> Complex Double
complexCeiling' t = negate . complexFloor' t . negate

complexRemainder' :: Double -> Complex Double -> Complex Double -> Complex Double
complexRemainder' t w z =
  if equalsT t w 0 then z
  else if isReal' t (z / w) && isInt' t (realPart $ z / w) then 0
  else z - w * complexFloor' t (if equalsT t w 0 then z else z / w)

complexGCD' :: Double -> Complex Double -> Complex Double -> Complex Double
complexGCD' t a w = asAnIntegerIfItIs $ if equalsT t (complexRemainder' t a w) 0 then a else complexGCD' t (complexRemainder' t a w) a

complexLCM' :: Double -> Complex Double -> Complex Double -> Complex Double
complexLCM' t x y = asAnIntegerIfItIs $ if equalsT t x 0 && equalsT t y 0 then 0 else (x * y) / (complexGCD' t x y)

asAnIntegerIfItIs :: Complex Double -> Complex Double
asAnIntegerIfItIs y = if isInt (realPart y) && isInt (imagPart y) then fromInteger (round $ realPart y) :+ fromInteger (round $ imagPart y) else y

-- * Scalar functions

boxyMonad :: MonadError Error m => (ScalarValue -> m ScalarValue) -> ScalarValue -> m ScalarValue
boxyMonad f (Box xs) = Box <$> scalarMonad f xs
boxyMonad f x = f x

scalarMonad :: MonadError Error m =>
  (ScalarValue -> m ScalarValue)
       -> Noun -> m Noun
scalarMonad f (Array sh cs) = Array sh <$> mapM (boxyMonad f) cs
scalarMonad f (Dictionary ks vs) = Dictionary ks <$> mapM (boxyMonad f) vs

boxyDyad :: MonadError Error m => (ScalarValue -> ScalarValue -> m ScalarValue) -> ScalarValue -> ScalarValue -> m ScalarValue
boxyDyad f (Box as) (Box bs) = Box <$> scalarDyad f as bs
boxyDyad f (Box as) b = Box <$> scalarDyad f as (scalar b)
boxyDyad f a (Box bs) = Box <$> scalarDyad f (scalar a) bs
boxyDyad f a b = f a b

scalarDyad :: MonadError Error m =>
  (ScalarValue -> ScalarValue -> m ScalarValue)
       -> Noun ->        Noun -> m Noun
scalarDyad f a@(Array ash as) b@(Array bsh bs)
  | null ash && null bsh = let ([a'], [b']) = (as, bs) in scalar <$> f' a' b'
  | null ash = let [a'] = as in Array bsh <$> mapM (a' `f'`) bs
  | null bsh = let [b'] = bs in Array ash <$> mapM (`f'` b') as
  | ash == bsh = Array (arrayShape a) <$> zipWithM f' (arrayContents a) (arrayContents b)
  | ash `isPrefixOf` bsh || bsh `isPrefixOf` ash = zipWithM (scalarDyad f) (majorCells a) (majorCells b) >>= fromMajorCells
  | length ash /= length bsh = throwError $ RankError "Mismatched left and right argument ranks"
  | otherwise = throwError $ LengthError "Mismatched left and right argument shapes"
  where
    f' = boxyDyad f
scalarDyad f (Dictionary aks avs) (Dictionary bks bvs)
  = dictionary <$> mapM (\k ->
      if k `elem` aks && k `elem` bks then (k, ) <$> boxyDyad f (fromJust $ lookup k $ zip aks avs) (fromJust $ lookup k $ zip bks bvs)
      else if k `elem` aks then pure (k, fromJust $ lookup k $ zip aks avs)
      else if k `elem` bks then pure (k, fromJust $ lookup k $ zip bks bvs)
      else throwError $ DomainError "???") (nub $ aks ++ bks)
scalarDyad f (Dictionary aks avs) (Array [] [b]) = Dictionary aks <$> mapM (\a' -> boxyDyad f a' b) avs
scalarDyad f (Array [] [a]) (Dictionary bks bvs) = Dictionary bks <$> mapM (\b' -> boxyDyad f a b') bvs
scalarDyad _ _ _ = throwError $ DomainError "Cannot combine dictionary and non-scalar array"

-- * Instances for arrays

monadN2N f = scalarMonad f' where
  f' x = do
    x' <- flip asNumber x $ DomainError "Expected number"
    Number <$> f x'

monadN2N' f = monadN2N $ pure . f

dyadNN2N f = scalarDyad f' where
  f' a b = do
    a' <- flip asNumber a $ DomainError "Expected number"
    b' <- flip asNumber b $ DomainError "Expected number"
    Number <$> f a' b'

dyadNN2N' f = dyadNN2N $ pure .: f

instance Num Noun where
  (+) = unerror .: dyadNN2N' (+)
  (-) = unerror .: dyadNN2N' (-)
  (*) = unerror .: dyadNN2N' (*)
  abs = unerror . monadN2N' abs
  signum = unerror . monadN2N' signum
  fromInteger = scalar . Number . fromInteger

instance Fractional Noun where
  recip = unerror . monadN2N (\case
    0 -> throwError $ DomainError "Divide by zero"
    x -> pure $ recip x)
  (/) = unerror .: dyadNN2N (\cases
    0 0 -> pure 1
    _ 0 -> throwError $ DomainError "Divide by zero"
    x y -> pure $ x / y)
  fromRational = scalar . Number . fromRational

instance Floating Noun where
  pi = scalar $ Number pi
  exp = unerror . monadN2N' exp
  log = unerror . monadN2N (\case
    0 -> throwError $ DomainError "Logarithm of zero"
    x -> pure $ log x)
  sin = unerror . monadN2N' sin
  cos = unerror . monadN2N' cos
  tan = unerror . monadN2N' tan
  asin = unerror . monadN2N' asin
  acos = unerror . monadN2N' acos
  atan = unerror . monadN2N' atan
  sinh = unerror . monadN2N' sinh
  cosh = unerror . monadN2N' cosh
  tanh = unerror . monadN2N' tanh
  asinh = unerror . monadN2N' asinh
  acosh = unerror . monadN2N' acosh
  atanh = unerror . monadN2N' atanh