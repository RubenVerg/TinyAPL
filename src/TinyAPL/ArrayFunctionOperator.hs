{-# LANGUAGE FlexibleContexts, LambdaCase, DeriveGeneric, DeriveAnyClass, InstanceSigs, TupleSections, MultiParamTypeClasses, FlexibleInstances #-}
module TinyAPL.ArrayFunctionOperator where

import TinyAPL.Error
import TinyAPL.Util
import qualified TinyAPL.Glyphs as G
import TinyAPL.Complex
import Numeric.Natural
import Data.List
import Control.Monad
import Data.Maybe (mapMaybe, fromJust, listToMaybe, fromMaybe)
import Control.Monad.State
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Tuple (swap)
import qualified Data.Matrix as M
import qualified Data.IORef as IORef
import Data.IORef (IORef)
import Control.DeepSeq
import GHC.Generics

-- * Arrays

{-|
  Scalars:
   * complex numbers (internally represented as @Complex Double@s)
   * characters
   * array boxes (enclosures)
-}
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

-- * Array helper functions

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

fromMajorCells :: [Noun] -> Noun
fromMajorCells [] = Array [0] []
fromMajorCells (c:cs) = let
  impl = fromJust $ arrayReshaped (1 + genericLength cs : arrayShape c) $ concatMap arrayContents $ c : cs
  in if all (\case
    Array sh _ -> sh == arrayShape c
    Dictionary _ _ -> False) cs then impl else error "fromMajorCells: mismatched shapes or dictionary"

-- * Number comparison functions

comparisonTolerance :: Double
comparisonTolerance = 1e-14

realEqual' :: Double -> Double -> Double -> Bool
realEqual' t a b = if isInfinite a || isInfinite b then a == b else abs (a - b) <= t * (abs a `max` abs b)

realEqual :: Double -> Double -> Bool
realEqual = realEqual' comparisonTolerance

complexEqual' :: Double -> Complex Double -> Complex Double -> Bool
complexEqual' t a@(ar :+ ai) b@(br :+ bi) =
  if isInfinite ar || isInfinite ai || isInfinite br || isInfinite bi
  then a == b
  else magnitude (a - b) <= t * (magnitude a `max` magnitude b)

complexEqual :: Complex Double -> Complex Double -> Bool
complexEqual = complexEqual' comparisonTolerance

isReal' :: Double -> Complex Double -> Bool
isReal' t (_ :+ b) = realEqual' t 0 b -- A number is real if its imaginary part compares equal to zero.

isReal :: Complex Double -> Bool
isReal = isReal' comparisonTolerance

tolerantEquals' :: Double -> Complex Double -> Complex Double -> Bool
tolerantEquals' t a b
  | isReal' t a && isReal' t b = realEqual' t (realPart a) (realPart b)
  | otherwise = complexEqual' t a b

tolerantEquals :: Complex Double -> Complex Double -> Bool
tolerantEquals = tolerantEquals' comparisonTolerance

tolerantcompareT :: Double -> Complex Double -> Complex Double -> Ordering
tolerantcompareT t (ar :+ ai) (br :+ bi)
  | realEqual' t ar br && realEqual' t ai bi = EQ
  | realEqual' t ar br = ai `compare` bi
  | otherwise = ar `compare` br

tolerantCompare :: Complex Double -> Complex Double -> Ordering
tolerantCompare = tolerantcompareT comparisonTolerance

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

instance TolerantOrd Double Double where
  compareT t a b = if realEqual' t a b then EQ else a `compare` b

instance TolerantOrd Double (Complex Double) where
  compareT t a b = tolerantcompareT t a b
  equalsT = tolerantEquals'

instance TolerantOrd c a => TolerantOrd c [a] where
  compareT _ [] [] = EQ
  compareT _ [] _ = LT
  compareT _ _ [] = GT
  compareT tolerance (a:as) (b:bs) = compareT tolerance a b <> compareT tolerance as bs

instance (TolerantOrd c a, TolerantOrd c b) => TolerantOrd c (a, b) where
  compareT tolerance (a, b) (c, d) = compareT tolerance a c <> compareT tolerance b d

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

isInt' :: Double -> Double -> Bool
isInt' t = realEqual' t <*> (fromInteger . round)

isInt :: Double -> Bool
isInt = isInt' comparisonTolerance

-- * @Show@ for scalars and arrays

showComplex :: Complex Double -> String
showComplex (a :+ b)
  | b `realEqual` 0 = showAplDouble a
  | otherwise = showAplDouble a ++ [G.imaginary] ++ showAplDouble b

instance Show ScalarValue where
  show (Number x) = showComplex x
  show (Character x) = [x]
  show (Box xs) = G.enclose : show xs
  show (Wrap fn) = [G.wrap, fst G.parens] ++ show fn ++ [snd G.parens]
  show (AdverbWrap adv) = [G.wrap, fst G.parens] ++ show adv ++ [snd G.parens]
  show (ConjunctionWrap conj) = [G.wrap, fst G.parens] ++ show conj ++ [snd G.parens]
  show (Struct _) = [fst G.struct] ++ "..." ++ [snd G.struct]

showElement :: ScalarValue -> String
showElement (Box xs) = show xs
showElement (Wrap fn) = show fn
showElement (AdverbWrap adv) = show adv
showElement (ConjunctionWrap conj) = show conj
showElement x = show x

instance Show Noun where
  show (Dictionary [] _)                  = [fst G.vector, G.guard, snd G.vector]
  show (Dictionary ks vs)                 = [fst G.vector] ++ intercalate [' ', G.separator, ' '] (zipWith (\k v -> showElement k ++ [G.guard, ' '] ++ showElement v) ks vs) ++ [snd G.vector]
  show (Array [] [s])                     = show s
  show (Array [_] xs)
    | not (null xs) && all isCharacter xs = xs >>= show
    | otherwise                           = [fst G.vector] ++ intercalate [' ', G.separator, ' '] (showElement <$> xs) ++ [snd G.vector]
  show arr                                = [fst G.highRank] ++ intercalate [' ', G.separator, ' '] (show <$> majorCells arr) ++ [snd G.highRank]

charRepr :: Char -> (String, Bool)
charRepr c = case lookup c (swap <$> G.escapes) of
  Just e -> ([G.stringEscape, e], True)
  Nothing -> ([c], False)

scalarRepr :: ScalarValue -> String
scalarRepr (Number x) = showComplex x
scalarRepr (Character x) = case charRepr x of
  (e, True) -> [G.first, G.stringDelimiter] ++ e ++ [G.stringDelimiter]
  (c, False) -> [G.charDelimiter] ++ c ++ [G.charDelimiter]
scalarRepr (Box xs) = G.enclose : arrayRepr xs
scalarRepr (Wrap fn) = [G.wrap, fst G.parens] ++ show fn ++ [snd G.parens]
scalarRepr (AdverbWrap adv) = [G.wrap, fst G.parens] ++ show adv ++ [snd G.parens]
scalarRepr (ConjunctionWrap conj) = [G.wrap, fst G.parens] ++ show conj ++ [snd G.parens]
scalarRepr (Struct _) = [fst G.struct] ++ "..." ++ [snd G.struct]

stringRepr :: [Char] -> String
stringRepr str = [G.stringDelimiter] ++ concatMap (fst . charRepr) str ++ [G.stringDelimiter]

arrayRepr :: Noun -> String
arrayRepr (Dictionary [] _)                  = [fst G.vector, G.guard, snd G.vector]
arrayRepr (Dictionary ks vs)                 = [fst G.vector] ++ intercalate [' ', G.separator, ' '] (zipWith (\k v -> arrayRepr (fromScalar k) ++ [G.guard, ' '] ++ arrayRepr (fromScalar v)) ks vs) ++ [snd G.vector]
arrayRepr (Array [] [s]) = scalarRepr s
arrayRepr (Array [_] xs)
  | not (null xs) && all isCharacter xs = stringRepr $ asCharacter' <$> xs
  | otherwise = [fst G.vector] ++ intercalate [' ', G.separator, ' '] (arrayRepr . fromScalar <$> xs) ++ [snd G.vector]
arrayRepr arr = [fst G.highRank] ++ intercalate [' ', G.separator, ' '] (arrayRepr <$> majorCells arr) ++ [snd G.highRank]

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
complexGCD' t a w = if equalsT t (complexRemainder' t a w) 0 then a else complexGCD' t (complexRemainder' t a w) a

complexLCM' :: Double -> Complex Double -> Complex Double -> Complex Double
complexLCM' t x y = if equalsT t x 0 && equalsT t y 0 then 0 else (x * y) / (complexGCD' t x y)

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
  | ash `isPrefixOf` bsh || bsh `isPrefixOf` ash = fromMajorCells <$> zipWithM (scalarDyad f) (majorCells a) (majorCells b)
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

monadB2B f = scalarMonad f' where
  f' x = do
    x' <- flip asBool x $ DomainError "Expected boolean"
    boolToScalar <$> f x'

monadB2B' f = monadB2B $ pure . f

dyadBB2B f = scalarDyad f' where
  f' a b = do
    a' <- flip asBool a $ DomainError "Expected boolean"
    b' <- flip asBool b $ DomainError "Expected boolean"
    boolToScalar <$> f a' b'

dyadBB2B' f = dyadBB2B $ pure .: f

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

-- * Functions

type ExtraArgs = [(ScalarValue, ScalarValue)]

data Function
  = DefinedFunction
    { functionMonad :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionDyad  :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionRepr  :: String
    , functionContext :: Maybe Context
    , definedFunctionId :: Integer }
  | PrimitiveFunction
    { functionMonad :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionDyad  :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionRepr  :: String
    , functionContext :: Maybe Context }
  | PartialFunction
    { functionMonad :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionDyad  :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionContext :: Maybe Context
    , partialFunctionFunction :: Function
    , partialFunctionLeft :: Noun }
  | DerivedFunctionNoun
    { functionMonad :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionDyad  :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionContext :: Maybe Context
    , derivedFunctionAdverb :: Adverb
    , derivedFunctionNounLeft :: Noun }
  | DerivedFunctionFunction
    { functionMonad :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionDyad  :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionContext :: Maybe Context
    , derivedFunctionAdverb :: Adverb
    , derivedFunctionFunctionLeft :: Function }
  | DerivedFunctionNounNoun
    { functionMonad :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionDyad  :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionContext :: Maybe Context
    , derivedFunctionConjunction :: Conjunction
    , derivedFunctionNounLeft :: Noun
    , derivedFunctionNounRight :: Noun }
  | DerivedFunctionNounFunction
    { functionMonad :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionDyad  :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionContext :: Maybe Context
    , derivedFunctionConjunction :: Conjunction
    , derivedFunctionNounLeft :: Noun
    , derivedFunctionFunctionRight :: Function }
  | DerivedFunctionFunctionNoun
    { functionMonad :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionDyad  :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionContext :: Maybe Context
    , derivedFunctionConjunction :: Conjunction
    , derivedFunctionFunctionLeft :: Function
    , derivedFunctionNounRight :: Noun }
  | DerivedFunctionFunctionFunction
    { functionMonad :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionDyad  :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionContext :: Maybe Context
    , derivedFunctionConjunction :: Conjunction
    , derivedFunctionFunctionLeft :: Function
    , derivedFunctionFunctionRight :: Function }
  | UnwrapArrayFunction
    { functionMonad :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionDyad  :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionContext :: Maybe Context
    , unwrapFunctionArray :: Noun }
  | TrainFunction
    { functionMonad :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionDyad  :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
    , functionContext :: Maybe Context
    , trainFunctionTines :: [Maybe Value] }
  | ExtraArgsFunction
    { functionMonad :: Maybe (ExtraArgs -> Noun -> St Noun)
    , functionDyad  :: Maybe (ExtraArgs -> Noun -> Noun -> St Noun)
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

showTine :: Maybe Value -> String
showTine Nothing = ""
showTine (Just x) = show x

instance Show Function where
  show (DefinedFunction { functionRepr = repr }) = repr
  show (PrimitiveFunction { functionRepr = repr }) = repr
  show (PartialFunction { partialFunctionFunction = fn, partialFunctionLeft = n }) = [fst G.parens] ++ show n ++ [snd G.parens] ++ show fn
  show (DerivedFunctionNoun { derivedFunctionAdverb = adv, derivedFunctionNounLeft = n }) = [fst G.parens] ++ show n ++ [snd G.parens] ++ show adv
  show (DerivedFunctionFunction { derivedFunctionAdverb = adv, derivedFunctionFunctionLeft = u }) = [fst G.parens] ++ show u ++ [snd G.parens] ++ show adv
  show (DerivedFunctionNounNoun { derivedFunctionConjunction = conj, derivedFunctionNounLeft = n, derivedFunctionNounRight = m }) = [fst G.parens] ++ show n ++ [snd G.parens] ++ show conj ++ [fst G.parens] ++ show m ++ [snd G.parens]
  show (DerivedFunctionNounFunction { derivedFunctionConjunction = conj, derivedFunctionNounLeft = n, derivedFunctionFunctionRight = v }) = [fst G.parens] ++ show n ++ [snd G.parens] ++ show conj ++ [fst G.parens] ++ show v ++ [snd G.parens]
  show (DerivedFunctionFunctionNoun { derivedFunctionConjunction = conj, derivedFunctionFunctionLeft = u, derivedFunctionNounRight = m }) = [fst G.parens] ++ show u ++ [snd G.parens] ++ show conj ++ [fst G.parens] ++ show m ++ [snd G.parens]
  show (DerivedFunctionFunctionFunction { derivedFunctionConjunction = conj, derivedFunctionFunctionLeft = u, derivedFunctionFunctionRight = v }) = [fst G.parens] ++ show u ++ [snd G.parens] ++ show conj ++ [fst G.parens] ++ show v ++ [snd G.parens]
  show (UnwrapArrayFunction { unwrapFunctionArray = arr }) = [G.unwrap, fst G.parens] ++ show arr ++ [snd G.parens]
  show (TrainFunction { trainFunctionTines = tines }) = [fst G.train] ++ intercalate [' ', G.separator, ' '] (showTine <$> tines) ++ [snd G.train]
  show (ExtraArgsFunction { extraArgsFunctionExtraArgs = args, extraArgsFunctionFunction = fn }) = [fst G.parens] ++ show fn ++ [snd G.parens, fst G.extraArgs] ++ intercalate [' ', G.separator, ' '] ((\(k, v) -> show k ++ [' ', G.guard, ' '] ++ show v) <$> args) ++ [snd G.extraArgs]

noMonad :: String -> Error
noMonad str = DomainError $ "Function " ++ str ++ " cannot be called monadically"

callMonad :: Function -> ExtraArgs -> Noun -> St Noun
callMonad f ea x = case functionMonad f of
  Just m -> case functionContext f of
    Just ctx -> runWithContext ctx $ m ea x
    Nothing -> m ea x
  Nothing -> throwError $ noMonad $ show f

noDyad :: String -> Error
noDyad str = DomainError $ "Function " ++ str ++ " cannot be called dyadically"

callDyad :: Function -> ExtraArgs -> Noun -> Noun -> St Noun
callDyad f ea a b = case functionDyad f of
  Just d -> case functionContext f of
    Just ctx -> runWithContext ctx $ d ea a b
    Nothing -> d ea a b
  Nothing -> throwError $ noDyad $ show f

-- * Operators

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

instance Show Adverb where
  show DefinedAdverb { adverbRepr = repr } = repr
  show PrimitiveAdverb { adverbRepr = repr } = repr
  show PartialAdverb { partialAdverbConjunction = conj, partialAdverbRight = n } = show conj ++ [fst G.parens] ++ show n ++ [snd G.parens]
  show TrainAdverb { trainAdverbTines = tines } = [G.underscore, fst G.train] ++ intercalate [' ', G.separator, ' '] (showTine <$> tines) ++ [snd G.train]
  show ExtraArgsAdverb { extraArgsAdverbExtraArgs = args, extraArgsAdverbAdverb = adv } = [fst G.parens] ++ show adv ++ [snd G.parens, fst G.extraArgs] ++ intercalate [' ', G.separator, ' '] ((\(k, v) -> show k ++ [' ', G.guard, ' '] ++ show v) <$> args) ++ [snd G.extraArgs]

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

callOnNoun :: Adverb -> ExtraArgs -> Noun -> St Function
callOnNoun adv ea x = case adverbOnNoun adv of
  Just f -> case adverbContext adv of
    Just ctx -> runWithContext ctx $ f ea x
    Nothing -> f ea x
  Nothing -> throwError $ DomainError $ "Operator " ++ show adv ++ " does not take array operands."

callOnFunction :: Adverb -> ExtraArgs -> Function -> St Function
callOnFunction adv ea x = case adverbOnFunction adv of
  Just f -> case adverbContext adv of
    Just ctx -> runWithContext ctx $ f ea x
    Nothing -> f ea x
  Nothing -> throwError $ DomainError $ "Operator " ++ show adv ++ " does not take functions operands."

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
  
instance Show Conjunction where
  show DefinedConjunction { conjRepr = repr } = repr
  show PrimitiveConjunction { conjRepr = repr } = repr
  show TrainConjunction { trainConjunctionTines = tines } = [G.underscore, fst G.train] ++ intercalate [' ', G.separator, ' '] (showTine <$> tines) ++ [snd G.train, G.underscore]
  show ExtraArgsConjunction { extraArgsConjunctionExtraArgs = args, extraArgsConjunctionConjunction = conj } = [fst G.parens] ++ show conj ++ [snd G.parens, fst G.extraArgs] ++ intercalate [' ', G.separator, ' '] ((\(k, v) -> show k ++ [' ', G.guard, ' '] ++ show v) <$> args) ++ [snd G.extraArgs]

instance Eq Conjunction where
  DefinedConjunction { definedConjunctionId = a } == DefinedConjunction { definedConjunctionId = b } = a == b
  PrimitiveConjunction { conjRepr = a } == PrimitiveConjunction { conjRepr = b } = a == b
  TrainConjunction { trainConjunctionTines = a } == TrainConjunction { trainConjunctionTines = b } = a == b
  ExtraArgsConjunction { extraArgsConjunctionExtraArgs = a, extraArgsConjunctionConjunction = b } == ExtraArgsConjunction { extraArgsConjunctionExtraArgs = c, extraArgsConjunctionConjunction = d } = a == c && b == d
  _ == _ = False

instance Ord Conjunction where
  compare :: Conjunction -> Conjunction -> Ordering
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

callOnNounAndNoun :: Conjunction -> ExtraArgs -> Noun -> Noun -> St Function
callOnNounAndNoun conj ea x y = case conjOnNounNoun conj of
  Just f -> case conjContext conj of
    Just ctx -> runWithContext ctx $ f ea x y
    Nothing -> f ea x y
  Nothing -> throwError $ DomainError $ "Operator " ++ show conj ++ " cannot be applied to two nouns."

callOnNounAndFunction :: Conjunction -> ExtraArgs -> Noun -> Function -> St Function
callOnNounAndFunction conj ea x y = case conjOnNounFunction conj of
  Just f -> case conjContext conj of
    Just ctx -> runWithContext ctx $ f ea x y
    Nothing -> f ea x y
  Nothing -> throwError $ DomainError $ "Operator " ++ show conj ++ " cannot be applied to an array and a function."

callOnFunctionAndNoun :: Conjunction -> ExtraArgs -> Function -> Noun -> St Function
callOnFunctionAndNoun conj ea x y = case conjOnFunctionNoun conj of
  Just f -> case conjContext conj of
    Just ctx -> runWithContext ctx $ f ea x y
    Nothing -> f ea x y
  Nothing -> throwError $ DomainError $ "Operator " ++ show conj ++ " cannot be applied to a function and an array."

callOnFunctionAndFunction :: Conjunction -> ExtraArgs -> Function -> Function -> St Function
callOnFunctionAndFunction conj ea x y = case conjOnFunctionFunction conj of
  Just f -> case conjContext conj of
    Just ctx -> runWithContext ctx $ f ea x y
    Nothing -> f ea x y
  Nothing -> throwError $ DomainError $ "Operator " ++ show conj ++ " cannot be applied to two functions."

-- * Quads

data Nilad = Nilad
  { niladGet :: Maybe (St Noun)
  , niladSet :: Maybe (Noun -> St ())
  , niladRepr :: String
  , niladContext :: Maybe Context }

instance NFData Nilad where
  rnf (Nilad g s r c) = rwhnf g `seq` rwhnf s `seq` rnf r `seq` rnf c `seq` ()

getNilad :: Nilad -> St Noun
getNilad (Nilad (Just g) _ _ (Just ctx)) = runWithContext ctx g
getNilad (Nilad (Just g) _ _ Nothing) = g
getNilad g@(Nilad Nothing _ _ _) = throwError $ DomainError $ "Nilad " ++ show g ++ " cannot be get"

setNilad :: Nilad -> Noun -> St ()
setNilad (Nilad _ (Just s) _ (Just ctx)) x = runWithContext ctx $ s x
setNilad (Nilad _ (Just s) _ Nothing) x = s x
setNilad s@(Nilad _ Nothing _ _) _ = throwError $ DomainError $ "Nilad " ++ show s ++ " cannot be set"

instance Show Nilad where 
  show (Nilad { niladRepr = r }) = r

data Quads = Quads
  { quadArrays :: [(String, Nilad)]
  , quadFunctions :: [(String, Function)]
  , quadAdverbs :: [(String, Adverb)]
  , quadConjunctions :: [(String, Conjunction)] }
  deriving (Show, Generic, NFData)

instance Semigroup Quads where
  (Quads aAr aFn aAd aCn) <> (Quads bAr bFn bAd bCn) = Quads (aAr ++ bAr) (aFn ++ bFn) (aAd ++ bAd) (aCn ++ bCn)

instance Monoid Quads where
  mempty = Quads [] [] [] []
  
quadsFromReprs :: [Nilad] -> [Function] -> [Adverb] -> [Conjunction] -> Quads
quadsFromReprs ns fs as cs = Quads ((\x -> (niladRepr x, x)) <$> ns) ((\x -> (functionRepr x, x)) <$> fs) ((\x -> (adverbRepr x, x)) <$> as) ((\x -> (conjRepr x, x)) <$> cs)

-- * State

data VariableType
  = VariableNormal
  | VariableConstant
  | VariablePrivate
  deriving (Show, Eq, Generic, NFData)

data Scope = Scope
  { scopeNouns :: [(String, (VariableType, Noun))]
  , scopeFunctions :: [(String, (VariableType, Function))]
  , scopeAdverbs :: [(String, (VariableType, Adverb))]
  , scopeConjunctions :: [(String, (VariableType, Conjunction))]
  , scopeParent :: Maybe (IORef Scope) }
  deriving (Generic, NFData)

instance Show Scope where
  show (Scope arr fn adv conj p) = "Scope { arrays = " ++ show arr ++ ", functions = " ++ show fn ++ ", adverbs = " ++ show adv ++ ", conjunctions = " ++ show conj ++ ", " ++ (case p of
    Nothing -> "no parent"
    Just _ -> "a parent") ++ " }"

specialNames :: [String]
specialNames = [[G.alpha], [G.omega], [G.alpha, G.alpha], [G.omega, G.omega], [G.alphaBar, G.alphaBar], [G.omegaBar, G.omegaBar], [G.del], [G.underscore, G.del], [G.underscore, G.del, G.underscore]]

scopeShallowLookupNoun :: Bool -> String -> Scope -> Maybe Noun
scopeShallowLookupNoun private name sc = case lookup name (scopeNouns sc) of
  Nothing -> Nothing
  Just (VariableNormal, x) -> Just x
  Just (VariableConstant, x) -> Just x
  Just (VariablePrivate, x) | private -> Just x
  Just (VariablePrivate, _) -> Nothing

scopeShallowLookupFunction :: Bool -> String -> Scope -> Maybe Function
scopeShallowLookupFunction private name sc = case lookup name (scopeFunctions sc) of
  Nothing -> Nothing
  Just (VariableNormal, x) -> Just x
  Just (VariableConstant, x) -> Just x
  Just (VariablePrivate, x) | private -> Just x
  Just (VariablePrivate, _) -> Nothing

scopeShallowLookupAdverb :: Bool -> String -> Scope -> Maybe Adverb
scopeShallowLookupAdverb private name sc = case lookup name (scopeAdverbs sc) of
  Nothing -> Nothing
  Just (VariableNormal, x) -> Just x
  Just (VariableConstant, x) -> Just x
  Just (VariablePrivate, x) | private -> Just x
  Just (VariablePrivate, _) -> Nothing

scopeShallowLookupConjunction :: Bool -> String -> Scope -> Maybe Conjunction
scopeShallowLookupConjunction private name sc = case lookup name (scopeConjunctions sc) of
  Nothing -> Nothing
  Just (VariableNormal, x) -> Just x
  Just (VariableConstant, x) -> Just x
  Just (VariablePrivate, x) | private -> Just x
  Just (VariablePrivate, _) -> Nothing

scopeLookupNoun :: Bool ->  String -> Scope -> St (Maybe Noun)
scopeLookupNoun private name sc = case scopeShallowLookupNoun private name sc of
  Just x -> pure $ Just x
  Nothing -> if name `elem` specialNames then pure Nothing else case scopeParent sc of
    Nothing -> pure Nothing
    Just p -> (liftToSt $ IORef.readIORef p) >>= scopeLookupNoun private name

scopeLookupFunction :: Bool -> String -> Scope -> St (Maybe Function)
scopeLookupFunction private name sc = case scopeShallowLookupFunction private name sc of
  Just x -> pure $ Just x
  Nothing -> if name `elem` specialNames then pure $ Nothing else case scopeParent sc of
    Nothing -> pure $ Nothing
    Just p -> (liftToSt $ IORef.readIORef p) >>= scopeLookupFunction private name

scopeLookupAdverb :: Bool -> String -> Scope -> St (Maybe Adverb)
scopeLookupAdverb private name sc = case scopeShallowLookupAdverb private name sc of
  Just x -> pure $ Just x
  Nothing -> if name `elem` specialNames then pure $ Nothing else case scopeParent sc of
    Nothing -> pure $ Nothing
    Just p -> (liftToSt $ IORef.readIORef p) >>= scopeLookupAdverb private name

scopeLookupConjunction :: Bool -> String -> Scope -> St (Maybe Conjunction)
scopeLookupConjunction private name sc = case scopeShallowLookupConjunction private name sc of
  Just x -> pure $ Just x
  Nothing -> if name `elem` specialNames then pure $ Nothing else case scopeParent sc of
    Nothing -> pure $ Nothing
    Just p -> (liftToSt $ IORef.readIORef p) >>= scopeLookupConjunction private name

scopeUpdateNoun :: Bool ->  String -> VariableType -> Noun -> Scope -> St Scope
scopeUpdateNoun private name ty val sc = case lookup name (scopeNouns sc) of
  Nothing -> pure $ sc{ scopeNouns = update name (ty, val) (scopeNouns sc) }
  Just (VariableNormal, _) -> pure $ sc{ scopeNouns = update name (VariableNormal, val) (scopeNouns sc) }
  Just (VariablePrivate, _) | private -> pure $ sc{ scopeNouns = update name (VariablePrivate, val) (scopeNouns sc) }
  Just (VariablePrivate, _) -> throwError $ DomainError "Cannot access private variable"
  Just (VariableConstant, _) -> throwError $ DomainError "Cannot modify constant variable"

scopeUpdateFunction :: Bool -> String -> VariableType -> Function -> Scope -> St Scope
scopeUpdateFunction private name ty val sc = case lookup name (scopeFunctions sc) of
  Nothing -> pure $ sc{ scopeFunctions = update name (ty, val) (scopeFunctions sc) }
  Just (VariableNormal, _) -> pure $ sc{ scopeFunctions = update name (VariableNormal, val) (scopeFunctions sc) }
  Just (VariablePrivate, _) | private -> pure $ sc{ scopeFunctions = update name (VariablePrivate, val) (scopeFunctions sc) }
  Just (VariablePrivate, _) -> throwError $ DomainError "Cannot access private variable"
  Just (VariableConstant, _) -> throwError $ DomainError "Cannot modify constant variable"

scopeUpdateAdverb :: Bool -> String -> VariableType -> Adverb -> Scope -> St Scope
scopeUpdateAdverb private name ty val sc = case lookup name (scopeAdverbs sc) of
  Nothing -> pure $ sc{ scopeAdverbs = update name (ty, val) (scopeAdverbs sc) }
  Just (VariableNormal, _) -> pure $ sc{ scopeAdverbs = update name (VariableNormal, val) (scopeAdverbs sc) }
  Just (VariablePrivate, _) | private -> pure $ sc{ scopeAdverbs = update name (VariablePrivate, val) (scopeAdverbs sc) }
  Just (VariablePrivate, _) -> throwError $ DomainError "Cannot access private variable"
  Just (VariableConstant, _) -> throwError $ DomainError "Cannot modify constant variable"

scopeUpdateConjunction :: Bool -> String -> VariableType -> Conjunction -> Scope -> St Scope
scopeUpdateConjunction private name ty val sc = case lookup name (scopeConjunctions sc) of
  Nothing -> pure $ sc{ scopeConjunctions = update name (ty, val) (scopeConjunctions sc) }
  Just (VariableNormal, _) -> pure $ sc{ scopeConjunctions = update name (VariableNormal, val) (scopeConjunctions sc) }
  Just (VariablePrivate, _) | private -> pure $ sc{ scopeConjunctions = update name (VariablePrivate, val) (scopeConjunctions sc) }
  Just (VariablePrivate, _) -> throwError $ DomainError "Cannot access private variable"
  Just (VariableConstant, _) -> throwError $ DomainError "Cannot modify constant variable"

scopeModifyNoun :: Bool -> String -> Noun -> Scope -> St Scope
scopeModifyNoun private name val sc = if name `elem` specialNames then throwError $ DomainError "Cannot modify special variable" else case lookup name (scopeNouns sc) of
  Just (t, _) -> scopeUpdateNoun private name t val sc
  Nothing -> case scopeParent sc of
    Nothing -> throwError $ DomainError "Modifying a non-existent variable"
    Just p -> readRef p >>= scopeModifyNoun private name val >>= writeRef p >>= const (pure sc)

scopeModifyFunction :: Bool -> String -> Function -> Scope -> St Scope
scopeModifyFunction private name val sc = if name `elem` specialNames then throwError $ DomainError "Cannot modify special variable" else case lookup name (scopeFunctions sc) of
  Just _ -> scopeUpdateFunction private name VariableNormal val sc
  Nothing -> case scopeParent sc of
    Nothing -> throwError $ DomainError "Modifying a non-existent variable"
    Just p -> readRef p >>= scopeModifyFunction private name val >>= writeRef p >>= const (pure sc)

scopeModifyAdverb :: Bool -> String -> Adverb -> Scope -> St Scope
scopeModifyAdverb private name val sc = if name `elem` specialNames then throwError $ DomainError "Cannot modify special variable" else case lookup name (scopeAdverbs sc) of
  Just _ -> scopeUpdateAdverb private name VariableNormal val sc
  Nothing -> case scopeParent sc of
    Nothing -> throwError $ DomainError "Modifying a non-existent variable"
    Just p -> readRef p >>= scopeModifyAdverb private name val >>= writeRef p >>= const (pure sc)

scopeModifyConjunction :: Bool -> String -> Conjunction -> Scope -> St Scope
scopeModifyConjunction private name val sc = if name `elem` specialNames then throwError $ DomainError "Cannot modify special variable" else case lookup name (scopeConjunctions sc) of
  Just _ -> scopeUpdateConjunction private name VariableNormal val sc
  Nothing -> case scopeParent sc of
    Nothing -> throwError $ DomainError "Modifying a non-existent variable"
    Just p -> readRef p >>= scopeModifyConjunction private name val >>= writeRef p >>= const (pure sc)

scopeShallowModifyNoun :: Bool -> String -> Noun -> Scope -> St Scope
scopeShallowModifyNoun private name val sc = if name `elem` specialNames then throwError $ DomainError "Cannot modify special variable" else case lookup name (scopeNouns sc) of
  Just _ -> scopeUpdateNoun private name VariableNormal val sc
  Nothing -> throwError $ DomainError "Modifying a non-existent variable"

scopeShallowModifyFunction :: Bool -> String -> Function -> Scope -> St Scope
scopeShallowModifyFunction private name val sc = if name `elem` specialNames then throwError $ DomainError "Cannot modify special variable" else case lookup name (scopeFunctions sc) of
  Just _ -> scopeUpdateFunction private name VariableNormal val sc
  Nothing -> throwError $ DomainError "Modifying a non-existent variable"

scopeShallowModifyAdverb :: Bool -> String -> Adverb -> Scope -> St Scope
scopeShallowModifyAdverb private name val sc = if name `elem` specialNames then throwError $ DomainError "Cannot modify special variable" else case lookup name (scopeAdverbs sc) of
  Just _ -> scopeUpdateAdverb private name VariableNormal val sc
  Nothing -> throwError $ DomainError "Modifying a non-existent variable"

scopeShallowModifyConjunction :: Bool -> String -> Conjunction -> Scope -> St Scope
scopeShallowModifyConjunction private name val sc = if name `elem` specialNames then throwError $ DomainError "Cannot modify special variable" else case lookup name (scopeConjunctions sc) of
  Just _ -> scopeUpdateConjunction private name VariableNormal val sc
  Nothing -> throwError $ DomainError "Modifying a non-existent variable"

data Context = Context
  { contextScope :: IORef Scope
  , contextQuads :: Quads
  , contextIn :: St String
  , contextOut :: String -> St ()
  , contextErr :: String -> St ()
  , contextIncrementalId :: IORef Integer }

assignId :: St Integer
assignId = do
  idRef <- gets contextIncrementalId
  id <- readRef idRef
  writeRef idRef $ id + 1
  pure id

instance NFData Context where
  rnf (Context s q i o e d) = rnf s `seq` rnf q `seq` rwhnf i `seq` rnf o `seq` rnf e `seq` rnf d `seq` ()

type St = StateT Context (ExceptT Error IO)

runSt :: St a -> Context -> ResultIO (a, Context)
runSt = runStateT

runWithContext :: Context -> St a -> St a
runWithContext ctx f = do
  r <- liftToSt $ runExceptT $ runSt f ctx
  case r of
    Left e -> throwError e
    Right (x, _) -> pure x

liftToSt :: IO a -> St a
liftToSt = liftIO

getContext :: St Context
getContext = get

getsContext :: (Context -> a) -> St a
getsContext = gets

putContext :: Context -> St ()
putContext = put

putScope :: IORef Scope -> St ()
putScope sc = do
  context <- get
  put $ context{ contextScope = sc }

createRef :: a -> St (IORef a)
createRef = liftToSt . IORef.newIORef

readRef :: IORef a -> St a
readRef = liftToSt . IORef.readIORef

writeRef :: IORef a -> a -> St ()
writeRef = liftToSt .: IORef.writeIORef

modifyRef :: IORef a -> (a -> a) -> St ()
modifyRef = liftToSt .: IORef.modifyIORef

-- * Value

data Value
  = VNoun Noun
  | VFunction Function
  | VAdverb Adverb
  | VConjunction Conjunction
  deriving (Eq, Ord, Generic, NFData)

instance Show Value where
  show (VNoun n)           = show n
  show (VFunction fn)      = show fn
  show (VAdverb adv)       = show adv
  show (VConjunction conj) = show conj

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

-- * Core extra args

coreExtraArgsToleranceKey :: String
coreExtraArgsToleranceKey = "tolerance"

data CoreExtraArgs = CoreExtraArgs
  { coreExtraArgsTolerance :: Double }

parseCoreExtraArgs :: MonadError Error m => ExtraArgs -> m CoreExtraArgs
parseCoreExtraArgs ea = do
  let 
    lookupStr :: String -> Maybe ScalarValue
    lookupStr s = lookup (box $ vector $ Character <$> s) ea
  let toleranceErr = DomainError "Tolerance must be a scalar real number"
  tolerance <- fromMaybe comparisonTolerance <$> (mapM (asNumber toleranceErr >=> asReal toleranceErr) $ lookupStr coreExtraArgsToleranceKey)
  pure CoreExtraArgs
    { coreExtraArgsTolerance = tolerance }
