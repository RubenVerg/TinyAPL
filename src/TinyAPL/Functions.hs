{-# LANGUAGE FlexibleContexts, LambdaCase, NegativeLiterals, TupleSections #-}

module TinyAPL.Functions where

import TinyAPL.ArrayFunctionOperator
import TinyAPL.Error
import {-# SOURCE #-} TinyAPL.Interpreter
import TinyAPL.Random
import TinyAPL.Util
import TinyAPL.Glyphs (deltaBar)

import Control.Monad.Except (MonadError)
import qualified TinyAPL.Complex as Cx
import TinyAPL.Complex ( Complex((:+)) )
import Data.Char
import Data.Maybe (fromJust, fromMaybe)
import Data.List (elemIndex, genericLength, genericTake, genericDrop, genericReplicate, nub, genericIndex, sortOn, sort, find, singleton, nubBy)
import qualified Data.List.NonEmpty as NE
import Numeric.Natural (Natural)
import qualified Data.Bifunctor as Bi
import Control.Monad
import Control.Monad.State (MonadIO)
import Data.Ord (Down(..))
import qualified Data.Matrix as M
import qualified TinyAPL.Gamma.Gamma as Gamma
import Data.Foldable (foldlM, foldrM)
import qualified Data.Map.Strict as Map
import Math.NumberTheory.Primes (unPrime, UniqueFactorisation(factorise))

-- * Functions

orStruct1 :: String -> (ScalarValue -> St ScalarValue) -> ScalarValue -> St ScalarValue
orStruct1 name _ x@(Struct ctx) = do
  scope <- readRef $ contextScope ctx
  fn <- scopeLookupFunction False (deltaBar : name) scope
  case fn of
    Nothing -> throwError $ DomainError $ "Struct does not support operation " ++ name
    Just fn' -> toScalar <$> callMonad fn' [] (scalar x)
orStruct1 _ f x = f x

orStruct1EA :: String -> (CoreExtraArgs -> ScalarValue -> St ScalarValue) -> CoreExtraArgs -> ScalarValue -> St ScalarValue
orStruct1EA name _ cea x@(Struct ctx) = do
  scope <- readRef $ contextScope ctx
  fn <- scopeLookupFunction False (deltaBar : name) scope
  case fn of
    Nothing -> throwError $ DomainError $ "Struct does not support operation " ++ name
    Just fn' -> toScalar <$> callMonad fn' (reprCoreExtraArgs cea) (scalar x)
orStruct1EA _ f cea x = f cea x

orStruct2 :: String -> (ScalarValue -> ScalarValue -> St ScalarValue) -> ScalarValue -> ScalarValue -> St ScalarValue
orStruct2 name _ x@(Struct ctx) y = do
  scope <- readRef $ contextScope ctx
  fn <- scopeLookupFunction False (deltaBar : name) scope
  case fn of
    Nothing -> throwError $ DomainError $ "Struct does not support operation " ++ name
    Just fn' -> toScalar <$> callDyad fn' [] (scalar x) (scalar y)
orStruct2 name _ x y@(Struct ctx) = do
  scope <- readRef $ contextScope ctx
  fn <- scopeLookupFunction False (deltaBar : name) scope
  case fn of
    Nothing -> throwError $ DomainError $ "Struct does not support operation " ++ name
    Just fn' -> toScalar <$> callDyad fn' [] (scalar x) (scalar y)
orStruct2 _ f x y = f x y

orStruct2EA :: String -> (CoreExtraArgs -> ScalarValue -> ScalarValue -> St ScalarValue) -> CoreExtraArgs -> ScalarValue -> ScalarValue -> St ScalarValue
orStruct2EA name _ cea x@(Struct ctx) y = do
  scope <- readRef $ contextScope ctx
  fn <- scopeLookupFunction False (deltaBar : name) scope
  case fn of
    Nothing -> throwError $ DomainError $ "Struct does not support operation " ++ name
    Just fn' -> toScalar <$> callDyad fn' (reprCoreExtraArgs cea) (scalar x) (scalar y)
orStruct2EA name _ cea x y@(Struct ctx) = do
  scope <- readRef $ contextScope ctx
  fn <- scopeLookupFunction False (deltaBar : name) scope
  case fn of
    Nothing -> throwError $ DomainError $ "Struct does not support operation " ++ name
    Just fn' -> toScalar <$> callDyad fn' (reprCoreExtraArgs cea) (scalar x) (scalar y)
orStruct2EA _ f cea x y = f cea x y

expectedNumber = DomainError "Expected number"
expectedReal = DomainError "Expected real"
expectedInteger = DomainError "Expected integer"
expectedNatural = DomainError "Expected natural"
expectedBool = DomainError "Expected boolean"

conjugate :: MonadError Error m => ScalarValue -> m ScalarValue
conjugate (Number y) = pure $ Number $ Cx.conjugate y
conjugate _ = throwError expectedNumber

conjugateS :: ScalarValue -> St ScalarValue
conjugateS = orStruct1 "Conjugate" conjugate

conjugate' :: Noun -> St Noun
conjugate' = scalarMonad conjugateS

add :: MonadError Error m => ScalarValue -> ScalarValue -> m ScalarValue
add (Number x) (Number y) = pure $ Number $ x + y
add (Number x) (Character y) = do
  x' <- asInt expectedInteger x
  pure $ Character $ chr $ ord y + x'
add (Character x) (Number y) = do
  y' <- asInt expectedInteger y
  pure $ Character $ chr $ ord x + y'
add _ _ = throwError expectedNumber

addS :: ScalarValue -> ScalarValue -> St ScalarValue
addS = orStruct2 "Add" add

add' :: Noun -> Noun -> St Noun
add' = scalarDyad addS

addAna' :: Noun -> Noun -> St [Noun]
addAna' x y = do
  let err = DomainError "Add ana must receive a natural"
  n <- sub' y x >>= asScalar err >>= asNumber err >>= asNat err
  pure $ genericTake n $ Prelude.repeat $ scalar $ Number 1

neg :: MonadError Error m => ScalarValue -> m ScalarValue
neg (Number y) = pure $ Number $ negate y
neg _ = throwError expectedNumber

negS :: ScalarValue -> St ScalarValue
negS = orStruct1 "Negate" neg

neg' :: Noun -> St Noun
neg' = scalarMonad negS

sub :: MonadError Error m => ScalarValue -> ScalarValue -> m ScalarValue
sub (Number x) (Number y) = pure $ Number $ x - y
sub (Character x) (Number y) = do
  y' <- asInt expectedInteger y
  pure $ Character $ chr $ ord x - y'
sub (Character x) (Character y) = pure $ Number $ fromInteger . toInteger $ ord x - ord y
sub _ _ = throwError expectedNumber

subS :: ScalarValue -> ScalarValue -> St ScalarValue
subS = orStruct2 "Subtract" sub

sub' :: Noun -> Noun -> St Noun
sub' = scalarDyad subS

sign :: MonadError Error m => ScalarValue -> m ScalarValue
sign (Number y) = pure $ Number $ signum y
sign (Character y) = pure $ Number $ if isUpperCase y then 1 else if isLowerCase y then -1 else 0
sign _ = throwError expectedNumber

signS :: ScalarValue -> St ScalarValue
signS = orStruct1 "Sign" sign

sign' :: Noun -> St Noun
sign' = scalarMonad signS

times :: MonadError Error m => ScalarValue -> ScalarValue -> m ScalarValue
times z@(Number 0) _ = pure z
times _ z@(Number 0) = pure z
times (Number x) (Number y) = pure $ Number $ x * y
times _ _ = throwError expectedNumber

timesS :: ScalarValue -> ScalarValue -> St ScalarValue
timesS = orStruct2 "Times" times

times' :: Noun -> Noun -> St Noun
times' = scalarDyad timesS

timesAna' :: Noun -> Noun -> St [Noun]
timesAna' x y = do
  let err = DomainError "Times ana must receive a natural after division"
  n <- divide' y x >>= asScalar err >>= asNumber err >>= asNat err
  pure $ factorise n >>= (\(pr, ct) -> genericTake ct $ Prelude.repeat $ scalar $ Number $ (:+ 0) $ fromIntegral $ unPrime pr)

signAndAbs :: MonadError Error m => ScalarValue -> m (ScalarValue, ScalarValue)
signAndAbs y = liftA2 (,) (sign y) (TinyAPL.Functions.abs y)

signAndAbsS :: ScalarValue -> St (ScalarValue, ScalarValue)
signAndAbsS y = liftA2 (,) (signS y) (absS y)

signAndAbs' :: Noun -> St (Noun, Noun)
signAndAbs' y = liftA2 (,) (sign' y) (abs' y)

reciprocal :: MonadError Error m => ScalarValue -> m ScalarValue
reciprocal (Number 0) = throwError $ DomainError "Divide by zero"
reciprocal (Number y) = pure $ Number $ recip y
reciprocal _ = throwError expectedNumber

reciprocalS :: ScalarValue -> St ScalarValue
reciprocalS = orStruct1 "Reciprocal" reciprocal

reciprocal' :: Noun -> St Noun
reciprocal' = scalarMonad reciprocalS

divide :: MonadError Error m => ScalarValue -> ScalarValue -> m ScalarValue
divide (Number 0) (Number 0) = pure $ Number 1
divide _ (Number 0) = throwError $ DomainError "Divide by zero"
divide (Number x) (Number y) = pure $ Number $ x / y
divide _ _ = throwError expectedNumber

divideS :: ScalarValue -> ScalarValue -> St ScalarValue
divideS = orStruct2 "Divide" divide

divide' :: Noun -> Noun -> St Noun
divide' = scalarDyad divideS

halve :: MonadError Error m => ScalarValue -> m ScalarValue
halve y = divide y $ Number 2

halveS :: ScalarValue -> St ScalarValue
halveS y = divideS y $ Number 2

halve' :: Noun -> St Noun
halve' = scalarMonad halveS

ePow :: MonadError Error m => ScalarValue -> m ScalarValue
ePow (Number y) = pure $ Number $ exp y
ePow _ = throwError expectedNumber

ePowS :: ScalarValue -> St ScalarValue
ePowS = orStruct1 "Exp" ePow

ePow' :: Noun -> St Noun
ePow' = scalarMonad ePowS

pow :: MonadError Error m => ScalarValue -> ScalarValue -> m ScalarValue
pow (Number x) (Number y) = case asNat (DomainError "") y of
  Left _ -> pure $ Number $ x ** y
  Right y' -> pure $ Number $ x ^ y'
pow _ _ = throwError expectedNumber

powS :: ScalarValue -> ScalarValue -> St ScalarValue
powS = orStruct2 "Power" pow

pow' :: Noun -> Noun -> St Noun
pow' = scalarDyad powS

square :: MonadError Error m => ScalarValue -> m ScalarValue
square (Number y) = pure $ Number $ y * y
square _ = throwError expectedNumber

squareS :: ScalarValue -> St ScalarValue
squareS = orStruct1 "Square" square

square' :: Noun -> St Noun
square' = scalarMonad squareS

raises :: MonadError Error m => ScalarValue -> ScalarValue -> m ScalarValue
raises = flip pow

raisesS :: ScalarValue -> ScalarValue -> St ScalarValue
raisesS = orStruct2 "Raises" raises

raises' :: Noun -> Noun -> St Noun
raises' = scalarDyad raisesS

ln :: MonadError Error m => ScalarValue -> m ScalarValue
ln (Number 0) = throwError $ DomainError "Logarithm of zero"
ln (Number y) = pure $ Number $ Prelude.log y
ln _ = throwError expectedNumber

lnS :: ScalarValue -> St ScalarValue
lnS = orStruct1 "Ln" ln

ln' :: Noun -> St Noun
ln' = scalarMonad lnS

log :: MonadError Error m => ScalarValue -> ScalarValue -> m ScalarValue
log (Number 1) (Number 1) = pure $ Number 1
log (Number 1) _ = throwError $ DomainError "Logarithm base one"
log _ (Number 0) = throwError $ DomainError "Logarithm of zero"
log (Number x) (Number y) = pure $ Number $ logBase x y
log _ _ = throwError expectedNumber

logS :: ScalarValue -> ScalarValue -> St ScalarValue
logS = orStruct2 "Log" TinyAPL.Functions.log

log' :: Noun -> Noun -> St Noun
log' = scalarDyad logS

squareRoot :: MonadError Error m => ScalarValue -> m ScalarValue
squareRoot (Number y) = pure $ Number $ sqrt y
squareRoot _ = throwError expectedNumber

squareRootS :: ScalarValue -> St ScalarValue
squareRootS = orStruct1 "Sqrt" squareRoot

squareRoot' :: Noun -> St Noun
squareRoot' = scalarMonad squareRootS

root :: MonadError Error m => ScalarValue -> ScalarValue -> m ScalarValue
root (Number x) (Number y) = pure $ Number $ y ** recip x
root _ _ = throwError expectedNumber

rootS :: ScalarValue -> ScalarValue -> St ScalarValue
rootS = orStruct2 "Root" root

root' :: Noun -> Noun -> St Noun
root' = scalarDyad rootS

matrixInverse :: MonadError Error m => M.Matrix (Complex Double) -> m (M.Matrix (Complex Double))
matrixInverse y = do
  let hermitian = fmap Cx.conjugate . M.transpose
  case M.inverse (hermitian y * y) of
    Left err -> throwError $ DomainError err
    Right r -> pure $ r * hermitian y

matrixInverse' :: MonadError Error m => Noun -> m Noun
matrixInverse' = atRank1 defaultCoreExtraArgs (\y -> do
  r <- rank y
  mat <- asMatrix (DomainError "") y >>= mapM (asNumber (DomainError "Matrix inverse argument must be numeric"))
  inv <- fmap Number <$> matrixInverse (if r < 2 then M.transpose mat else mat)
  if r < 2 then pure $ Array (arrayShape y) (M.toList inv)
  else pure $ matrix inv) 2

matrixDivide :: MonadError Error m => M.Matrix (Complex Double) -> M.Matrix (Complex Double) -> m (M.Matrix (Complex Double))
matrixDivide x y = (* x) <$> matrixInverse y

matrixDivide' :: MonadError Error m => Noun -> Noun -> m Noun
matrixDivide' = atRank2 defaultCoreExtraArgs (\x y -> do
  x' <- asMatrix (DomainError "") x >>= mapM (asNumber (DomainError "Matrix divide arguments must be numeric"))
  y' <- asMatrix (DomainError "") y >>= mapM (asNumber (DomainError "Matrix divide arguments must be numeric")) 
  matrix . fmap Number <$> matrixDivide x' y') (2, 2)

floor :: MonadError Error m => CoreExtraArgs -> ScalarValue -> m ScalarValue
floor CoreExtraArgs{ coreExtraArgsTolerance = t } (Number y) = pure $ Number $ complexFloor' t y
floor _ (Character y) = pure $ Character $ toLower y
floor _ _ = throwError expectedNumber

floorS :: CoreExtraArgs -> ScalarValue -> St ScalarValue
floorS = orStruct1EA "Floor" TinyAPL.Functions.floor

floor' :: CoreExtraArgs -> Noun -> St Noun
floor' = scalarMonad . floorS

floorAndFrac :: MonadError Error m => CoreExtraArgs -> ScalarValue -> m (ScalarValue, ScalarValue)
floorAndFrac cea y = liftA2 (,) (TinyAPL.Functions.floor cea y) (remainder cea (Number 1) y)

floorAndFracS :: CoreExtraArgs -> ScalarValue -> St (ScalarValue, ScalarValue)
floorAndFracS cea y = liftA2 (,) (floorS cea y) (remainderS cea (Number 1) y)

floorAndFrac' :: CoreExtraArgs -> Noun -> St (Noun, Noun)
floorAndFrac' cea y = liftA2 (,) (floor' cea y) (remainder' cea (scalar $ Number 1) y)

ceil :: MonadError Error m => CoreExtraArgs -> ScalarValue -> m ScalarValue
ceil CoreExtraArgs{ coreExtraArgsTolerance = t } (Number y) = pure $ Number $ complexCeiling' t y
ceil _ (Character y) = pure $ Character $ toUpper y
ceil _ _ = throwError expectedNumber

ceilS :: CoreExtraArgs -> ScalarValue -> St ScalarValue
ceilS = orStruct1EA "Ceiling" ceil

ceil' :: CoreExtraArgs -> Noun -> St Noun
ceil' = scalarMonad . ceilS

round :: MonadError Error m => ScalarValue -> m ScalarValue
round (Number y) = pure $ Number $ componentFloor $ y + (0.5 :+ 0.5)
round _ = throwError expectedNumber

roundS :: ScalarValue -> St ScalarValue
roundS = orStruct1 "Round" TinyAPL.Functions.round

round' :: Noun -> St Noun
round' = scalarMonad roundS

roundTo :: MonadError Error m => ScalarValue -> ScalarValue -> m ScalarValue
roundTo = commute $ leftFork (TinyAPL.Functions.round `atop` divide) times

roundToS :: ScalarValue -> ScalarValue -> St ScalarValue
roundToS = orStruct2 "RoundTo" roundTo

roundTo' :: Noun -> Noun -> St Noun
roundTo' = scalarDyad roundToS

min :: MonadError Error m => ScalarValue -> ScalarValue -> m ScalarValue
min x y = pure $ Prelude.min x y

min' :: MonadError Error m => Noun -> Noun -> m Noun
min' = scalarDyad TinyAPL.Functions.min

max :: MonadError Error m => ScalarValue -> ScalarValue -> m ScalarValue
max x y = pure $ Prelude.max x y

max' :: MonadError Error m => Noun -> Noun -> m Noun
max' = scalarDyad TinyAPL.Functions.max

lcm :: MonadError Error m => CoreExtraArgs -> ScalarValue -> ScalarValue -> m ScalarValue
lcm CoreExtraArgs{ coreExtraArgsTolerance = t } (Number x) (Number y) = pure $ Number $ complexLCM' t x y
lcm _ _ _ = throwError expectedNumber

lcmS :: CoreExtraArgs -> ScalarValue -> ScalarValue -> St ScalarValue
lcmS = orStruct2EA "And" TinyAPL.Functions.lcm

lcm' :: CoreExtraArgs -> Noun -> Noun -> St Noun
lcm' = scalarDyad . lcmS

gcd :: MonadError Error m => CoreExtraArgs -> ScalarValue -> ScalarValue -> m ScalarValue
gcd CoreExtraArgs{ coreExtraArgsTolerance = t } (Number x) (Number y) = pure $ Number $ complexGCD' t x y
gcd _ _ _ = throwError expectedNumber

gcdS :: CoreExtraArgs -> ScalarValue -> ScalarValue -> St ScalarValue
gcdS = orStruct2EA "Or" TinyAPL.Functions.gcd

gcd' :: CoreExtraArgs -> Noun -> Noun -> St Noun
gcd' = scalarDyad . gcdS

nand :: MonadError Error m => ScalarValue -> ScalarValue -> m ScalarValue
nand (Number 0) (Number 0) = pure $ Number 1
nand (Number 0) (Number 1) = pure $ Number 1
nand (Number 1) (Number 0) = pure $ Number 1
nand (Number 1) (Number 1) = pure $ Number 0
nand _ _ = throwError expectedBool

nandS :: ScalarValue -> ScalarValue -> St ScalarValue
nandS = orStruct2 "Nand" nand

nand' :: Noun -> Noun -> St Noun
nand' = scalarDyad nandS

nor :: MonadError Error m => ScalarValue -> ScalarValue -> m ScalarValue
nor (Number 0) (Number 0) = pure $ Number 1
nor (Number 0) (Number 1) = pure $ Number 0
nor (Number 1) (Number 0) = pure $ Number 0
nor (Number 1) (Number 1) = pure $ Number 0
nor _ _ = throwError expectedBool

norS :: ScalarValue -> ScalarValue -> St ScalarValue
norS = orStruct2 "Nor" nor

nor' :: Noun -> Noun -> St Noun
nor' = scalarDyad norS

imaginary :: MonadError Error m => ScalarValue -> m ScalarValue
imaginary (Number y) = pure $ Number $ y * i
imaginary _ = throwError expectedNumber

imaginaryS :: ScalarValue -> St ScalarValue
imaginaryS = orStruct1 "Imaginary" imaginary

imaginary' :: Noun -> St Noun
imaginary' = scalarMonad imaginaryS

cartesian :: MonadError Error m => ScalarValue -> ScalarValue -> m ScalarValue
cartesian (Number x) (Number y) = pure $ Number $ x + y * i
cartesian _ _ = throwError expectedNumber

cartesianS :: ScalarValue -> ScalarValue -> St ScalarValue
cartesianS = orStruct2 "Cartesian" cartesian

cartesian' :: Noun -> Noun -> St Noun
cartesian' = scalarDyad cartesianS

unitPolar :: MonadError Error m => ScalarValue -> m ScalarValue
unitPolar (Number y) = pure $ Number $ exp $ i * y
unitPolar _ = throwError expectedNumber

unitPolarS :: ScalarValue -> St ScalarValue
unitPolarS = orStruct1 "UnitPolar" unitPolar

unitPolar' :: Noun -> St Noun
unitPolar' = scalarMonad unitPolarS

polar :: MonadError Error m => ScalarValue -> ScalarValue -> m ScalarValue
polar (Number x) (Number y) = pure $ Number $ x * exp (i * y)
polar _ _ = throwError expectedNumber

polarS :: ScalarValue -> ScalarValue -> St ScalarValue
polarS = orStruct2 "Polar" polar

polar' :: Noun -> Noun -> St Noun
polar' = scalarDyad polarS

abs :: MonadError Error m => ScalarValue -> m ScalarValue
abs (Number y) = pure $ Number $ Prelude.abs y
abs (Character y) = pure $ Character $ toLower $ toUpper y
abs _ = throwError expectedNumber

absS :: ScalarValue -> St ScalarValue
absS = orStruct1 "Abs" TinyAPL.Functions.abs

abs' :: Noun -> St Noun
abs' = scalarMonad absS

remainder :: MonadError Error m => CoreExtraArgs -> ScalarValue -> ScalarValue -> m ScalarValue
remainder CoreExtraArgs{ coreExtraArgsTolerance = t } (Number x) (Number y) = pure $ Number $ complexRemainder' t x y
remainder _ _ _ = throwError expectedNumber

remainderS :: CoreExtraArgs -> ScalarValue -> ScalarValue -> St ScalarValue
remainderS = orStruct2EA "Modulo" remainder

remainder' :: CoreExtraArgs -> Noun -> Noun -> St Noun
remainder' = scalarDyad . remainderS

phase :: MonadError Error m => ScalarValue -> m ScalarValue
phase (Number y) = pure $ Number $ Cx.phase y :+ 0
phase _ = throwError expectedNumber

phaseS :: ScalarValue -> St ScalarValue
phaseS = orStruct1 "Phase" phase

phase' :: Noun -> St Noun
phase' = scalarMonad phaseS

absAndPhase :: MonadError Error m => ScalarValue -> m (ScalarValue, ScalarValue)
absAndPhase y = liftA2 (,) (TinyAPL.Functions.abs y) (phase y)

absAndPhaseS :: ScalarValue -> St (ScalarValue, ScalarValue)
absAndPhaseS y = liftA2 (,) (absS y) (phaseS y)

absAndPhase' :: Noun -> St (Noun, Noun)
absAndPhase' y = liftA2 (,) (abs' y) (phase' y)

real :: MonadError Error m => ScalarValue -> m ScalarValue
real (Number y) = pure $ Number $ Cx.realPart y :+ 0
real _ = throwError expectedNumber

realS :: ScalarValue -> St ScalarValue
realS = orStruct1 "RealPart" real

real' :: Noun -> St Noun
real' = scalarMonad realS

imag :: MonadError Error m => ScalarValue -> m ScalarValue
imag (Number y) = pure $ Number $ Cx.imagPart y :+ 0
imag _ = throwError expectedNumber

imagS :: ScalarValue -> St ScalarValue
imagS = orStruct1 "ImaginaryPart" imag

imag' :: Noun -> St Noun
imag' = scalarMonad imagS

realAndImag :: MonadError Error m => ScalarValue -> m (ScalarValue, ScalarValue)
realAndImag y = liftA2 (,) (real y) (imag y)

realAndImagS :: ScalarValue -> St (ScalarValue, ScalarValue)
realAndImagS y = liftA2 (,) (realS y) (imagS y)

realAndImag' :: Noun -> St (Noun, Noun)
realAndImag' y = liftA2 (,) (real' y) (imag' y)

arctan :: MonadError Error m => ScalarValue -> ScalarValue -> m ScalarValue
arctan (Number x) (Number y) = pure $ Number $ Cx.phase (y + x * i) :+ 0
arctan _ _ = throwError expectedNumber

arctanS :: ScalarValue -> ScalarValue -> St ScalarValue
arctanS = orStruct2 "Arctan" arctan

arctan' :: Noun -> Noun -> St Noun
arctan' = scalarDyad arctanS

sine :: MonadError Error m => ScalarValue -> m ScalarValue
sine (Number y) = pure $ Number $ sin y
sine _ = throwError expectedNumber

sine' :: Noun -> St Noun
sine' = scalarMonad sine

cosine :: MonadError Error m => ScalarValue -> m ScalarValue
cosine (Number y) = pure $ Number $ cos y
cosine _ = throwError expectedNumber

cosine' :: Noun -> St Noun
cosine' = scalarMonad cosine

sineAndCosine :: MonadError Error m => ScalarValue -> m (ScalarValue, ScalarValue)
sineAndCosine y = liftA2 (,) (sine y) (cosine y)

sineAndCosine' :: Noun -> St (Noun, Noun)
sineAndCosine' y = liftA2 (,) (sine' y) (cosine' y)

not :: MonadError Error m => ScalarValue -> m ScalarValue
not (Number y) = pure $ Number $ 1 - y
not _ = throwError expectedNumber

notS :: ScalarValue -> St ScalarValue
notS = orStruct1 "Not" TinyAPL.Functions.not

not' :: Noun -> St Noun
not' = scalarMonad notS

increment :: MonadError Error m => ScalarValue -> m ScalarValue
increment (Number y) = pure $ Number $ y + 1
increment (Character y) = pure $ Character $ chr $ ord y + 1
increment _ = throwError expectedNumber

incrementS :: ScalarValue -> St ScalarValue
incrementS = orStruct1 "Increment" increment

increment' :: Noun -> St Noun
increment' = scalarMonad incrementS

decrement :: MonadError Error m => ScalarValue -> m ScalarValue
decrement (Number y) = pure $ Number $ y - 1
decrement (Character '\0') = pure $ Character '\0'
decrement (Character y) = pure $ Character $ chr $ ord y - 1
decrement _ = throwError expectedNumber

decrementS :: ScalarValue -> St ScalarValue
decrementS = orStruct1 "Decrement" decrement

decrement' :: Noun -> St Noun
decrement' = scalarMonad decrementS

span :: MonadError Error m => ScalarValue -> ScalarValue -> m ScalarValue
span (Number x) (Number y) = pure $ Number $ 1 + x - y
span _ _ = throwError expectedNumber

spanS :: ScalarValue -> ScalarValue -> St ScalarValue
spanS = orStruct2 "Span" TinyAPL.Functions.span

span' :: Noun -> Noun -> St Noun
span' = scalarDyad spanS

equal :: MonadError Error m => CoreExtraArgs -> ScalarValue -> ScalarValue -> m Bool
equal CoreExtraArgs{ coreExtraArgsTolerance = t } x y = pure $ equalsT t x y

equal' :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Noun
equal' cea = scalarDyad (fmap boolToScalar .: equal cea)

notEqual :: MonadError Error m => CoreExtraArgs -> ScalarValue -> ScalarValue -> m Bool
notEqual CoreExtraArgs{ coreExtraArgsTolerance = t } x y = pure $ notEqualT t x y

notEqual' :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Noun
notEqual' cea = scalarDyad (fmap boolToScalar .: notEqual cea)

less :: MonadError Error m => CoreExtraArgs -> ScalarValue -> ScalarValue -> m Bool
less CoreExtraArgs{ coreExtraArgsTolerance = t } x y = pure $ lessT t x y

less' :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Noun
less' cea = scalarDyad (fmap boolToScalar .: less cea)

lessEqual :: MonadError Error m => CoreExtraArgs -> ScalarValue -> ScalarValue -> m Bool
lessEqual CoreExtraArgs{ coreExtraArgsTolerance = t } x y = pure $ lessEqualT t x y

lessEqual' :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Noun
lessEqual' cea = scalarDyad (fmap boolToScalar .: lessEqual cea)

greaterEqual :: MonadError Error m => CoreExtraArgs -> ScalarValue -> ScalarValue -> m Bool
greaterEqual CoreExtraArgs{ coreExtraArgsTolerance = t } x y = pure $ greaterEqualT t x y

greaterEqual' :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Noun
greaterEqual' cea = scalarDyad (fmap boolToScalar .: greaterEqual cea)

greater :: MonadError Error m => CoreExtraArgs -> ScalarValue -> ScalarValue -> m Bool
greater CoreExtraArgs{ coreExtraArgsTolerance = t } x y = pure $ greaterT t x y

greater' :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Noun
greater' cea = scalarDyad (fmap boolToScalar .: greater cea)

precedes :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Bool
precedes CoreExtraArgs{ coreExtraArgsTolerance = t } x y = pure $ lessT t x y

precedes' :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Noun
precedes' cea x y = scalar . boolToScalar <$> precedes cea x y

precedesOrIdentical :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Bool
precedesOrIdentical CoreExtraArgs{ coreExtraArgsTolerance = t } x y = pure $ lessEqualT t x y

precedesOrIdentical' :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Noun
precedesOrIdentical' cea x y = scalar . boolToScalar <$> precedesOrIdentical cea x y

succeedsOrIdentical :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Bool
succeedsOrIdentical CoreExtraArgs{ coreExtraArgsTolerance = t } x y = pure $ greaterEqualT t x y

succeedsOrIdentical' :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Noun
succeedsOrIdentical' cea x y = scalar . boolToScalar <$> succeedsOrIdentical cea x y

succeeds :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Bool
succeeds CoreExtraArgs{ coreExtraArgsTolerance = t } x y = pure $ greaterT t x y

succeeds' :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Noun
succeeds' cea x y = scalar . boolToScalar <$> succeeds cea x y

minimal :: MonadError Error m => Noun -> Noun -> m Noun
minimal x y = pure $ Prelude.min x y

maximal :: MonadError Error m => Noun -> Noun -> m Noun
maximal x y = pure $ Prelude.max x y

identical :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Bool
identical CoreExtraArgs{ coreExtraArgsTolerance = t } x y = pure $ equalsT t x y

identical' :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Noun
identical' cea x y = scalar . boolToScalar <$> identical cea x y

notIdentical :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Bool
notIdentical CoreExtraArgs{ coreExtraArgsTolerance = t } x y = pure $ notEqualT t x y

notIdentical' :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Noun
notIdentical' cea x y = scalar . boolToScalar <$> notIdentical cea x y

tally :: MonadError Error m => Noun -> m Natural
tally y = pure $ genericLength $ majorCells y

tally' :: MonadError Error m => Noun -> m Noun
tally' y = scalar . Number . fromInteger . toInteger <$> tally y

unTally :: MonadError Error m => ScalarValue -> m Noun
unTally y = do
  let err = DomainError "Un tally must receive a natural"
  n <- asNumber err y >>= asNat err
  pure $ vector $ genericTake n $ Prelude.repeat $ Number 0

unTally' :: Noun -> St Noun
unTally' y = do
  let err = DomainError "Un tally argument must be scalar"
  asScalar err y >>= unTally

nubSieve :: (Ord a, MonadError Error m) => [a] -> m [Bool]
nubSieve ys = pure $ zipWith (\c idx -> fromJust (c `elemIndex` ys) == idx) ys [0..]

nubSieve' :: MonadError Error m => CoreExtraArgs -> Noun -> m Noun
nubSieve' CoreExtraArgs{ coreExtraArgsTolerance = t } arr@(Array _ _) = do
  nub <- nubSieve $ TolerantL t <$> majorCells arr
  pure $ vector $ boolToScalar <$> nub
nubSieve' CoreExtraArgs{ coreExtraArgsTolerance = t } (Dictionary _ vs) = vector . fmap boolToScalar <$> nubSieve (TolerantL t <$> vs)

shape :: MonadError Error m => Noun -> m [Natural]
shape (Array sh _) = pure sh
shape (Dictionary ks _) = pure [genericLength ks]

shape' :: MonadError Error m => Noun -> m Noun
shape' arr = do
  sh <- shape arr
  pure $ vector $ Number . fromInteger . toInteger <$> sh

reshape :: MonadError Error m => [Integer] -> Noun -> m Noun
reshape shape arr@(Array _ xs) = do
  let negative = TinyAPL.Util.count (< 0) shape
  if negative == 0 then case arrayReshaped (fromInteger . toInteger <$> shape) xs of
    Nothing -> throwError $ DomainError "Cannot reshape an empty array to a non-empty array"
    Just res -> pure res
  else if negative == 1 && -1 `elem` shape then do
    let bound = genericLength xs
    let known = product $ filter (>= 0) shape
    if known == 0 then throwError $ DomainError "Shape cannot contain both 0 and -1"
    else if bound `mod` known /= 0 then throwError $ DomainError "Shape is not a multiple of the bound of the array"
    else reshape ((\x -> if x == -1 then bound `div` known else x) <$> shape) arr
  else throwError $ DomainError "Invalid shape"
reshape _ (Dictionary _ _) = throwError $ DomainError "Dictionary cannot be reshaped"

reshape' :: MonadError Error m => Noun -> Noun -> m Noun
reshape' sh arr = do
  let err = DomainError "Shape must be an integer vector"
  shape <- asVector err sh >>= mapM (asNumber err >=> asInt err)
  reshape shape arr

unShape :: MonadError Error m => CoreExtraArgs -> [Natural] -> m Noun
unShape cea sh = indexGenerator cea (product sh) >>= reshape (fromIntegral <$> sh)

unShape' :: MonadError Error m => CoreExtraArgs -> Noun -> m Noun
unShape' cea arr = do
  let err = DomainError "Un Shape shape must be a natural vector"
  shape <- asVector err arr >>= mapM (asNumber err >=> asNat err)
  unShape cea shape

rank :: MonadError Error m => Noun -> m Natural
rank = pure . arrayRank

rank' :: MonadError Error m => Noun -> m Noun
rank' arr = scalar . Number . fromInteger . toInteger <$> rank arr

unRank :: MonadError Error m => CoreExtraArgs -> Natural -> m Noun
unRank CoreExtraArgs{ coreExtraArgsOrigin = o } rk = reshape (genericTake rk $ Prelude.repeat 0) (scalar $ Number $ (:+ 0) $ fromIntegral o)

unRank' :: MonadError Error m => CoreExtraArgs -> Noun -> m Noun
unRank' cea y = do
  let err = DomainError "Un Rank rank must be a natural"
  rk <- asScalar err y >>= asNumber err >>= asNat err
  unRank cea rk

promote :: MonadError Error m => Noun -> m Noun
promote arr = reshape (1 : map toInteger (arrayShape arr)) arr

demote :: MonadError Error m => Noun -> m Noun
demote arr@(Array sh cs) = case (toInteger <$> sh, cs) of
  ([], _) -> pure arr
  ([_], []) -> throwError $ DomainError "Demote empty vector to scalar"
  ([_], c:_) -> pure $ scalar c
  (a:b:ss, _) -> reshape (a * b : ss) arr
demote (Dictionary _ _) = throwError $ DomainError "Dictionary cannot be demoted"

rerank :: MonadError Error m => Natural -> Noun -> m Noun
rerank n arr =
  if arrayRank arr == n then pure arr
  else if arrayRank arr > n then demote arr >>= rerank n
  else promote arr >>= rerank n

rerank' :: MonadError Error m => Noun -> Noun -> m Noun
rerank' narr arr = do
  let err = DomainError "Rerank left argument must be a scalar natural"
  n <- asScalar err narr >>= asNumber err >>= asNat err
  rerank n arr

ravel :: MonadError Error m => Noun -> m [ScalarValue]
ravel (Array _ cs) = pure cs
ravel (Dictionary _ vs) = pure vs

ravel' :: MonadError Error m => Noun -> m Noun
ravel' = fmap vector . ravel

enlist :: MonadError Error m => Noun -> m [ScalarValue]
enlist (Array [] [Box a]) = enlist a
enlist (Array [] sc) = pure sc
enlist (Array _ cs) = concat <$> mapM (enlist . fromScalar) cs
enlist (Dictionary _ vs) = enlist $ vector vs

enlist' :: MonadError Error m => Noun -> m Noun
enlist' y = vector <$> enlist y

depth :: MonadError Error m => Noun -> m Natural
depth arr = pure $ arrayDepth arr

depth' :: MonadError Error m => Noun -> m Noun
depth' = fmap (scalar . Number . fromInteger . toInteger) . depth

reverse :: MonadError Error m => [a] -> m [a]
reverse = pure . Prelude.reverse

reverse' :: MonadError Error m => Noun -> m Noun
reverse' = onMajorCells TinyAPL.Functions.reverse

rotate :: MonadError Error m => CoreExtraArgs -> [Integer] -> Noun -> m Noun
rotate _ [] xs = pure xs
rotate _ _ sc@(Array [] _) = pure sc
rotate cea@CoreExtraArgs{ coreExtraArgsFill = Nothing } (r:rs) xs = fromMajorCells . TinyAPL.Util.rotate r <$> mapM (TinyAPL.Functions.rotate cea rs) (majorCells xs)
rotate cea@CoreExtraArgs{ coreExtraArgsFill = Just fill } (r:rs) xs = do
  re <- fromMajorCells . (if r < 0 then Prelude.reverse . genericDrop (negate r) . Prelude.reverse else genericDrop r) <$> mapM (TinyAPL.Functions.rotate cea rs) (majorCells xs)
  if r < 0 then reverse' re >>= reverse' . fillArray (arrayShape xs) fill else pure $ fillArray (arrayShape xs) fill re

rotate' :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Noun
rotate' cea rot arr = do
  let err = DomainError "Rotate left argument must be an integer vector or scalar"
  rs <- asVector err rot >>= mapM (asNumber err >=> asInt err)
  TinyAPL.Functions.rotate cea rs arr

take :: MonadError Error m => CoreExtraArgs -> [Integer] -> Noun -> m Noun
take _ [] xs = pure xs
take cea@CoreExtraArgs{ coreExtraArgsBackward = True } ts xs = TinyAPL.Functions.take cea{ coreExtraArgsBackward = False } (negate <$> ts) xs
take cea@CoreExtraArgs{ coreExtraArgsFill = Nothing } (t:ts) xs = do
  let take' c = if c < 0 then Prelude.reverse . genericTake (negate c) . Prelude.reverse else genericTake c
  fromMajorCells . take' t <$> mapM (TinyAPL.Functions.take cea ts) (majorCells xs)
take cea@CoreExtraArgs{ coreExtraArgsFill = Just fill } ts'@(t:ts) xs = do
  r <- fromMajorCells <$> mapM (TinyAPL.Functions.take cea ts) (majorCells xs)
  let finalShape = (fromInteger . Prelude.abs <$> ts') ++ Prelude.drop (length ts') (arrayShape r)
  if t < 0 then reverse' r >>= reverse' . fillArray finalShape fill else pure $ fillArray finalShape fill r

take' :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Noun
take' cea tak arr = do
  let err = DomainError "Take left argument must be an integer vector"
  ts <- asVector err tak >>= mapM (asNumber err >=> asInt err)
  TinyAPL.Functions.take cea ts arr

drop :: MonadError Error m => CoreExtraArgs -> [Integer] -> Noun -> m Noun
drop _ [] xs = pure xs
drop cea@CoreExtraArgs{ coreExtraArgsBackward = True } ts xs = TinyAPL.Functions.drop cea{ coreExtraArgsBackward = False } (negate <$> ts) xs
drop cea (d:ds) xs = let
  drop' c = if c < 0 then Prelude.reverse . genericDrop (negate c) . Prelude.reverse else genericDrop c
  in fromMajorCells . drop' d <$> mapM (TinyAPL.Functions.drop cea ds) (majorCells xs)

drop' :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Noun
drop' cea dro arr = do
  let err = DomainError "Drop left argument must be an integer vector"
  ds <- asVector err dro >>= mapM (asNumber err >=> asInt err)
  TinyAPL.Functions.drop cea ds arr

enclose :: MonadError Error m => Noun -> m ScalarValue
enclose y = pure $ box y

enclose' :: MonadError Error m => Noun -> m Noun
enclose' = fmap scalar . enclose

singleton :: MonadError Error m => Noun -> m Noun
singleton y = pure $ vector [box y]

pair :: MonadError Error m => Noun -> Noun -> m Noun
pair x y = pure $ vector $ box <$> [x, y]

keyValuePair :: MonadError Error m => Noun -> Noun -> m Noun
keyValuePair k v = pure $ dictionary [(box k, box v)]

fromPairs :: MonadError Error m => Noun -> m Noun
fromPairs (Array [_] xs) = do
  let err = DomainError "From Pairs argument must be a 2-column matrix or vector of pairs or dictionaries"
  dictionary . concat <$> mapM (\x -> do
    let x' = fromScalar x
    case x' of
      (Dictionary ks vs) -> pure $ zip ks vs
      (Array [2] [a, b]) -> pure $ [(a, b)]
      _ -> throwError err) xs
fromPairs arr@(Array [_, 2] _) = majorCells' arr >>= fromPairs
fromPairs _ = throwError $ DomainError "From Pairs argument must be a 2-column matrix or vector of pairs or dictionaries"

fromKeysAndValues :: MonadError Error m => [ScalarValue] -> [ScalarValue] -> m Noun
fromKeysAndValues ks vs
  | length ks == length vs = pure $ Dictionary ks vs
  | otherwise = throwError $ LengthError "From Keys and Values arguments must have the same length"

fromKeysAndValues' :: MonadError Error m => Noun -> Noun -> m Noun
fromKeysAndValues' k v = do
  let err = DomainError "From Keys and Values arguments must be vectors"
  ks <- asVector err k
  vs <- asVector err v
  fromKeysAndValues ks vs

fromInvertedTable :: MonadError Error m => Noun -> m Noun
fromInvertedTable (Array [2] [k, v]) = do
  let err = DomainError "From Inverted Table argument must be a pair of vectors"
  ks <- asVector err $ fromScalar k
  vs <- asVector err $ fromScalar v
  fromKeysAndValues ks vs
fromInvertedTable es@(Array [2, _] _) = do
  let err = DomainError "From Inverted Table argument must be a pair of vectors"
  let [k, v] = majorCells es
  ks <- asVector err k
  vs <- asVector err v
  fromKeysAndValues ks vs
fromInvertedTable _ = throwError $ DomainError "From Inverted Table argument must be a pair of vectors"

first :: MonadError Error m => CoreExtraArgs -> Noun -> m Noun
first CoreExtraArgs{ coreExtraArgsFill = Nothing } (Array _ []) = throwError $ DomainError "First on empty array"
first CoreExtraArgs{ coreExtraArgsFill = Just fill } (Array _ []) = pure $ fromScalar fill
first _ (Array _ (x:_)) = pure $ fromScalar x
first _ (Dictionary ks _) = pure $ vector ks

last :: MonadError Error m => CoreExtraArgs -> Noun -> m Noun
last CoreExtraArgs{ coreExtraArgsFill = Nothing } (Array _ []) = throwError $ DomainError "Last on empty array"
last CoreExtraArgs{ coreExtraArgsFill = Just fill } (Array _ []) = pure $ fromScalar fill
last _ (Array _ xs) = pure $ fromScalar $ Prelude.last xs
last _ (Dictionary _ vs) = pure $ vector vs

firstAndLast :: MonadError Error m => CoreExtraArgs -> Noun -> m (Noun, Noun)
firstAndLast cea y = liftA2 (,) (first cea y) (TinyAPL.Functions.last cea y)

firstCell :: MonadError Error m => CoreExtraArgs -> Noun -> m Noun
firstCell CoreExtraArgs{ coreExtraArgsFill = Nothing } (Array _ []) = throwError $ DomainError "First cell on empty array"
firstCell CoreExtraArgs{ coreExtraArgsFill = Just fill } (Array _ []) = pure $ fromScalar fill
firstCell _ arr@Array{} = pure $ headPromise $ majorCells arr
firstCell _ _ = throwError $ DomainError "First cell on dictionary"

lastCell :: MonadError Error m => CoreExtraArgs -> Noun -> m Noun
lastCell CoreExtraArgs{ coreExtraArgsFill = Nothing } (Array _ []) = throwError $ DomainError "Last cell on empty array"
lastCell CoreExtraArgs{ coreExtraArgsFill = Just fill } (Array _ []) = pure $ fromScalar fill
lastCell _ arr@Array{} = pure $ Prelude.last $ majorCells arr
lastCell _ _ = throwError $ DomainError "Last cell on dictionary"

firstAndLastCells :: MonadError Error m => CoreExtraArgs -> Noun -> m (Noun, Noun)
firstAndLastCells cea y = liftA2 (,) (firstCell cea y) (lastCell cea y)

indexGenerator :: MonadError Error m => CoreExtraArgs -> Natural -> m Noun
indexGenerator _ 0 = pure $ vector []
indexGenerator CoreExtraArgs { coreExtraArgsOrigin = o, coreExtraArgsBackward = b } i = pure $ vector $ (if b then Prelude.reverse else id) $ Number . fromInteger . toInteger . (+ o) <$> [0..i - 1]

indexGeneratorN :: MonadError Error m => CoreExtraArgs -> [Natural] -> m Noun
indexGeneratorN CoreExtraArgs { coreExtraArgsOrigin = o, coreExtraArgsBackward = b } is = pure $ fromJust $ arrayReshaped is $ (if b then Prelude.reverse else id) $ box . fromJust . arrayReshaped [genericLength is] . fmap (Number . fromInteger . toInteger . (+ o)) <$> generateIndices is

indexGenerator' :: MonadError Error m => CoreExtraArgs -> Noun -> m Noun
indexGenerator' cea arr = do
  let err = DomainError "Index Generator argument must be a natural scalar or vector"
  is <- asVector err arr >>= mapM (asNumber err >=> asNat err)
  if isScalar arr then indexGenerator cea $ headPromise is
  else indexGeneratorN cea is

range :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Noun
range cea = commute $ (indexGenerator' cea{ coreExtraArgsOrigin = 0 } `atop` (scalarDyad TinyAPL.Functions.span)) `leftFork` eachLeft (scalarDyad add)

oneRange :: MonadError Error m => CoreExtraArgs -> Noun -> m Noun
oneRange cea = range cea $ scalar $ Number 1

replicate :: MonadError Error m => [Natural] -> [a] -> m [a]
replicate [] [] = pure []
replicate (r:rs) (x:xs) = (genericReplicate r x ++) <$> TinyAPL.Functions.replicate rs xs
replicate _ _ = throwError $ LengthError "Replicate with different lengths"

replicateDict :: MonadError Error m => Eq a => [(a, Bool)] -> [(a, b)] -> m [(a, b)]
replicateDict sels xs = pure $ filter (\(k, _) -> fromMaybe False $ lookup k sels) xs

replicate' :: MonadError Error m => Noun -> Noun -> m Noun
replicate' rs@(Array _ _) arr@(Array _ _) = do
  let err = DomainError "Replicate left argument must be a natural vector or scalar"
  rs' <- asVector err rs >>= mapM (asNumber err >=> asNat err)
  let cells = majorCells arr
  rs'' <- if isScalar rs then case rs' of [r] -> pure $ Prelude.replicate (length cells) r; _ -> throwError unreachable else pure rs'
  fromMajorCells <$> TinyAPL.Functions.replicate rs'' cells
replicate' (Dictionary sk sv) (Dictionary ak av) = do
  let err = DomainError "Replicate left argument must be a dictionary with boolean values"
  sv' <- mapM (asBool err) sv
  dictionary <$> replicateDict (zip sk sv') (zip ak av)
replicate' _ _ = throwError $ DomainError "Replicate left argument cannot mix arrays and dictionaries"

indices :: MonadError Error m => CoreExtraArgs -> Noun -> m Noun
indices CoreExtraArgs { coreExtraArgsOrigin = o } n = do
  let err = DomainError "Where argument must be an array of naturals or dictionary with natural values"
  case n of
    (Array sh cs) -> do
      let indices = generateIndices sh
      let shape = [genericLength sh | length sh /= 1]
      let rep idx c = genericReplicate c $ box $ fromJust $ arrayReshaped shape $ Number . fromInteger . toInteger . (+ o) <$> idx
      counts <- mapM (asNumber err >=> asNat err) cs
      pure $ vector $ concat $ zipWith rep indices counts
    (Dictionary ks vs) -> do
      counts <- mapM (asNumber err >=> asNat err) vs
      let rep key c = genericReplicate c key
      pure $ vector $ concat $ zipWith rep ks counts

unique :: (Eq a, MonadError Error m) => [a] -> m [a]
unique xs = pure $ nub xs

unique' :: MonadError Error m => CoreExtraArgs -> Noun -> m Noun
unique' CoreExtraArgs{ coreExtraArgsTolerance = t } arr@(Array _ _) = fromMajorCells . fmap unTolerantL <$> unique (TolerantL t <$> majorCells arr)
unique' CoreExtraArgs{ coreExtraArgsTolerance = t } (Dictionary _ vs) = vector . fmap unTolerantL <$> unique (TolerantL t <$> vs)

union :: (Eq a, MonadError Error m) => [a] -> [a] -> m [a]
union xs ys = pure $ xs ++ filter (Prelude.not . (`elem` xs)) ys

union' :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Noun
union' CoreExtraArgs{ coreExtraArgsTolerance = t } x@(Array _ _) y@(Array _ _) = fromMajorCells . fmap unTolerantL <$> union (TolerantL t <$> majorCells x) (TolerantL t <$> majorCells y)
union' CoreExtraArgs{ coreExtraArgsTolerance = t } (Dictionary aks avs) (Dictionary bks bvs) = pure $ dictionary $ nubBy ((==) `on` TolerantL t `on` fst) $ zip aks avs ++ zip bks bvs
union' _ _ _ = throwError $ DomainError "Cannot union array and dictionary"

intersection :: (Eq a, MonadError Error m) => [a] -> [a] -> m [a]
intersection xs ys = pure $ filter (`elem` ys) xs

intersection' :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Noun
intersection' CoreExtraArgs{ coreExtraArgsTolerance = t } x@(Array _ _) y@(Array _ _) = fromMajorCells . fmap unTolerantL <$> intersection (TolerantL t <$> majorCells x) (TolerantL t <$> majorCells y)
intersection' CoreExtraArgs{ coreExtraArgsTolerance = t } (Dictionary aks avs) (Dictionary bks bvs) = pure $ dictionary $ fmap (Bi.first unTolerantL) $ filter (\(k, _) -> k `elem` (TolerantL t <$> aks) && k `elem` (TolerantL t <$> bks)) $ zip (TolerantL t <$> aks) avs ++ zip (TolerantL t <$> bks) bvs
intersection' _ _ _ = throwError $ DomainError "Cannot intersect array and dictionary"

difference :: (Eq a, MonadError Error m) => [a] -> [a] -> m [a]
difference xs ys = pure $ filter (Prelude.not . (`elem` ys)) xs

difference' :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Noun
difference' CoreExtraArgs{ coreExtraArgsTolerance = t } x@(Array _ _) y@(Array _ _) = fromMajorCells . fmap unTolerantL <$> difference (TolerantL t <$> majorCells x) (TolerantL t <$> majorCells y)
difference' CoreExtraArgs{ coreExtraArgsTolerance = t } (Dictionary aks avs) (Dictionary bks bvs) = pure $ dictionary $ fmap (Bi.first unTolerantL) $ filter (\(k, _) -> k `elem` (TolerantL t <$> aks) && Prelude.not (k `elem` (TolerantL t <$> bks))) $ zip (TolerantL t <$> aks) avs ++ zip (TolerantL t <$> bks) bvs
difference' _ _ _ = throwError $ DomainError "Cannot difference array and dictionary"

symmetricDifference :: (Eq a, MonadError Error m) => [a] -> [a] -> m [a]
symmetricDifference xs ys = do
  a <- difference xs ys
  b <- difference ys xs
  pure $ a ++ b

symmetricDifference' :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Noun
symmetricDifference' CoreExtraArgs{ coreExtraArgsTolerance = t } x@(Array _ _) y@(Array _ _) = fromMajorCells . fmap unTolerantL <$> symmetricDifference (TolerantL t <$> majorCells x) (TolerantL t <$> majorCells y)
symmetricDifference' CoreExtraArgs{ coreExtraArgsTolerance = t } (Dictionary aks avs) (Dictionary bks bvs) = pure $ dictionary $ fmap (Bi.first unTolerantL) $ filter (\(k, _) -> (k `elem` (TolerantL t <$> aks)) /= (k `elem` (TolerantL t <$> bks))) $ zip (TolerantL t <$> aks) avs ++ zip (TolerantL t <$> bks) bvs
symmetricDifference' _ _ _ = throwError $ DomainError "Cannot symmetric difference array and dictionary"

roll :: (MonadError Error m, MonadIO m) => CoreExtraArgs -> Natural -> m Double
roll CoreExtraArgs { coreExtraArgsOrigin = o } y =
  if y == 0 then randomR (0, 1)
  else fromInteger . (+ toInteger o) <$> randomR (0, toInteger y - 1)

roll' :: (MonadError Error m, MonadIO m) => CoreExtraArgs -> Noun -> m Noun
roll' cea = scalarMonad $ \y -> do
  n <- asNumber expectedNatural y >>= asNat expectedNatural
  Number . (:+ 0) <$> roll cea n

deal :: (MonadError Error m, MonadIO m) => CoreExtraArgs -> Natural -> Natural -> m [Natural]
deal CoreExtraArgs { coreExtraArgsOrigin = o } count max
  | count > max = throwError $ DomainError "Deal left must be less than or equal to right argument"
  | otherwise = do
    let go 0 _ = pure []
        go n xs = do
          index <- randomR (0, length xs - 1)
          let num = xs !! index
          (num :) <$> go (n - 1) (filter (/= num) xs)
    go count $ fmap (+ o) [0..max-1]

deal' :: (MonadError Error m, MonadIO m) => CoreExtraArgs -> Noun -> Noun -> m Noun
deal' cea shape max = do
  let err = DomainError "Deal arguments must be natural numbers"
  s <- asVector err shape >>= mapM (asNumber err >=> asNat err)
  m <- asScalar err max >>= asNumber err >>= asNat err
  Array s . fmap (Number . (:+ 0) . fromInteger . toInteger) <$> deal cea (product s) m

indexCell :: MonadError Error m => CoreExtraArgs -> Integer -> Noun -> m Noun
indexCell CoreExtraArgs { coreExtraArgsOrigin = o, coreExtraArgsFill = f } i x@(Array _ _)
  | i < 0 = indexCell defaultCoreExtraArgs (genericLength (majorCells x) + i) x
  | i - toInteger o >= genericLength (majorCells x) = case f of
    Just f' -> pure $ fromScalar f'
    Nothing -> throwError $ IndexError "Index out of bounds"
  | otherwise = pure $ genericIndex (majorCells x) (i - toInteger o)
indexCell _ _ (Dictionary _ _) = throwError $ DomainError "Dictionary cannot be cell-indexed"

indexElement :: MonadError Error m => ScalarValue -> Noun -> m (Maybe ScalarValue)
indexElement _ (Array _ _) = throwError $ DomainError "Array cannot be element-indexed"
indexElement i (Dictionary ks vs) = pure $ lookup i $ zip ks vs

squad :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Noun
squad cea i y@(Array _ _) = do
  let err = DomainError "Squad left argument must be a vector of arrays of integers"
  axisIndices <- fmap fromScalar <$> asVector err i
  let
    go :: MonadError Error m => [Noun] -> Noun -> m Noun
    go [] y = pure y
    go (is:iss) y =
      onScalars1 defaultCoreExtraArgs (\(Array [] [ind]) -> asNumber err ind >>= asInt err >>= flip (indexCell cea) y >>= go iss) is
  go axisIndices y
squad CoreExtraArgs{ coreExtraArgsFill = f } i d@(Dictionary _ _) = indexElement (toScalar i) d >>= (\case
  Just r -> pure $ fromScalar r
  Nothing -> case f of
    Just f' -> pure $ fromScalar f'
    Nothing -> throwError $ IndexError "Key not found in dictionary")

from :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Noun
from cea x y = onScalars1 defaultCoreExtraArgs (\x' -> (first defaultCoreExtraArgs `before` squad cea) x' y) x

catenate :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Noun
catenate cea@CoreExtraArgs{ coreExtraArgsFill = fill } a@(Array ash acs) b@(Array bsh bcs) =
  if null acs && Prelude.not (null bcs) then pure b
  else if null bcs && Prelude.not (null acs) then pure a
  else if arrayRank a == arrayRank b then
    if (isScalar a && isScalar b) || (fill /= Nothing || tailMaybe ash == tailMaybe bsh) then pure $ fromMajorCellsMaybeFilled fill $ majorCells a ++ majorCells b
    else throwError $ LengthError "Incompatible shapes to Catenate"
  else if isScalar a then catenate cea (fromJust $ arrayReshaped (1 : tailPromise bsh) acs) b
  else if isScalar b then catenate cea a (fromJust $ arrayReshaped (1 : tailPromise ash) bcs)
  else if arrayRank a == arrayRank b + 1 then promote b >>= (\b' -> catenate cea a b')
  else if arrayRank a + 1 == arrayRank b then promote a >>= (\a' -> catenate cea a' b)
  else throwError $ RankError "Incompatible ranks to Catenate"
catenate cea a@(Dictionary _ _) b@(Dictionary _ _) = union' cea b a -- intentionally swapped
catenate _ (Array [_] []) d@(Dictionary _ _) = pure d
catenate _ d@(Dictionary _ _) (Array [_] []) = pure d
catenate _ _ _ = throwError $ DomainError "Cannot catenate dictionary and array"

join :: MonadError Error m => CoreExtraArgs -> [Noun] -> m Noun
join cea = fold (catenate cea `after` first defaultCoreExtraArgs) (vector [])

join' :: MonadError Error m => CoreExtraArgs -> Noun -> m Noun
join' cea = TinyAPL.Functions.join cea . majorCells

gradeUp :: MonadError Error m => Ord a => CoreExtraArgs -> [a] -> m [Natural]
gradeUp cea@CoreExtraArgs{ coreExtraArgsBackward = True } xs = gradeDown cea{ coreExtraArgsBackward = False } xs
gradeUp CoreExtraArgs{ coreExtraArgsOrigin = o } xs = pure $ map ((+ o) . fst) $ sortOn snd $ zip [0..genericLength xs] xs

gradeUp' :: MonadError Error m => CoreExtraArgs -> Noun -> m Noun
gradeUp' cea@CoreExtraArgs{ coreExtraArgsTolerance = t } arr@(Array _ _) = vector . fmap (Number . fromInteger . toInteger) <$> gradeUp cea (TolerantL t <$> majorCells arr)
gradeUp' CoreExtraArgs{ coreExtraArgsTolerance = t } (Dictionary ks vs) = vector . fmap unTolerantL <$> sortByUp (TolerantL t <$> ks) (TolerantL t <$> vs)

gradeDown :: MonadError Error m => Ord a => CoreExtraArgs -> [a] -> m [Natural]
gradeDown cea@CoreExtraArgs{ coreExtraArgsBackward = True } xs = gradeUp cea{ coreExtraArgsBackward = False } xs
gradeDown CoreExtraArgs{ coreExtraArgsOrigin = o } xs = pure $ map ((+ o) . fst) $ sortOn snd $ zip [0..genericLength xs] (Down <$> xs)

gradeDown' :: MonadError Error m => CoreExtraArgs -> Noun -> m Noun
gradeDown' cea@CoreExtraArgs{ coreExtraArgsTolerance = t } arr@(Array _ _) = vector . fmap (Number . fromInteger . toInteger) <$> gradeDown cea (TolerantL t <$> majorCells arr)
gradeDown' CoreExtraArgs{ coreExtraArgsTolerance = t } (Dictionary ks vs) = vector . fmap unTolerantL <$> sortByDown (TolerantL t <$> ks) (TolerantL t <$> vs)

sortByUp :: MonadError Error m => Ord b => [a] -> [b] -> m [a]
sortByUp as bs = pure $ map fst $ sortOn snd $ zip as bs

sortByUp' :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Noun
sortByUp' cea@CoreExtraArgs{ coreExtraArgsBackward = True } as bs = sortByDown' cea{ coreExtraArgsBackward = False } as bs
sortByUp' CoreExtraArgs{ coreExtraArgsTolerance = t } as@(Array _ _) bs@(Array _ _) = fromMajorCells . fmap unTolerantL <$> sortByUp (TolerantL t <$> majorCells as) (TolerantL t <$> majorCells bs)
sortByUp' _ _ _ = throwError $ DomainError "Only arrays can be sorted"

sortByDown :: MonadError Error m => Ord b => [a] -> [b] -> m [a]
sortByDown as bs = pure $ map fst $ sortOn snd $ zip as $ Down <$> bs

sortByDown' :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Noun
sortByDown' cea@CoreExtraArgs{ coreExtraArgsBackward = True } as bs = sortByUp' cea{ coreExtraArgsBackward = False } as bs
sortByDown' CoreExtraArgs{ coreExtraArgsTolerance = t } as@(Array _ _) bs@(Array _ _) = fromMajorCells . fmap unTolerantL <$> sortByDown (TolerantL t <$> majorCells as) (TolerantL t <$> majorCells bs)
sortByDown' _ _ _ = throwError $ DomainError "Only arrays can be sorted"

sortUp :: MonadError Error m => Ord a =>[a] -> m [a]
sortUp = pure . sort

sortUp' :: MonadError Error m => CoreExtraArgs -> Noun -> m Noun
sortUp' cea@CoreExtraArgs{ coreExtraArgsBackward = True } arr = sortDown' cea{ coreExtraArgsBackward = False } arr
sortUp' CoreExtraArgs{ coreExtraArgsTolerance = t } arr@(Array _ _) = fromMajorCells . fmap unTolerantL <$> sortUp (TolerantL t <$> majorCells arr)
sortUp' _ _ = throwError $ DomainError "Only arrays can be sorted"

sortDown :: MonadError Error m => Ord a => [a] -> m [a]
sortDown = pure . fmap getDown . sort . fmap Down

sortDown' :: MonadError Error m => CoreExtraArgs -> Noun -> m Noun
sortDown' cea@CoreExtraArgs{ coreExtraArgsBackward = True } arr = sortUp' cea{ coreExtraArgsBackward = False } arr
sortDown' CoreExtraArgs{ coreExtraArgsTolerance = t } arr@(Array _ _) = fromMajorCells . fmap unTolerantL <$> sortDown (TolerantL t <$> majorCells arr)
sortDown' _ _ = throwError $ DomainError "Only arrays can be sorted"

reorderAxesInner :: MonadError Error m => Noun -> Noun -> m Noun
reorderAxesInner x@(Array _ _) y@(Array _ _) = do
  shy <- shape' y
  sx <- sortUp' defaultCoreExtraArgs x
  is <- sortByUp' defaultCoreExtraArgs shy x
  is' <- key' ((reduce' defaultCoreExtraArgs min') `atop` (pure .: flip const)) sx is
  iota <- indexGenerator' defaultCoreExtraArgs is'
  indices <- eachRight (from defaultCoreExtraArgs) x iota
  from defaultCoreExtraArgs indices y
reorderAxesInner _ _ = throwError $ DomainError "Dictionaries cannot be transposed"

reorderAxes' :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Noun
reorderAxes' CoreExtraArgs{ coreExtraArgsOrigin = o } x y = do
  let err = DomainError "Transpose left argument must be a vector of integers not exceeding the rank of the right argument"
  x' <- asVector err x
    >>= mapM (asNumber err >=> asInt err)
    >>= mapM (\i -> pure $ if i < 0 then fromInteger $ i + toInteger (arrayRank y) else fromInteger $ i - toInteger o)
    >>= mapM (\i -> i <$ when (i >= arrayRank y) (throwError err))
  let iry = [0..arrayRank y - 1]
  axes <- genericTake (arrayRank y) . (x' ++) <$> difference iry x'
  reorderAxesInner (vector $ Number . fromIntegral <$> axes) y

reorderAxes :: MonadError Error m => CoreExtraArgs -> [Integer] -> Noun -> m Noun
reorderAxes cea is arr = reorderAxes' cea (vector $ Number . fromIntegral <$> is) arr

orient' :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Noun
orient' cea x y = do
  let err = DomainError "Orient left argument must be a vector of integers not exceeding the rank of the right argument"
  x' <- asVector err x
    >>= mapM (asNumber err >=> asInt err)
    >>= mapM (\i -> pure $ if i < 0 then fromInteger $ i + toInteger (arrayRank y) else fromInteger $ i - toInteger (coreExtraArgsOrigin cea))
    >>= mapM (\i -> i <$ when (i >= arrayRank y) (throwError err))
  let iry = [0..arrayRank y - 1]
  axes <- difference iry x' >>= gradeUp defaultCoreExtraArgs . genericTake (arrayRank y) . (x' ++)
  reorderAxesInner (vector $ Number . fromIntegral <$> axes) y

orient :: MonadError Error m => CoreExtraArgs -> [Integer] -> Noun -> m Noun
orient cea is arr = orient' cea (vector $ Number . fromIntegral <$> is) arr

dyadicTranspose' :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Noun
dyadicTranspose' cea@CoreExtraArgs{ coreExtraArgsBackward = b } = (if b then orient' else reorderAxes') cea

invertedTable :: MonadError Error m => Noun -> m ([ScalarValue], [ScalarValue])
invertedTable (Dictionary ks vs) = pure (ks, vs)
invertedTable _ = throwError $ DomainError "Inverted Table argument must be a dictionary"

invertedTable' :: MonadError Error m => Noun -> m Noun
invertedTable' = invertedTable >=> uncurry (pair `on` vector)

transpose :: MonadError Error m => CoreExtraArgs -> Noun -> m Noun
transpose CoreExtraArgs{ coreExtraArgsBackward = b } arr@(Array _ _) = (if b then orient else reorderAxes) defaultCoreExtraArgs [-1] arr
transpose _ dict@(Dictionary _ _) = invertedTable' dict

factorial :: MonadError Error m => ScalarValue -> m ScalarValue
factorial (Number n) = case asInt (DomainError "") n of
  Left _ -> pure $ Number $ Gamma.gamma $ n + 1
  Right i
    | i < 0 -> throwError $ DomainError "Factorial of a negative integer"
    | otherwise -> pure $ Number $ Gamma.factorial i
factorial _ = throwError expectedNumber

factorialS :: ScalarValue -> St ScalarValue
factorialS = orStruct1 "Factorial" factorial

factorial' :: Noun -> St Noun
factorial' = scalarMonad factorialS

binomial :: MonadError Error m => ScalarValue -> ScalarValue -> m ScalarValue
binomial (Number x) (Number y) = let
  go :: MonadError Error m => Complex Double -> Complex Double -> m (Complex Double)
  go n k = do
    let ni = asInt (DomainError "") n :: Either Error Integer
    let ki = asInt (DomainError "") k :: Either Error Integer
    case (ni, ki) of
      (Right n', Right k')
        | n' < 0 && k' >= 0 -> (((-1) ^ k') *) <$> go (k - n - 1) k
        | n' < 0 && k' <= n' -> (((-1) ^ (n' - k')) *) <$> go (-k - 1) (n - k)
        | n' < 0 -> pure 0
      (Right n', _) | n' < 0 -> throwError $ DomainError "If Choose left argument is a negative integer, the right argument must be an integer"
      (Right n', Right k')
        | k' < 0 || k' > n' -> pure 0
        | otherwise -> pure $ Gamma.factorial n' / (Gamma.factorial k' * Gamma.factorial (n' - k'))
      _ -> pure $ Gamma.gamma (n + 1) / (Gamma.gamma (k + 1) * Gamma.gamma (n - k + 1))
  in Number <$> go y x
binomial _ _ = throwError expectedNumber

binomialS :: ScalarValue -> ScalarValue -> St ScalarValue
binomialS = orStruct2 "Binomial" binomial

binomial' :: Noun -> Noun -> St Noun
binomial' = scalarDyad binomialS

raise :: MonadError Error m => Int -> String -> m ()
raise = throwError .: fromErrorCode

raise' :: MonadError Error m => Noun -> Noun -> m Noun
raise' code msg = do
  let err = DomainError "Raise left argument must be an integer scalar"
  code' <- asScalar err code >>= asNumber err >>= asInt err
  when (code' /= 0) $ raise code' $ show msg
  pure $ vector []

raise1 :: MonadError Error m => Noun -> m Noun
raise1 msg = do
  raise 1 $ show msg
  pure $ vector []

decode :: MonadError Error m => [Complex Double] -> [Complex Double] -> m (Complex Double)
decode ns cs =
  if length ns /= length cs then throwError $ LengthError "Decode arguments must have the same length"
  else pure $ sum $ zipWith (*) (Prelude.reverse $ scanl1 (*) (init $ 1 : Prelude.reverse ns)) cs

decode' :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Noun
decode' CoreExtraArgs{ coreExtraArgsBackward = back } a@(Array _ _) b@(Array _ _) = atRank2 defaultCoreExtraArgs (\ns cs -> do
  let err = DomainError "Decode arguments must be number arrays"
  cs' <- asVector err cs >>= mapM (asNumber err)
  ns' <- case asScalar err ns of
    Right x -> Prelude.replicate (length cs') <$> asNumber err x
    Left _ -> asVector err ns >>= mapM (asNumber err)
  scalar . Number <$> decode ns' (if back then Prelude.reverse cs' else cs')) (1, 1) a b
decode' _ _ _ = throwError $ DomainError "Decode arguments must be number arrays"

decodeBase2 :: MonadError Error m => CoreExtraArgs -> Noun -> m Noun
decodeBase2 cea = decode' cea (scalar $ Number 2)

encode :: MonadError Error m => CoreExtraArgs -> [Complex Double] -> Complex Double -> m [Complex Double]
encode _ [] _ = pure []
encode cea@CoreExtraArgs { coreExtraArgsTolerance = t } (bs :> b) n = do
  let rem = if b == 0 then n else complexRemainder' t b n
  let div = if b == 0 then 0 else complexFloor' t $ n / b
  (`snoc` rem) <$> encode cea bs div

encodeScalar :: MonadError Error m => CoreExtraArgs -> Complex Double -> Complex Double -> m [Complex Double]
encodeScalar _ b _ | Cx.magnitude b <= 1 = throwError $ DomainError "Scalar encode left argument must be greater than 1 in magnitude"
encodeScalar _ _ n | Number n == Number 0 = pure []
encodeScalar cea@CoreExtraArgs { coreExtraArgsTolerance = t } b n = do
  let rem = complexRemainder' t b n
  let div = complexFloor' t $ n / b
  let rem1 = complexRemainder' t b div
  let div1 = complexFloor' t $ div / b
  if rem == rem1 && div == div1 then pure [n]
  else (`snoc` rem) <$> encodeScalar cea b div

encode' :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Noun
encode' cea@CoreExtraArgs{ coreExtraArgsBackward = True } a@(Array _ _) b@(Array _ _) = atRank2 defaultCoreExtraArgs{ coreExtraArgsFill = Just $ Number 0 } (\b n -> do
  let err = DomainError "Encode arguments must be number arrays"
  n' <- asScalar err n >>= asNumber err
  case asScalar err b of
    Right b' -> vector . fmap Number . Prelude.reverse . (\xs -> if null xs then [0] else xs) <$> (asNumber err b' >>= flip (encodeScalar cea) n')
    Left _ -> vector . fmap Number . Prelude.reverse <$> (asVector err b >>= mapM (asNumber err) >>= flip (encode cea) n')) (1, 0) a b
encode' cea a@(Array _ _) b@(Array _ _) = encode' cea{ coreExtraArgsBackward = True } a b >>= atRank1 defaultCoreExtraArgs reverse' 1
encode' _ _ _ = throwError $ DomainError "Encode arguments must be number arrays"

encodeBase2 :: MonadError Error m => CoreExtraArgs -> Noun -> m Noun
encodeBase2 cea = encode' cea (scalar $ Number 2)

searchFunction :: MonadError Error m => (TolerantL Double Noun -> [TolerantL Double Noun] -> m Noun) -> (TolerantL Double ScalarValue -> [TolerantL Double ScalarValue] -> [TolerantL Double ScalarValue] -> m ScalarValue) -> CoreExtraArgs -> Noun -> Noun -> m Noun
searchFunction f _ CoreExtraArgs { coreExtraArgsTolerance = t } ns hs@(Array _ _) = let cutRank = arrayRank hs `naturalSaturatedSub` 1
  in if arrayRank ns < cutRank then throwError $ DomainError "Search function needle must have rank at least equal to the rank of the major cells of the haystack"
  else do
    let hc = majorCells hs
    nc <- atRank1 defaultCoreExtraArgs enclose' (toInteger cutRank) ns
    onScalars1 defaultCoreExtraArgs (\n -> do
      n' <- first defaultCoreExtraArgs n
      f (TolerantL t n') (TolerantL t <$> hc)) nc
searchFunction _ g CoreExtraArgs { coreExtraArgsTolerance = t } n (Dictionary ks vs) = do
  fromScalar <$> g (TolerantL t $ box n) (TolerantL t <$> ks) (TolerantL t <$> vs)

elementOf :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Noun
elementOf = searchFunction (pure .: scalar .: boolToScalar .: elem) (\e _ v -> pure $ boolToScalar $ e `elem` v)

count :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Noun
count = searchFunction (pure .: scalar .: Number .: (:+ 0) .: countEqual) (\e _ v -> pure $ Number $ countEqual e v :+ 0)

indexOf :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Noun
indexOf cea@CoreExtraArgs{ coreExtraArgsOrigin = o, coreExtraArgsBackward = b, coreExtraArgsFill = f } = flip $ searchFunction
  (pure .: scalar .: (\n hs -> fromMaybe (fromMaybe (Number $ (genericLength hs + fromIntegral o) :+ 0) f) $ Number . (:+ 0) . (+ fromIntegral o) <$> (if b then genericElemLastIndex else genericElemIndex) n hs))
  (\e k v -> case find (\(_, u) -> e == u) (zip k v) of
    Just (i, _) -> pure $ unTolerantL i
    Nothing -> throwError $ IndexError "Value not found in dictionary") cea

intervalIndex :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Noun
intervalIndex cea@CoreExtraArgs{ coreExtraArgsOrigin = o } hs' ns =
  if Prelude.not $ sorted $ majorCells hs' then throwError $ DomainError "Interval index left argument must be sorted"
  else (searchFunction (pure .: scalar .: Number .: (:+ 0) .: (\n hs -> do
    let lowers = Nothing : fmap Just hs
    let uppers = fmap Just hs :> Nothing
    let bounds = Prelude.reverse $ zipWith3 (\l u i -> ((l, u), i)) lowers uppers $ fmap (+ fromIntegral o) [0..genericLength hs]
    fromMaybe 0 $ fmap snd $ flip find bounds $ \case
      ((Just lower, Just upper), _) | lower <= n && n < upper -> True
      ((Just lower, Nothing), _) | lower <= n -> True
      ((Nothing, Just upper), _) | n < upper -> True
      _ -> False)) (\_ _ _ -> throwError $ DomainError "Interval index only works with arrays")) cea ns hs'

laminate :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Noun
laminate cea = catenate cea `over` promote

majorCells' :: MonadError Error m => Noun -> m Noun
majorCells' = pure . vector . fmap box . majorCells

mix :: MonadError Error m => CoreExtraArgs -> Noun -> m Noun
mix cea = atRank1 cea (first defaultCoreExtraArgs) 0

group :: MonadError Error m => CoreExtraArgs -> [Integer] -> [a] -> m [[a]]
group CoreExtraArgs{ coreExtraArgsOrigin = o } is xs = do
  let buckets = genericTake (1 + ((-1) `Prelude.max` maximum ((subtract $ toInteger o) <$> is))) $ Prelude.repeat []
  let go :: MonadError Error m => [[a]] -> [Integer] -> [a] -> m [[a]]
      go buckets _ [] = pure buckets
      go buckets (i:is) (x:xs)
        | i < toInteger o = go buckets is xs
        | otherwise = go ((genericTake i buckets) ++ [genericIndex buckets (i - toInteger o) `snoc` x] ++ (genericDrop (i + 1) buckets)) is xs
      go _ [] _ = throwError $ DomainError "Group left argument too short"
  go buckets is xs

group' :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Noun
group' cea is xs = do
  let err = DomainError "Group left argument must be a vector of integers"
  is' <- asVector err is >>= mapM (asNumber err >=> asInt err)
  (vector . fmap (box . fromMajorCells)) <$> TinyAPL.Functions.group cea is' (majorCells xs)

partition :: MonadError Error m => [Natural] -> [a] -> m [[a]]
partition is xs = do
  let areNewPartitions = True : mapAdjacent (/=) is
  let ws = zip3 ((0 /=) <$> is) areNewPartitions xs
  pure $ Prelude.reverse $ fmap Prelude.reverse $ foldl' (\ps (keep, newPart, x) ->
    if Prelude.not keep then ps
    else if newPart then [x] : ps
    else (x : headPromise ps) : tailPromise ps) [] ws

partition' :: MonadError Error m => Noun -> Noun -> m Noun
partition' is xs = do
  let err = DomainError "Partition left argument must be a vector of naturals"
  is' <- asVector err is >>= mapM (asNumber err >=> asNat err)
  (vector . fmap (box . fromMajorCells)) <$> partition is' (majorCells xs)

partitionEnclose :: MonadError Error m => [Natural] -> [a] -> m [[a]]
partitionEnclose ms xs = pure $ Prelude.reverse $ fmap Prelude.reverse $ foldl' (\ps (co, x) ->
  if (co == Nothing || co == Just 0) && null ps then ps
  else if co == Nothing || co == Just 0 then (fromJust x : headPromise ps) : tailPromise ps
  else fromMaybe [] (Data.List.singleton <$> x) : genericTake (fromJust co - 1) (Prelude.repeat []) ++ ps) [] $ zipLongest ms xs

partitionEnclose' :: MonadError Error m => Noun -> Noun -> m Noun
partitionEnclose' is xs = do
  let err = DomainError "Partition left argument must be a vector of naturals"
  is' <- asVector err is >>= mapM (asNumber err >=> asNat err)
  (vector . fmap (box . fromMajorCells)) <$> partitionEnclose is' (majorCells xs)

execute :: String -> St Noun
execute code = do
  ctx <- getContext
  scope <- createRef $ Scope [] [] [] [] Nothing -- The scope has intentionally no parent; execution runs in an isolated context
  (res, _) <- (liftToSt $ runResult $ runSt (run' "<execute>" code) $ ctx { contextScope = scope }) >>= liftEither
  case res of
    (VNoun x) -> pure x
    _ -> throwError $ DomainError "Execute code must return a noun"

execute' :: Noun -> St Noun
execute' code = do
  let err = DomainError "Execute code must be a string or array of strings"
  (sh, ss) <- asArrayOfStrings err code
  res <- mapM execute ss
  pure $ Array sh (toScalar <$> res)

format :: MonadError Error m => Noun -> m String
format x = pure $ show x

format' :: MonadError Error m => Noun -> m Noun
format' x = vector . fmap Character <$> format x

find' :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Noun
find' cea@CoreExtraArgs{ coreExtraArgsBackward = b } n hs = do
  when (arrayRank n > arrayRank hs) $ throwError $ DomainError "Find left argument must have rank at most equal to the right argument's"
  n' <- rerank (arrayRank hs) n
  onInfixes 0 (identical' cea n') ((, 1, if b then -6 else 6, []) <$> arrayShape n') hs

mask' :: MonadError Error m => CoreExtraArgs -> Noun -> Noun -> m Noun
mask' cea n hs = do
  when (arrayRank n > arrayRank hs) $ throwError $ DomainError "Mask left argument must have rank at most equal to the right argument's"
  n' <- rerank (arrayRank hs) n
  wh <- find' cea{ coreExtraArgsBackward = False } n' hs >>= indices defaultCoreExtraArgs
  inds <- indexGeneratorN defaultCoreExtraArgs (arrayShape n') >>= ravel'
  allInds <- indexGeneratorN defaultCoreExtraArgs (arrayShape hs)
  masks <- each1 (eachLeft (scalarDyad add) inds >=> TinyAPL.Functions.count cea allInds) wh >>= mix cea >>= pure . majorCells
  nonOverlapping <- Prelude.reverse . fst <$> foldlM (\(f, a) mask -> do
    ov <- arrayContents <$> (scalarDyad . TinyAPL.Functions.lcm) defaultCoreExtraArgs a mask
    if any (== Number 1) ov then pure (f, a)
    else do
      a' <- (scalarDyad . TinyAPL.Functions.gcd) defaultCoreExtraArgs a mask
      pure (mask : f, a')) ([], (arrayReshapedNE (arrayShape hs) $ Number <$> 0 NE.:| Prelude.repeat 0)) masks
  zipWithM (\(Array sh cs) ind -> Array sh <$> mapM (times (Number $ fromIntegral ind :+ 0)) cs) nonOverlapping [1..] >>= fold (scalarDyad add) (arrayReshapedNE (arrayShape hs) $ Number <$> 0 NE.:| Prelude.repeat 0)

histogram :: MonadError Error m => CoreExtraArgs -> Noun -> m Noun
histogram cea@CoreExtraArgs{ coreExtraArgsOrigin = o } = (((indexGenerator' cea `compose` first defaultCoreExtraArgs) `compose` reduce' defaultCoreExtraArgs max') `compose` ((flip (scalarDyad sub) $ scalar $ Number $ fromIntegral o) `compose` (scalarMonad increment))) `leftHook` TinyAPL.Functions.count defaultCoreExtraArgs

-- * Modifiers

compose :: MonadError Error m => (b -> m c) -> (a -> m b) -> a -> m c
compose f g = g >=> f

reverseCompose :: MonadError Error m => (a -> m b) -> (b -> m c) -> a -> m c
reverseCompose = flip compose

atop :: MonadError Error m => (c -> m d) -> (a -> b -> m c) -> a -> b -> m d
atop f g x y = g x y >>= f

reverseAtop :: MonadError Error m => (a -> b -> m c) -> (c -> m d) -> a -> b -> m d
reverseAtop = flip atop

over :: MonadError Error m => (b -> b -> m c) -> (a -> m b) -> a -> a -> m c
over f g x y = do
  x' <- g x
  y' <- g y
  f x' y'

reverseOver :: MonadError Error m => (a -> m b) -> (b -> b -> m c) -> a -> a -> m c
reverseOver = flip over

after :: MonadError Error m => (a -> c -> m d) -> (b -> m c) -> a -> b -> m d
after f g x y = do
  y' <- g y
  f x y'

before :: MonadError Error m => (a -> m b) -> (b -> c -> m d) -> a -> c -> m d
before f g x y = do
  x' <- f x
  g x' y

leftHook :: MonadError Error m => (a -> m b) -> (b -> a -> m c) -> a -> m c
leftHook f g y = do
  y' <- f y
  g y' y

rightHook :: MonadError Error m => (a -> b -> m c) -> (a -> m b) -> a -> m c
rightHook f g y = do
  y' <- g y
  f y y'

fork1 :: MonadError Error m => (a -> m b) -> (b -> c -> m d) -> (a -> m c) -> a -> m d 
fork1 f g h x = do
  b <- h x
  a <- f x
  g a b

fork2 :: MonadError Error m => (a -> b -> m c) -> (c -> d -> m e) -> (a -> b -> m d) -> a -> b -> m e
fork2 f g h x y = do
  b <- h x y
  a <- f x y
  g a b

mirror :: MonadError Error m => (b -> b -> m c) -> (a -> a -> m b) -> a -> a -> m c
mirror f g x y = do
  b <- g x y
  a <- g y x
  f a b

leftFork :: MonadError Error m => (a -> b -> m c) -> (c -> b -> m d) -> a -> b -> m d
leftFork f g x y = do
  a <- f x y
  g a y

rightFork :: MonadError Error m => (a -> c -> m d) -> (a -> b -> m c) -> a -> b -> m d
rightFork f g x y = do
  a <- g x y
  f x a

constant1 :: MonadError Error m => a -> b -> m a
constant1 a _ = pure a

constant2 :: MonadError Error m => a -> b -> c -> m a
constant2 a _ _ = pure a

duplicate :: MonadError Error m => (a -> a -> m b) -> a -> m b
duplicate f y = f y y

commute :: MonadError Error m => (b -> a -> m c) -> a -> b -> m c
commute f x y = f y x

reduce :: MonadError Error m => (a -> a -> m a) -> [a] -> m a
reduce _ [] = throwError $ DomainError "Reduce empty axis"
reduce f (x:xs) = foldlM f x xs

reduceBack :: MonadError Error m => (a -> a -> m a) -> [a] -> m a
reduceBack _ [] = throwError $ DomainError "Reduce empty axis"
reduceBack f (xs:>x) = foldrM f x xs

reduce' :: MonadError Error m => CoreExtraArgs -> (Noun -> Noun -> m Noun) -> Noun -> m Noun
reduce' CoreExtraArgs{ coreExtraArgsBackward = b } f a@(Array _ _) = (if b then reduceBack else reduce) f (majorCells a)
reduce' CoreExtraArgs{ coreExtraArgsBackward = b } f (Dictionary _ vs) = (if b then reduceBack else reduce) f (scalar <$> vs)

fold :: MonadError Error m => (a -> a -> m a) -> a -> [a] -> m a
fold = foldlM

foldBack :: MonadError Error m => (a -> a -> m a) -> a -> [a] -> m a
foldBack = foldrM

fold' :: MonadError Error m => CoreExtraArgs -> (Noun -> Noun -> m Noun) -> Noun -> Noun -> m Noun
fold' CoreExtraArgs{ coreExtraArgsBackward = b } f s a@(Array _ _) = (if b then foldBack else fold) f s $ majorCells a
fold' CoreExtraArgs{ coreExtraArgsBackward = b } f s (Dictionary _ vs) = (if b then foldBack else fold) f s $ scalar <$> vs

onPrefixes :: MonadError Error m => ([a] -> m b) -> [a] -> m [b]
onPrefixes f = mapM f . prefixes

onPrefixes' :: MonadError Error m => (Noun -> m Noun) -> Noun -> m Noun
onPrefixes' f arr = fromMajorCells <$> onPrefixes (f . fromMajorCells) (majorCells arr)

onSuffixes :: MonadError Error m => ([a] -> m b) -> [a] -> m [b]
onSuffixes f = mapM f . suffixes

onSuffixes' :: MonadError Error m => (Noun -> m Noun) -> Noun -> m Noun
onSuffixes' f arr = fromMajorCells <$> onSuffixes (f . fromMajorCells) (majorCells arr)

each1 :: MonadError Error m => (Noun -> m Noun) -> Noun -> m Noun
each1 f (Array sh cs) = Array sh <$> mapM (fmap box . f . fromScalar) cs
each1 f (Dictionary ks vs) = Dictionary ks <$> mapM (fmap box . f . fromScalar) vs

each2 :: MonadError Error m => (Noun -> Noun -> m Noun) -> Noun -> Noun -> m Noun
each2 f (Array ash acs) (Array bsh bcs)
  | null ash && null bsh = scalar . box <$> f (fromScalar $ headPromise acs) (fromScalar $ headPromise bcs)
  | null ash = Array bsh <$> mapM (fmap box . ((fromScalar $ headPromise acs) `f`) . fromScalar) bcs
  | null bsh = Array ash <$> mapM (fmap box . (`f` (fromScalar $ headPromise bcs)) . fromScalar) acs
  | ash == bsh = Array ash <$> zipWithM (fmap box .: f) (fromScalar <$> acs) (fromScalar <$> bcs)
  | length ash /= length bsh = throwError $ RankError "Incompatible ranks to Each"
  | otherwise = throwError $ LengthError "Incompatible shapes to Each"
each2 _ _ _ = throwError $ NYIError "Each on dictionaries not implemented"

{-
We'd want to do this:

each1 :: MonadError Error m => (Array -> m Array) -> Array -> m Array
each1 = boxed1 . onContents1 . onScalars1

each2 :: MonadError Error m => (Array -> Array -> m Array) -> Array -> Array -> m Array
each2 = boxed2 . onContents2 . onScalars2

but atRank2 is defined using each2 (which I'm not sure I like), so we can't.
-}

boxed1 :: MonadError Error m => (Noun -> m Noun) -> Noun -> m Noun
boxed1 = compose enclose'

boxed2 :: MonadError Error m => (Noun -> Noun -> m Noun) -> Noun -> Noun -> m Noun
boxed2 = atop enclose'

discloseIfScalar :: MonadError Error m => Noun -> m Noun
discloseIfScalar (Array [] [Box xs]) = pure xs
discloseIfScalar x = pure x

onContents1 :: MonadError Error m => (Noun -> m Noun) -> Noun -> m Noun
onContents1 = (`compose` discloseIfScalar)

onContents2 :: MonadError Error m => (Noun -> Noun -> m Noun) -> Noun -> Noun -> m Noun
onContents2 = (`over` discloseIfScalar)

eachLeft :: MonadError Error m => (Noun -> Noun -> m Noun) -> Noun -> Noun -> m Noun
eachLeft f x y = each2 f x (scalar $ box y)

eachRight :: MonadError Error m => (Noun -> Noun -> m Noun) -> Noun -> Noun -> m Noun
eachRight f x = each2 f (scalar $ box x)

key :: (Eq a, MonadError Error m) => (a -> [b] -> m c) -> [a] -> [b] -> m [c]
key f ks vs
  | length ks == length vs = mapM (uncurry f) (TinyAPL.Util.group ks vs)
  | otherwise = throwError $ LengthError "Incompatible shapes to Key"

key' :: MonadError Error m => (Noun -> Noun -> m Noun) -> Noun -> Noun -> m Noun
key' f karr@(Array _ _) varr@(Array _ _) = fromMajorCells <$> key (\k vs -> f k $ vector $ toScalar <$> vs) (majorCells karr) (majorCells varr)
key' _ _ _ = throwError $ NYIError "Key on dictionaries"

keyMonad :: MonadError Error m => CoreExtraArgs -> (Noun -> Noun -> m Noun) -> Noun -> m Noun
keyMonad cea f arr = do
  t <- tally' arr
  is <- indexGenerator' cea t
  key' f arr is

parseRank :: MonadError Error m => Noun -> m (Integer, Integer, Integer)
parseRank arr = do
  let err = DomainError "Rank or depth right operand must be a 1-, 2- or 3-element integer vector"
  v <- asVector err arr >>= mapM (asNumber err >=> asInt err)
  case v of
    [d] -> pure (d, d, d)
    [d, e] -> pure (e, d, e)
    [d, e, f] -> pure (d, e, f)
    _ -> throwError err

atRank1 :: MonadError Error m => CoreExtraArgs -> (Noun -> m Noun) -> Integer -> Noun -> m Noun
atRank1 cea@CoreExtraArgs{ coreExtraArgsFill = fill } f rank arr@(Array _ _)
  | arrayRank arr == 0 = f arr
  | rank >= 0 && toInteger (arrayRank arr) <= rank = f arr
  | rank >= 0 = fromMajorCellsMaybeFilled fill <$> mapM (atRank1 cea f rank) (majorCells arr)
  | rank == -1 = fromMajorCellsMaybeFilled fill <$> mapM f (majorCells arr)
  | otherwise = fromMajorCellsMaybeFilled fill <$> mapM (atRank1 cea f $ rank + 1) (majorCells arr)
atRank1 _ _ _ (Dictionary _ _) = throwError $ NYIError "At Rank on dictionaries"

atRank2 :: MonadError Error m => CoreExtraArgs -> (Noun -> Noun -> m Noun) -> (Integer, Integer) -> Noun -> Noun -> m Noun
atRank2 cea f (ra, rb) a b = do
  -- @dzaima (in fact, {a b c←⌽3⍴⌽⍵⍵ ⋄ ↑(⊂⍤b⊢⍺) ⍺⍺¨ ⊂⍤c⊢⍵} is an impl of the dyadic case from the monadic one (with Dyalog's ↑ meaning))
  as <- atRank1 cea enclose' ra a
  bs <- atRank1 cea enclose' rb b
  each2 f as bs >>= atRank1 cea (first defaultCoreExtraArgs) 0

atRank1' :: MonadError Error m => CoreExtraArgs -> (Noun -> m Noun) -> Noun -> Noun -> m Noun
atRank1' cea f r y = do
  (a, _, _) <- parseRank r
  atRank1 cea f a y

atRank2' :: MonadError Error m => CoreExtraArgs -> (Noun -> Noun -> m Noun) -> Noun -> Noun -> Noun -> m Noun
atRank2' cea f r x y = do
  (_, b, c) <- parseRank r
  atRank2 cea f (b, c) x y

onCells1 :: MonadError Error m => CoreExtraArgs -> (Noun -> m Noun) -> Noun -> m Noun
onCells1 cea f = atRank1 cea f (-1)

onCells2 :: MonadError Error m => CoreExtraArgs -> (Noun -> Noun -> m Noun) -> Noun -> Noun -> m Noun
onCells2 cea f = atRank2 cea f (-1, -1)

cellsLeft :: MonadError Error m => CoreExtraArgs -> (Noun -> Noun -> m Noun) -> Noun -> Noun -> m Noun
cellsLeft cea f = atRank2 cea f (-1, likePositiveInfinity)

cellsRight :: MonadError Error m => CoreExtraArgs -> (Noun -> Noun -> m Noun) -> Noun -> Noun -> m Noun
cellsRight cea f = atRank2 cea f (likePositiveInfinity, -1)

onScalars1 :: MonadError Error m => CoreExtraArgs -> (Noun -> m Noun) -> Noun -> m Noun
onScalars1 cea f = atRank1 cea f 0

onScalars2 :: MonadError Error m => CoreExtraArgs -> (Noun -> Noun -> m Noun) -> Noun -> Noun -> m Noun
onScalars2 cea f = atRank2 cea f (0, 0)

atDepth1 :: MonadError Error m => (Noun -> m Noun) -> Integer -> Noun -> m Noun
atDepth1 f depth arr@(Array _ _)
  | arrayDepth arr == 0 || (depth >= 0 && toInteger (arrayDepth arr) <= depth) = f arr
  | depth == -1 = each1 f arr
  | otherwise = each1 (atDepth1 f $ depth + (if depth < 0 then 1 else 0)) arr
atDepth1 f depth (Dictionary ks vs) = do
  res <- atDepth1 f depth (vector vs) >>= asVector (DomainError "At Depth on dictionary must return a vector")
  pure $ dictionary $ zip ks res

atDepth2 :: MonadError Error m => (Noun -> Noun -> m Noun) -> (Integer, Integer) -> Noun -> Noun -> m Noun
atDepth2 f (da, db) a@(Array _ _) b@(Array _ _) = go f (if da == 0 then likeNegativeInfinity else da, if db == 0 then likeNegativeInfinity else db) a b where
  go f (da, db) a b = let
    leftPure = da == 0 || arrayDepth a == 0 || (da >= 0 && toInteger (arrayDepth a) <= da)
    rightPure = db == 0 || arrayDepth b == 0 || (db >= 0 && toInteger (arrayDepth b) <= db)
    in case (leftPure, rightPure) of
      (True, True) -> f a b
      (True, False) -> atDepth1 (a `f`) db b
      (False, True) -> atDepth1 (`f` b) da a
      (False, False) -> each2 (go f (da + (if da < 0 then 1 else 0), db + (if db < 0 then 1 else 0))) a b
atDepth2 _ _ _ _ = throwError $ NYIError "Dyad At Depth on dictionaries"

atDepth1' :: MonadError Error m => (Noun -> m Noun) -> Noun -> Noun -> m Noun
atDepth1' f d y = do
  (a, _, _) <- parseRank d
  atDepth1 f a y

atDepth2' :: MonadError Error m => (Noun -> Noun -> m Noun) -> Noun -> Noun -> Noun -> m Noun
atDepth2' f d x y = do
  (_, b, c) <- parseRank d
  atDepth2 f (b, c) x y

onSimpleScalars1 :: MonadError Error m => (Noun -> m Noun) -> Noun -> m Noun
onSimpleScalars1 f = atDepth1 f 0

onSimpleScalars2 :: MonadError Error m => (Noun -> Noun -> m Noun) -> Noun -> Noun -> m Noun
onSimpleScalars2 f = atDepth2 f (0, 0)

repeat :: MonadError Error m => (a -> m a) -> Natural -> a -> m a
repeat _ 0 x = pure x
repeat f n x = f x >>= TinyAPL.Functions.repeat f (n - 1)

until :: MonadError Error m => (a -> m a) -> (a -> a -> m Bool) -> a -> m a
until f p x = let
  go :: Monad m => (a -> m a) -> (a -> a -> m Bool) -> a -> a -> m a
  go f p prev x = do
    r <- f x
    t <- p r prev
    if t then pure r else go f p x r
  in f x >>= go f p x

repeat1 :: MonadError Error m => (Noun -> m Noun) -> Noun -> Noun -> m Noun
repeat1 f t y = do
  let err = DomainError "Repeat right operand must be a natural scalar"
  n <- asScalar err t >>= asNumber err >>= asNat err
  TinyAPL.Functions.repeat f n y

repeat2 :: MonadError Error m => (Noun -> Noun -> m Noun) -> Noun -> Noun -> Noun -> m Noun
repeat2 f t x y = do
  let err = DomainError "Repeat right operand must be a natural scalar"
  n <- asScalar err t >>= asNumber err >>= asNat err
  TinyAPL.Functions.repeat (f x) n y

until1 :: MonadError Error m => (Noun -> m Noun) -> (Noun -> Noun -> m Noun) -> Noun -> m Noun
until1 f p y = let
  err = DomainError "Until right operand must return a boolean scalar"
  in TinyAPL.Functions.until f (\cu pr -> p cu pr >>= asScalar err >>= asBool err) y

until2 :: MonadError Error m => (Noun -> Noun -> m Noun) -> (Noun -> Noun -> m Noun) -> Noun -> Noun -> m Noun
until2 f p x y = let
  err = DomainError "Until right operand must return a boolean scalar"
  in TinyAPL.Functions.until (f x) (\cu pr -> p cu pr >>= asScalar err >>= asBool err) y

under :: MonadError Error m => (Noun -> m Noun) -> (Noun -> m Noun) -> Noun -> m Noun
under f g arr@(Array _ _) = do
  let numsL = [1..fromIntegral $ product $ arrayShape arr]
  let nums = fromJust $ arrayReshaped (arrayShape arr) $ Number . (:+ 0) <$> numsL
  pairs <- each2 pair arr nums
  rs <- g pairs
  (nums'Sh, nums') <- liftA2 (,) arrayShape (fmap (\case { Number (x :+ 0) -> x; _ -> error "???" }) . arrayContents) <$> each1 (TinyAPL.Functions.last defaultCoreExtraArgs) rs
  res <- onScalars1 defaultCoreExtraArgs (first defaultCoreExtraArgs `compose` first defaultCoreExtraArgs) rs >>= f
  unless (distinct nums') $ throwError $ DomainError "Under right argument must return each element at most once"
  if isScalar res then do
    pure $ Array (arrayShape arr) $ zipWith (\num el -> if num `elem` nums' then headPromise $ arrayContents res else el) numsL (arrayContents arr)
  else if nums'Sh == arrayShape res then do
    let cs = Map.fromList $ zip nums' (arrayContents res)
    pure $ Array (arrayShape arr) $ zipWith (\num el -> fromMaybe el $ Map.lookup num cs) numsL (arrayContents arr)
  else throwError $ DomainError "Under left argument mustn't change the shape of the argument"
under _ _ (Dictionary _ _) = throwError $ NYIError "Under on dictionaries"

under2 :: MonadError Error m => (Noun -> Noun -> m Noun) -> (Noun -> m Noun) -> Noun -> Noun -> m Noun
under2 f g x = under (f x) g

underK :: MonadError Error m => Noun -> (Noun -> m Noun) -> Noun -> m Noun
underK arr = under (\_ -> pure arr)

table :: MonadError Error m => (Noun -> Noun -> m Noun) -> Noun -> Noun -> m Noun
table f = atRank2 defaultCoreExtraArgs (onScalars2 defaultCoreExtraArgs f) (0, likePositiveInfinity)

innerProduct :: MonadError Error m => (Noun -> m Noun) -> (Noun -> Noun -> m Noun) -> Noun -> Noun -> m Noun
innerProduct f g = atRank2 defaultCoreExtraArgs (atop f (onCells2 defaultCoreExtraArgs (onScalars2 defaultCoreExtraArgs g))) (1, likePositiveInfinity)

onInfixesFill :: MonadError Error m => Natural -> [ScalarValue] -> Integer -> NE.NonEmpty Noun -> m [Noun]
onInfixesFill 0 [fill] count ys = pure $ genericTake (Prelude.abs count) $ Prelude.repeat $ arrayReshapedNE (arrayShape $ NE.head ys) (NE.singleton fill)
onInfixesFill 1 [] count ys
  | count > 0 = pure $ genericTake count $ Prelude.repeat $ NE.last ys
  | count < 0 = pure $ genericTake (Prelude.abs count) $ Prelude.repeat $ NE.head ys
  | otherwise = pure []
onInfixesFill 2 [] count ys
  | count > 0 = pure $ genericTake count $ Prelude.cycle $ Prelude.reverse (NE.toList ys) ++ NE.toList ys
  | count < 0 = pure $ Prelude.reverse $ genericTake (Prelude.abs count) $ Prelude.cycle $ NE.toList ys ++ Prelude.reverse (NE.toList ys)
  | otherwise = pure []
onInfixesFill 3 [] count ys
  | count > 0 = pure $ genericTake count $ Prelude.cycle $ tailPromise (Prelude.reverse $ NE.toList ys) ++ tailPromise (NE.toList ys)
  | count < 0 = pure $ Prelude.reverse $ genericTake (Prelude.abs count) $ Prelude.cycle $ tailPromise (NE.toList ys) ++ tailPromise (Prelude.reverse $ NE.toList ys)
  | otherwise = pure []
onInfixesFill 4 [] count ys
  | count > 0 = pure $ genericTake count $ Prelude.cycle $ NE.toList ys
  | count < 0 = pure $ Prelude.reverse $ genericTake (Prelude.abs count) $ Prelude.cycle $ Prelude.reverse $ NE.toList ys
  | otherwise = pure []
onInfixesFill _ _ _ _ = throwError $ DomainError "On Infixes: invalid fill mode or parameters"

encloseAnAxis :: MonadError Error m => Natural -> Noun -> m [Noun]
encloseAnAxis n arr = majorCells <$> orient defaultCoreExtraArgs [fromIntegral n] arr

mixAnAxis :: MonadError Error m => Natural -> [Noun] -> m Noun
mixAnAxis n arrs = reorderAxes defaultCoreExtraArgs [fromIntegral n] $ fromMajorCells arrs

onInfixes :: MonadError Error m => Natural -> (Noun -> m Noun) -> [(Natural, Natural, Integer, [ScalarValue])] -> Noun -> m Noun
onInfixes _ _ [] y = pure y
onInfixes axis f ((size, skip, 1, _) : specs) y = do
  cells <- encloseAnAxis axis y
  let howMany = let n = 1 + ((genericLength cells `naturalSaturatedSub` size) `div` skip) in if skip * (n - 1) + (size `Prelude.max` skip) < genericLength cells then n + 1 else n
  let slices = (\i -> genericTake size $ genericDrop (i * skip) cells) <$> [0..howMany - 1]
  fromMajorCells <$> mapM (mixAnAxis axis >=> (if null specs then f else onInfixes (axis + 1) f specs)) slices
onInfixes axis f ((size, skip, -1, _) : specs) y = do
  cells <- Prelude.reverse <$> encloseAnAxis axis y
  let howMany = let n = 1 + ((genericLength cells `naturalSaturatedSub` size) `div` skip) in if skip * (n - 1) + (size `Prelude.max` skip) < genericLength cells then n + 1 else n
  let slices = Prelude.reverse $ filter (Prelude.not . null) $ (\i -> Prelude.reverse $ genericTake size $ genericDrop (i * skip) cells) <$> [0..howMany - 1]
  fromMajorCells <$> mapM (mixAnAxis axis >=> (if null specs then f else onInfixes (axis + 1) f specs)) slices
onInfixes axis f ((size, skip, 2, _) : specs) y = do
  cells <- encloseAnAxis axis y
  let howMany = 1 + ((genericLength cells `naturalSaturatedSub` size) `div` skip)
  let slices = (\i -> genericTake size $ genericDrop (i * skip) cells) <$> [0..howMany - 1]
  fromMajorCells <$> mapM (mixAnAxis axis >=> (if null specs then f else onInfixes (axis + 1) f specs)) slices
onInfixes axis f ((size, skip, -2, _) : specs) y = do
  cells <- Prelude.reverse <$> encloseAnAxis axis y
  let howMany = 1 + ((genericLength cells `naturalSaturatedSub` size) `div` skip)
  let slices = Prelude.reverse $ filter (Prelude.not . null) $ (\i -> Prelude.reverse $ genericTake size $ genericDrop (i * skip) cells) <$> [0..howMany - 1]
  fromMajorCells <$> mapM (mixAnAxis axis >=> (if null specs then f else onInfixes (axis + 1) f specs)) slices
onInfixes axis f ((size, skip, 3, (mode' : params)) : specs) y = do
  let modeErr = DomainError "On Infixes: invalid mode"
  mode <- asNumber modeErr mode' >>= asNat modeErr
  originalCells <- encloseAnAxis axis y
  let padSize = let n = 1 + ((genericLength originalCells `naturalSaturatedSub` size) `div` skip) in if skip * (n - 1) + (size `Prelude.max` skip) < genericLength originalCells then (fromIntegral n * fromIntegral skip - genericLength originalCells) `mod` fromIntegral size else 0
  padCells <- onInfixesFill mode params padSize (NE.fromList originalCells)
  let cells = originalCells ++ padCells
  onInfixes axis f ((size, skip, 2, []) : specs) (fromMajorCells cells)
onInfixes axis f ((size, skip, -3, (mode' : params)) : specs) y = do
  let modeErr = DomainError "On Infixes: invalid mode"
  mode <- asNumber modeErr mode' >>= asNat modeErr
  originalCells <- encloseAnAxis axis y
  let padSize = let n = 1 + ((genericLength originalCells `naturalSaturatedSub` size) `div` skip) in if skip * (n - 1) + (size `Prelude.max` skip) < genericLength originalCells then (fromIntegral n * fromIntegral skip - genericLength originalCells) `mod` fromIntegral size else 0
  padCells <- onInfixesFill mode params (negate padSize) (NE.fromList originalCells)
  let cells = padCells ++ originalCells
  onInfixes axis f ((size, skip, -2, []) : specs) (fromMajorCells cells)
onInfixes axis f ((size, skip, 5, (mode' : params)) : specs) y = do
  let modeErr = DomainError "On Infixes: invalid mode"
  mode <- asNumber modeErr mode' >>= asNat modeErr
  originalCells <- encloseAnAxis axis y
  let padSize = fromIntegral $ size `naturalSaturatedSub` (genericLength originalCells `naturalSaturatedSub` (ceiling $ fromIntegral (genericLength originalCells) / fromIntegral skip - 1) * skip)
  padCells <- onInfixesFill mode params padSize (NE.fromList originalCells)
  let cells = originalCells ++ padCells
  onInfixes axis f ((size, skip, 2, []) : specs) (fromMajorCells cells)
onInfixes axis f ((size, skip, -5, (mode' : params)) : specs) y = do
  let modeErr = DomainError "On Infixes: invalid mode"
  mode <- asNumber modeErr mode' >>= asNat modeErr
  originalCells <- encloseAnAxis axis y
  let padSize = fromIntegral $ size `naturalSaturatedSub` (genericLength originalCells `naturalSaturatedSub` (ceiling $ fromIntegral (genericLength originalCells) / fromIntegral skip) * (skip - 1))
  padCells <- onInfixesFill mode params (negate padSize) (NE.fromList originalCells)
  let cells = padCells ++ originalCells
  onInfixes axis f ((size, skip, -2, []) : specs) (fromMajorCells cells)
onInfixes axis f ((size, skip, 6, _) : specs) y = do
  cells <- encloseAnAxis axis y
  let howMany = ceiling $ fromIntegral (genericLength cells) / fromIntegral skip
  let slices = filter (Prelude.not . null) $ (\i -> genericTake size $ genericDrop (i * skip) cells) <$> [0..howMany - 1]
  fromMajorCells <$> mapM (mixAnAxis axis >=> (if null specs then f else onInfixes (axis + 1) f specs)) slices
onInfixes axis f ((size, skip, -6, _) : specs) y = do
  cells <- Prelude.reverse <$> encloseAnAxis axis y
  let howMany = ceiling $ fromIntegral (genericLength cells) / fromIntegral skip
  let slices = Prelude.reverse $ filter (Prelude.not . null) $ (\i -> Prelude.reverse $ genericTake size $ genericDrop (i * skip) cells) <$> [0..howMany - 1]
  fromMajorCells <$> mapM (mixAnAxis axis >=> (if null specs then f else onInfixes (axis + 1) f specs)) slices
onInfixes _ _ ((_, _, other, _) : _) _ = throwError $ DomainError $ "On Infixes: invalid mode " ++ show other

onInfixes' :: MonadError Error m => (Noun -> m Noun) -> Noun -> Noun -> m Noun
onInfixes' f x y = do
  when (null $ arrayContents y) $ throwError $ DomainError "On Infixes: empty array"
  let err = DomainError "On Infixes: invalid arguments"
  unScalarX <- if isScalar x then do { xs <- asScalar err x; x' <- asNumber err xs >>= asInt err; if x' > 0 then pure $ vector [ xs, Number 1 ] else pure $ vector [ Number $ fromIntegral $ Prelude.abs x', Number $ fromIntegral $ Prelude.abs x' ] } else pure x
  unVectorX <- if arrayRank unScalarX == 2 then pure unScalarX else if arrayRank unScalarX <= 1 then rerank 2 unScalarX else throwError err
  let rows = majorCells unVectorX
  specs <- mapM (\row -> do
    vec <- asVector err row
    when (null vec) $ throwError $ DomainError "On Infixes left argument must provide at the very least the window size"
    size <- asNumber err >=> asNat err $ headPromise vec
    when (size == 0) $ throwError $ DomainError "On Infixes size cannot be zero"
    skip <- if length vec < 2 then pure 1 else asNumber err >=> asNat err $ vec !! 1
    when (skip == 0) $ throwError $ DomainError "On Infixes skip cannot be zero"
    mode <- if length vec < 3 then pure 2 else asNumber err >=> asInt err $ vec !! 2
    args <- if length vec < 4 then pure [] else asVector err $ fromScalar $ vec !! 3
    pure (size, skip, mode, args)) rows
  onInfixes 0 f specs y

bitwise1 :: MonadError Error m => CoreExtraArgs -> (Noun -> m Noun) -> ScalarValue -> m ScalarValue
bitwise1 cea@CoreExtraArgs{ coreExtraArgsBackward = b } f y'@(Number _) = do
  enc <- (if y' < Number 0 then if b then init else tailPromise else id) <$> (encodeBase2 cea (scalar y') >>= asVector unreachable >>= mapM (asNumber unreachable))
  res <- f $ vector $ Number <$> enc
  case asScalar (DomainError "") res of
    Right r -> pure r
    Left _ -> do
      let err = DomainError "Bitwise operation must return a scalar or vector of numbers"
      res' <- asVector err res >>= mapM (asNumber err)
      signBit <- (f $ scalar $ Number $ if y' < Number 0 then 1 else 0) >>= asScalar err >>= asNumber err
      (decodeBase2 cea $ vector $ fmap Number $ (if signBit /= 0 then if b then (:> -1) else (-1 :) else id) $ res') >>= asScalar unreachable
bitwise1 _ _ _ = throwError expectedNumber

bitwise1' :: MonadError Error m => CoreExtraArgs -> (Noun -> m Noun) -> Noun -> m Noun
bitwise1' cea f = scalarMonad (bitwise1 cea f)

bitwise2 :: MonadError Error m => CoreExtraArgs -> (Noun -> Noun -> m Noun) -> ScalarValue -> ScalarValue -> m ScalarValue
bitwise2 cea@CoreExtraArgs{ coreExtraArgsBackward = b } f x'@(Number _) y'@(Number _) = do
  encX <- (if x' < Number 0 then if b then init else tailPromise else id) <$> (encodeBase2 cea (scalar x') >>= asVector unreachable >>= mapM (asNumber unreachable))
  encY <- (if y' < Number 0 then if b then init else tailPromise else id) <$> (encodeBase2 cea (scalar y') >>= asVector unreachable >>= mapM (asNumber unreachable))
  let maxLen = (Prelude.max `on` genericLength) encX encY
  padX <- take' cea{ coreExtraArgsFill = Just $ Number $ if x' < Number 0 then 1 else 0 } (scalar $ Number $ (-maxLen) :+ 0) (vector $ Number <$> encX) >>= asVector unreachable >>= mapM (asNumber unreachable)
  padY <- take' cea{ coreExtraArgsFill = Just $ Number $ if y' < Number 0 then 1 else 0 } (scalar $ Number $ (-maxLen) :+ 0) (vector $ Number <$> encY) >>= asVector unreachable >>= mapM (asNumber unreachable)
  res <- f (vector $ Number <$> padX) (vector $ Number <$> padY)
  case asScalar (DomainError "") res of
    Right r -> pure r
    Left _ -> do
      let err = DomainError "Bitwise operation must return a scalar or vector of numbers"
      res' <- asVector err res >>= mapM (asNumber err)
      signBit <- (f (scalar $ Number $ if x' < Number 0 then 1 else 0) (scalar $ Number $ if y' < Number 0 then 1 else 0)) >>= asScalar err >>= asNumber err
      (decodeBase2 cea $ vector $ fmap Number $ (if signBit /= 0 then if b then (:> -1) else (-1 :) else id) $ res') >>= asScalar unreachable
bitwise2 _ _ _ _ = throwError expectedNumber

bitwise2' :: MonadError Error m => CoreExtraArgs -> (Noun -> Noun -> m Noun) -> Noun -> Noun -> m Noun
bitwise2' cea f = scalarDyad (bitwise2 cea f)
