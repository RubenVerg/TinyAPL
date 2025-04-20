{-# LANGUAGE NegativeLiterals #-}

module TinyAPL.PrimitivesSpec where

import TinyAPL.Noun
import TinyAPL.Function
import TinyAPL.Adverb
import TinyAPL.Conjunction
import TinyAPL.Context
import TinyAPL.CoreQuads
import TinyAPL.Complex
import TinyAPL.Error
import qualified TinyAPL.Glyphs as G
import qualified TinyAPL.Primitives as P
import TinyAPL.Util

import Test.Hspec hiding (context)
import Data.IORef
import System.IO.Unsafe
import Data.List (singleton)
import Data.Functor.Identity (Identity(runIdentity))

scope = unsafePerformIO $ newIORef $ Scope [] [] [] [] Nothing True
{-# NOINLINE scope #-}
idRef = unsafePerformIO $ newIORef (0 :: Integer)
{-# NOINLINE idRef #-}
context = Context scope core undefined undefined undefined idRef "" P.primitives

instance Show ScalarValue where
  show = runIdentity . showM

instance Show Noun where
  show = runIdentity . showM

spec :: Spec
spec = do
  let m' :: Function -> ExtraArgs -> Noun -> IO (Result Noun)
      m' fn ea y = runResult $ fst <$> runSt (callMonad fn ea y) context

      m :: Function -> Noun -> IO (Result Noun)
      m fn = m' fn []

      d' :: Function -> ExtraArgs -> Noun -> Noun -> IO (Result Noun)
      d' fn ea x y = runResult $ fst <$> runSt (callDyad fn ea x y) context

      d :: Function -> Noun -> Noun -> IO (Result Noun)
      d fn = d' fn []

      aa' :: Adverb -> ExtraArgs -> Noun -> IO (Result Function)
      aa' adv ea' u = runResult $ fst <$> runSt (callOnNoun adv ea' u) context

      aa :: Adverb -> Noun -> IO (Result Function)
      aa adv = aa' adv []

      aam' :: Adverb -> ExtraArgs -> ExtraArgs -> Noun -> Noun -> IO (Result Noun)
      aam' adv ea' ea u x = runResult $ fst <$> runSt (callOnNoun adv ea' u >>= (\f -> callMonad f ea x)) context

      aam :: Adverb -> Noun -> Noun -> IO (Result Noun)
      aam adv = aam' adv [] []

      aad' :: Adverb -> ExtraArgs -> ExtraArgs -> Noun -> Noun -> Noun -> IO (Result Noun)
      aad' adv ea' ea u x y = runResult $ fst <$> runSt (callOnNoun adv ea' u >>= (\f -> callDyad f ea x y)) context

      aad :: Adverb -> Noun -> Noun -> Noun -> IO (Result Noun)
      aad adv = aad' adv [] []

      af' :: Adverb -> ExtraArgs -> Function -> IO (Result Function)
      af' adv ea' f = runResult $ fst <$> runSt (callOnFunction adv ea' f) context

      af :: Adverb -> Function -> IO (Result Function)
      af adv = af' adv []

      afm' :: Adverb -> ExtraArgs -> ExtraArgs -> Function -> Noun -> IO (Result Noun)
      afm' adv ea' ea f x = runResult $ fst <$> runSt (callOnFunction adv ea' f >>= (\f' -> callMonad f' ea x)) context

      afm :: Adverb -> Function -> Noun -> IO (Result Noun)
      afm adv = afm' adv [] []

      afd' :: Adverb -> ExtraArgs -> ExtraArgs -> Function -> Noun -> Noun -> IO (Result Noun)
      afd' adv ea' ea f x y = runResult $ fst <$> runSt (callOnFunction adv ea' f >>= (\f' -> callDyad f' ea x y)) context

      afd :: Adverb -> Function -> Noun -> Noun -> IO (Result Noun)
      afd adv = afd' adv [] []

      caa' :: Conjunction -> ExtraArgs -> Noun -> Noun -> IO (Result Function)
      caa' conj ea' u v = runResult $ fst <$> runSt (callOnNounAndNoun conj ea' u v) context

      caa :: Conjunction -> Noun -> Noun -> IO (Result Function)
      caa conj = caa' conj []

      caam' :: Conjunction -> ExtraArgs -> ExtraArgs -> Noun -> Noun -> Noun -> IO (Result Noun)
      caam' conj ea' ea u v x = runResult $ fst <$> runSt (callOnNounAndNoun conj ea' u v >>= (\f -> callMonad f ea x)) context

      caam :: Conjunction -> Noun -> Noun -> Noun -> IO (Result Noun)
      caam conj = caam' conj [] []

      caad' :: Conjunction -> ExtraArgs -> ExtraArgs -> Noun -> Noun -> Noun -> Noun -> IO (Result Noun)
      caad' conj ea' ea u v x y = runResult $ fst <$> runSt (callOnNounAndNoun conj ea' u v >>= (\d -> callDyad d ea x y)) context

      caad :: Conjunction -> Noun -> Noun -> Noun -> Noun -> IO (Result Noun)
      caad conj = caad' conj [] []

      caf' :: Conjunction -> ExtraArgs -> Noun -> Function -> IO (Result Function)
      caf' conj ea' u g = runResult $ fst <$> runSt (callOnNounAndFunction conj ea' u g) context

      caf :: Conjunction -> Noun -> Function -> IO (Result Function)
      caf conj = caf' conj []

      cafm' :: Conjunction -> ExtraArgs -> ExtraArgs -> Noun -> Function -> Noun -> IO (Result Noun)
      cafm' conj ea' ea u g x = runResult $ fst <$> runSt (callOnNounAndFunction conj ea' u g >>= (\f -> callMonad f ea x)) context

      cafm :: Conjunction -> Noun -> Function -> Noun -> IO (Result Noun)
      cafm conj = cafm' conj [] []

      cafd' :: Conjunction -> ExtraArgs -> ExtraArgs -> Noun -> Function -> Noun -> Noun -> IO (Result Noun)
      cafd' conj ea' ea u g x y = runResult $ fst <$> runSt (callOnNounAndFunction conj ea' u g >>= (\f -> callDyad f ea x y)) context

      cafd :: Conjunction -> Noun -> Function -> Noun -> Noun -> IO (Result Noun)
      cafd conj = cafd' conj [] []

      cfa' :: Conjunction -> ExtraArgs -> Function -> Noun -> IO (Result Function)
      cfa' conj ea' f v = runResult $ fst <$> runSt (callOnFunctionAndNoun conj ea' f v) context

      cfa :: Conjunction -> Function -> Noun -> IO (Result Function)
      cfa conj = cfa' conj []

      cfam' :: Conjunction -> ExtraArgs -> ExtraArgs -> Function -> Noun -> Noun -> IO (Result Noun)
      cfam' conj ea' ea f v x = runResult $ fst <$> runSt (callOnFunctionAndNoun conj ea' f v >>= (\f' -> callMonad f' ea x)) context

      cfam :: Conjunction -> Function -> Noun -> Noun -> IO (Result Noun)
      cfam conj = cfam' conj [] []

      cfad' :: Conjunction -> ExtraArgs -> ExtraArgs -> Function -> Noun -> Noun -> Noun -> IO (Result Noun)
      cfad' conj ea' ea f v x y = runResult $ fst <$> runSt (callOnFunctionAndNoun conj ea' f v >>= (\f' -> callDyad f' ea x y)) context

      cfad :: Conjunction -> Function -> Noun -> Noun -> Noun -> IO (Result Noun)
      cfad conj = cfad' conj [] []

      cff' :: Conjunction -> ExtraArgs -> Function -> Function -> IO (Result Function)
      cff' conj ea' f g = runResult $ fst <$> runSt (callOnFunctionAndFunction conj ea' f g) context

      cff :: Conjunction -> Function -> Function -> IO (Result Function)
      cff conj = cff' conj []

      cffm' :: Conjunction -> ExtraArgs -> ExtraArgs -> Function -> Function -> Noun -> IO (Result Noun)
      cffm' conj ea' ea f g x = runResult $ fst <$> runSt (callOnFunctionAndFunction conj ea' f g >>= (\f' -> callMonad f' ea x)) context

      cffm :: Conjunction -> Function -> Function -> Noun -> IO (Result Noun)
      cffm conj = cffm' conj [] []

      cffd' :: Conjunction -> ExtraArgs -> ExtraArgs -> Function -> Function -> Noun -> Noun -> IO (Result Noun)
      cffd' conj ea' ea f g x y = runResult $ fst <$> runSt (callOnFunctionAndFunction conj ea' f g >>= (\f' -> callDyad f' ea x y)) context

      cffd :: Conjunction -> Function -> Function -> Noun -> Noun -> IO (Result Noun)
      cffd conj = cffd' conj [] []

      e2m :: Either e a -> Maybe a
      e2m (Right x) = Just x
      e2m (Left _) = Nothing

  describe "arrays" $ do
    describe [G.zilde] $ do
      it "is an empty vector" $ do
        P.zilde `shouldBe` vector []
  
  describe "functions" $ do
    describe [G.plus] $ do
      describe "conjugate" $ do
        it "doesn't change real numbers" $ do
          m P.plus (vector [Number 3, Number -2]) `shouldReturn` pure (vector [Number 3, Number -2])
        it "conjugates complex numbers" $ do
          m P.plus (vector [Number (3 :+ 2), Number (1 :+ -1)]) `shouldReturn` pure (vector [Number (3 :+ -2), Number (1 :+ 1)])
      describe "add" $ do
        it "adds complex numbers" $ do
          d P.plus (scalar $ Number (2 :+ 1)) (vector [Number 1, Number -3, Number (0 :+ 1)]) `shouldReturn` pure (vector [Number (3 :+ 1), Number (-1 :+ 1), Number (2 :+ 2)])
        it "adds an integer and a character" $ do
          d P.plus (scalar $ Number 1) (scalar $ Character '0') `shouldReturn` pure (scalar $ Character '1')
    
    describe [G.minus] $ do
      describe "negate" $ do
        it "negates complex numbers" $ do
          m P.minus (vector [Number 3, Number -1, Number (2 :+ 3)]) `shouldReturn` pure (vector [Number -3, Number 1, Number (-2 :+ -3)])
      describe "subtract" $ do
        it "subtracts complex numbers" $ do
          d P.minus (scalar $ Number 3) (vector [Number 1, Number -3, Number (2 :+ 1)]) `shouldReturn` pure (vector [Number 2, Number 6, Number (1 :+ -1)])
        it "subtracts an integer from a character" $ do
          d P.minus (scalar $ Character '9') (scalar $ Number 3) `shouldReturn` pure (scalar $ Character '6')
        it "subtracts characters" $ do
          d P.minus (scalar $ Character '8') (scalar $ Character '0') `shouldReturn` pure (scalar $ Number 8)

    describe [G.times] $ do
      describe "signum" $ do
        it "returns the sign of real numbers" $ do
          m P.times (vector [Number 3, Number 0, Number -5]) `shouldReturn` pure (vector [Number 1, Number 0, Number -1])
        it "returns the direction of complex numbers" $ do
          m P.times (vector [Number (0 :+ 2), Number (1 :+ 1)]) `shouldReturn` pure (vector [Number (0 :+ 1), Number (sqrt 0.5 :+ sqrt 0.5)])
      describe "case" $ do
        it "returns the case of characters" $ do
          m P.times (vector $ Character <$> "a.A") `shouldReturn` pure (vector [Number -1, Number 0, Number 1])
      describe "multiply" $ do
        it "multiplies complex numbers" $ do
          d P.times (scalar $ Number (2 :+ 1)) (vector [Number 2, Number -1, Number (3 :+ -1)]) `shouldReturn` pure (vector [Number (4 :+ 2), Number (-2 :+ -1), Number (7 :+ 1)])
        it "always returns zero when an argument is zero" $ do
          d P.times (scalar $ Number 0) (scalar $ Number $ inf :+ 0) `shouldReturn` pure (scalar $ Number 0)
          d P.times (scalar $ Number $ ninf :+ 0) (scalar $ Number 0) `shouldReturn` pure (scalar $ Number 0)
    
    describe [G.divide] $ do
      describe "reciprocal" $ do
        it "returns the reciprocal of complex numbers" $ do
          m P.divide (vector [Number 3, Number (2 :+ 1), Number -3]) `shouldReturn` pure (vector [Number (1 / 3), Number (1 / (2 :+ 1)), Number (-1 / 3)])
        it "fails with zero argument" $ do
          e2m <$> m P.divide (scalar $ Number 0) `shouldReturn` Nothing
      describe "divide" $ do
        it "divides complex numbers" $ do
          d P.divide (scalar $ Number (1 :+ 2)) (vector [Number 2, Number -3, Number (2 :+ -3)]) `shouldReturn` pure (vector [Number (0.5 :+ 1), Number ((-1 / 3) :+ (-2 / 3)), Number ((-4 / 13) :+ (7 / 13))])
        it "returns 1 for 0/0" $ do
          d P.divide (scalar $ Number 0) (scalar $ Number 0) `shouldReturn` pure (scalar $ Number 1)
        it "fails with zero right argument" $ do
          e2m <$> d P.divide (vector [Number 2, Number 1]) (scalar $ Number 0) `shouldReturn` Nothing
    
    describe [G.power] $ do
      describe "exp" $ do
        it "applies the exponential function to complex numbers" $ do
          m P.power (vector [Number 1, Number 2, Number -1, Number (0 :+ pi)]) `shouldReturn` pure (vector [Number $ exp 1, Number $ exp 2, Number $ exp -1, Number $ -1 :+ 0])
      describe "power" $ do
        it "exponentiates complex numbers" $ do
          d P.power (scalar $ Number 2) (vector [Number 1, Number -1, Number (2 :+ 1)]) `shouldReturn` pure (vector [Number 2, Number $ 1 / 2, Number $ 2 ** (2 :+ 1)])

    describe [G.logarithm] $ do
      describe "ln" $ do
        it "returns the natural logarithm of complex numbers" $ do
          m P.logarithm (vector [Number 1, Number (exp 1), Number -1]) `shouldReturn` pure (vector [Number $ log 1, Number 1, Number $ 0 :+ pi])
        it "fails with zero argument" $ do
          e2m <$> m P.logarithm (scalar $ Number 0) `shouldReturn` Nothing
      describe "log" $ do
        it "returns the logarithm of complex numbers" $ do
          d P.logarithm (scalar $ Number $ 2 :+ 1) (vector [Number 1, Number (0 :+ 3)]) `shouldReturn` pure (vector [Number 0, Number $ logBase (2 :+ 1) (0 :+ 3)])
        it "returns 1 for 1, 1" $ do
          d P.logarithm (scalar $ Number 1) (scalar $ Number 1) `shouldReturn` pure (scalar $ Number 1)
        it "fails for left argument 1" $ do
          e2m <$> d P.logarithm (scalar $ Number 1) (vector [Number 3, Number 0.5]) `shouldReturn` Nothing

    describe [G.root] $ do
      describe "square root" $ do
        it "returns the square root of complex arguments" $ do
          m P.root (vector [Number 4, Number -1, Number (1 :+ 1)]) `shouldReturn` pure (vector [Number 2, Number (0 :+ 1), Number $ sqrt (1 :+ 1)])
      describe "nth root" $ do
        it "returns the nth root of complex arguments" $ do
          d P.root (vector [Number 3, Number 4, Number (0 :+ -1)]) (vector [Number 8, Number 16, Number (2 :+ 1)]) `shouldReturn` pure (vector [Number 2, Number 2, Number $ (2 :+ 1) ** recip (0 :+ -1)])
    
    describe [G.floor] $ do
      describe "floor" $ do
        it "floors real numbers" $ do
          m P.floor (vector [Number 1, Number 1.25, Number -1.75]) `shouldReturn` pure (vector [Number 1, Number 1, Number -2])
        it "floors complex numbers" $ do
          m P.floor (vector [Number (0.25 :+ 0.25), Number (0.75 :+ 0.25), Number (0.25 :+ 0.75)]) `shouldReturn` pure (vector [Number 0, Number 1, Number (0 :+ 1)])
      describe "lowercase" $ do
        it "lowercases characters" $ do
          m P.floor (vector $ Character <$> "Ab.") `shouldReturn` pure (vector $ Character <$> "ab.")
      describe "min" $ do
        it "returns the minimum of real numbers" $ do
          d P.floor (vector [Number 3, Number -1]) (vector [Number 2, Number 5]) `shouldReturn` pure (vector [Number 2, Number -1])
        it "returns the lexicographical minimum of complex numbers" $ do
          d P.floor (vector [Number (2 :+ 1), Number (-1 :+ 3)]) (vector [Number (2 :+ 3), Number (2 :+ 5)]) `shouldReturn` pure (vector [Number (2 :+ 1), Number (-1 :+ 3)])

    describe [G.ceil] $ do
      describe "ceiling" $ do
        it "ceils real numbers" $ do
          m P.ceil (vector [Number 1, Number 1.25, Number -1.75]) `shouldReturn` pure (vector [Number 1, Number 2, Number -1])
        it "ceils complex numbers" $ do
          m P.ceil (vector [Number (0.25 :+ 0.25), Number (0.75 :+ 0.25), Number (0.75 :+ 0.75)]) `shouldReturn` pure (vector [Number (0 :+ 1), Number 1, Number (1 :+ 1)])
      describe "uppercase" $ do
        it "uppercases characters" $ do
          m P.ceil (vector $ Character <$> "Ab.") `shouldReturn` pure (vector $ Character <$> "AB.")
      describe "max" $ do
        it "returns the maximum of real numbers" $ do
          d P.ceil (vector [Number 3, Number -1]) (vector [Number 2, Number 5]) `shouldReturn` pure (vector [Number 3, Number 5])
        it "returns the lexicographical maximum of complex numbers" $ do
          d P.ceil (vector [Number (2 :+ 1), Number (-1 :+ 3)]) (vector [Number (2 :+ 3), Number (2 :+ 5)]) `shouldReturn` pure (vector [Number (2 :+ 3), Number (2 :+ 5)])

    describe [G.round] $ do
      describe "round" $ do
        it "rounds real numbers" $ do
          m P.round (vector [Number 1.2, Number 1.7]) `shouldReturn` pure (vector [Number 1, Number 2])
        it "rounds .5 above" $ do
          m P.round (vector [Number 1.5, Number -1.5]) `shouldReturn` pure (vector [Number 2, Number -1])
        it "rounds components of complex numbers" $ do
          m P.round (vector [Number (3 :+ 2.9), Number (1.2 :+ 3.4)]) `shouldReturn` pure (vector [Number (3 :+ 3), Number (1 :+ 3)])
      describe "round to" $ do
        it "rounds real numbers to the nearest multiple of other real numbers" $ do
          d P.round (vector [Number 1, Number 0.5, Number 10]) (scalar $ Number 123.45) `shouldReturn` pure (vector [Number 123, Number 123.5, Number 120])

    describe [G.less, G.equal, G.lessEqual, G.greater, G.notEqual, G.greaterEqual] $ do
      describe "comparisons" $ do
        it "compares scalars" $ do
          d P.less (vector [Number 1, Number 1, Number 1]) (vector [Number 0, Number 1, Number 2]) `shouldReturn` pure (vector [Number 0, Number 0, Number 1])
          d P.equal (vector [Number 1, Number 1, Number 1]) (vector [Number 0, Number 1, Number 2]) `shouldReturn` pure (vector [Number 0, Number 1, Number 0])
          d P.lessEqual (vector [Number 1, Number 1, Number 1]) (vector [Number 0, Number 1, Number 2]) `shouldReturn` pure (vector [Number 0, Number 1, Number 1])
          d P.greater (vector [Number 1, Number 1, Number 1]) (vector [Number 0, Number 1, Number 2]) `shouldReturn` pure (vector [Number 1, Number 0, Number 0])
          d P.notEqual (vector [Number 1, Number 1, Number 1]) (vector [Number 0, Number 1, Number 2]) `shouldReturn` pure (vector [Number 1, Number 0, Number 1])
          d P.greaterEqual (vector [Number 1, Number 1, Number 1]) (vector [Number 0, Number 1, Number 2]) `shouldReturn` pure (vector [Number 1, Number 1, Number 0])
    
    describe [G.notEqual] $ do
      describe "nub sieve" $ do
        it "marks positions of the first occurrence of each element" $ do
          m P.notEqual (vector [Number 1, Number 2, Number 1, Number 3, Number 3]) `shouldReturn` pure (vector [Number 1, Number 1, Number 0, Number 1, Number 0])

    describe [G.greater] $ do
      describe "first cell" $ do
        it "returns the first cell of the array" $ do
          m P.greater (vector [Number 1, Number 2, Number 3]) `shouldReturn` pure (scalar $ Number 1)
          m P.greater (fromMajorCells [vector [Number 1, Number 2], vector [Number 3, Number 4]]) `shouldReturn` pure (vector [Number 1, Number 2])
          m P.greater (scalar $ Number 1) `shouldReturn` pure (scalar $ Number 1)

    describe [G.greaterEqual] $ do
      describe "last cell" $ do
        it "returns the last cell of the array" $ do
          m P.greaterEqual (vector [Number 1, Number 2, Number 3]) `shouldReturn` pure (scalar $ Number 3)
          m P.greaterEqual (fromMajorCells [vector [Number 1, Number 2], vector [Number 3, Number 4]]) `shouldReturn` pure (vector [Number 3, Number 4])
          m P.greaterEqual (scalar $ Number 1) `shouldReturn` pure (scalar $ Number 1)

    describe [G.and] $ do
      describe "promote" $ do
        it "adds a unit leading axis to arrays" $ do
          m P.and (vector [Number 1, Number 2, Number 3]) `shouldReturn` pure (fromMajorCells [vector [Number 1, Number 2, Number 3]])
          m P.and (scalar $ Number 1) `shouldReturn` pure (vector [Number 1])
      describe "and" $ do
        it "combines boolean values with the and logical operation" $ do
          d P.and (vector [Number 0, Number 0, Number 1, Number 1]) (vector [Number 0, Number 1, Number 0, Number 1]) `shouldReturn` pure (vector [Number 0, Number 0, Number 0, Number 1])
      describe "lcm" $ do
        it "applies the LCM function to numbers" $ do
          d P.and (vector [Number 2, Number 0.5]) (vector [Number 3, Number 4.5]) `shouldReturn` pure (vector [Number 6, Number 4.5])
    
    describe [G.or] $ do
      describe "demote" $ do
        it "combines two leading axes" $ do
          m P.or (fromMajorCells [vector [Number 1, Number 2, Number 3], vector [Number 4, Number 5, Number 6]]) `shouldReturn` pure (vector [Number 1, Number 2, Number 3, Number 4, Number 5, Number 6])
        it "extracts the first element of vectors" $ do
          m P.or (vector [Number 1, Number 2, Number 3]) `shouldReturn` pure (scalar $ Number 1)
        it "leaves scalars unchanged" $ do
          m P.or (scalar $ Number 1) `shouldReturn` pure (scalar $ Number 1)
      describe "or" $ do
        it "combines boolean values with the or logical operation" $ do
          d P.or (vector [Number 0, Number 0, Number 1, Number 1]) (vector [Number 0, Number 1, Number 0, Number 1]) `shouldReturn` pure (vector [Number 0, Number 1, Number 1, Number 1])
      describe "gcd" $ do
        it "applies the GCD function to numbers" $ do
          d P.or (vector [Number 2, Number 0.5]) (vector [Number 3, Number 4.5]) `shouldReturn` pure (vector [Number 1, Number 0.5])

    describe [G.nand] $ do
      describe "nand" $ do
        it "combines boolean values with the nand logical operation" $ do
          d P.nand (vector [Number 0, Number 0, Number 1, Number 1]) (vector [Number 0, Number 1, Number 0, Number 1]) `shouldReturn` pure (vector [Number 1, Number 1, Number 1, Number 0])

    describe [G.nor] $ do
      describe "nor" $ do
        it "combines boolean values with the nor logical operation" $ do
          d P.nor (vector [Number 0, Number 0, Number 1, Number 1]) (vector [Number 0, Number 1, Number 0, Number 1]) `shouldReturn` pure (vector [Number 1, Number 0, Number 0, Number 0])

    describe [G.cartesian] $ do
      describe "pure imaginary" $ do
        it "multiplies arguments by i" $ do
          m P.cartesian (vector [Number 2, Number (3 :+ 1)]) `shouldReturn` pure (vector [Number (0 :+ 2), Number (-1 :+ 3)])
      describe "cartesian" $ do
        it "combines real and imaginary parts of a number" $ do
          d P.cartesian (vector [Number 1, Number (0 :+ 3)]) (vector [Number 2, Number (1 :+ 4)]) `shouldReturn` pure (vector [Number (1 :+ 2), Number (-4 :+ 4)])
    
    describe [G.polar] $ do
      describe "unit imaginary" $ do
        it "returns the point of the unit circle with phase specified by the argument" $ do
          m P.polar (vector [Number 0, Number pi, Number $ pi / 2, Number $ pi / 4]) `shouldReturn` pure (vector [Number 1, Number -1, Number (0 :+ 1), Number (sqrt 0.5 :+ sqrt 0.5)])
      describe "polar" $ do
        it "returns the complex number specified by the phase and radius" $ do
          d P.polar (vector [Number 3, Number 1, Number -1, Number (3 :+ 2)]) (vector [Number 0, Number $ pi / 2, Number pi, Number $ pi / 2]) `shouldReturn` pure (vector [Number 3, Number (0 :+ 1), Number 1, Number (-2 :+ 3)])

    describe [G.identical] $ do
      describe "depth" $ do
        it "returns 0 for simple scalars" $ do
          m P.identical (scalar $ Number 7) `shouldReturn` pure (scalar $ Number 0)
        it "returns one more than the depth of the contents for scalar boxes" $ do
          m P.identical (scalar $ box $ vector [Number 1, Number 2, Number 3]) `shouldReturn` pure (scalar $ Number 2)
        it "returns 1 for simple arrays" $ do
          m P.identical (vector [Number 1, Number 2, Number 3]) `shouldReturn` pure (scalar $ Number 1)
          m P.identical (fromMajorCells [vector [Number 1, Number 2], vector [Number 3, Number 4]]) `shouldReturn` pure (scalar $ Number 1)
        it "returns the correct depth for complex arrays" $ do
          m P.identical (vector [box $ vector [Number 1, box $ vector [Number 2, Number 3]], box $ vector [Number 4, Number 5, box $ vector [Number 6, box $ vector [Number 7, Number 8], box $ scalar $ box $ scalar $ box $ vector [Number 9]]]]) `shouldReturn` pure (scalar $ Number 6)

    describe [G.notIdentical] $ do
      describe "tally" $ do
        it "returns the length of a vector" $ do
          m P.notIdentical (vector [Number 1, Number 2, Number 3]) `shouldReturn` pure (scalar $ Number 3)
        it "returns the count of major cells for a higher-rank vector" $ do
          m P.notIdentical (fromMajorCells [vector [Number 1, Number 2, Number 3], vector [Number 4, Number 5, Number 6]]) `shouldReturn` pure (scalar $ Number 2)
        it "returns 1 for a scalar" $ do
          m P.notIdentical (scalar $ Number 10) `shouldReturn` pure (scalar $ Number 1)

    describe [G.identical, G.notIdentical, G.precedes, G.precedesOrIdentical, G.succeedsOrIdentical, G.succeeds] $ do
      describe "tao comparisons" $ do
        it "compares arrays" $ do
          let l = vector [box $ vector [Number 1, Number 1, Number 1], box $ vector [Number 2, Number 2, Number 2], box $ vector [Number 3, Number 3, Number 3]]
          let r = vector [box $ vector [Number 2, Number 2, Number 2], box $ vector [Number 2, Number 2, Number 2], box $ vector [Number 2, Number 2, Number 2]]
          Right identical <- af P.each P.identical
          Right notIdentical <- af P.each P.notIdentical
          Right precedes <- af P.each P.precedes
          Right precedesOrIdentical <- af P.each P.precedesOrIdentical
          Right succeedsOrIdentical <- af P.each P.succeedsOrIdentical
          Right succeeds <- af P.each P.succeeds
          d identical l r `shouldReturn` pure (vector [Number 0, Number 1, Number 0])
          d notIdentical l r `shouldReturn` pure (vector [Number 1, Number 0, Number 1])
          d precedes l r `shouldReturn` pure (vector [Number 1, Number 0, Number 0])
          d precedesOrIdentical l r `shouldReturn` pure (vector [Number 1, Number 1, Number 0])
          d succeedsOrIdentical l r `shouldReturn` pure (vector [Number 0, Number 1, Number 1])
          d succeeds l r `shouldReturn` pure (vector [Number 0, Number 0, Number 1])

    describe [G.precedesOrIdentical] $ do
      describe "sort up" $ do
        it "sorts an array ascending" $ do
          m P.precedesOrIdentical (vector [Number 7, Number 9, Number 2, Number 2]) `shouldReturn` pure (vector [Number 2, Number 2, Number 7, Number 9])
    
    describe [G.succeedsOrIdentical] $ do
      describe "sort down" $ do
        it "sorts an array descending" $ do
          m P.succeedsOrIdentical (vector [Number 7, Number 9, Number 2, Number 2]) `shouldReturn` pure (vector [Number 9, Number 7, Number 2, Number 2])

    describe [G.minimal, G.maximal] $ do
      describe "minimal and maximal" $ do
        it "returns the smallest and largest of two arrays" $ do
          d P.minimal (vector [Number 1, Number 2, Number 3]) (vector [Number 4, Number 5, Number 6]) `shouldReturn` pure (vector [Number 1, Number 2, Number 3])
          d P.maximal (vector [Number 1, Number 2, Number 3]) (vector [Number 4, Number 5, Number 6]) `shouldReturn` pure (vector [Number 4, Number 5, Number 6])

    describe [G.rho] $ do
      describe "shape" $ do
        it "returns the shape of an array" $ do
          m P.rho (vector [Number 1, Number 2, Number 3]) `shouldReturn` pure (vector [Number 3])
          m P.rho (fromMajorCells [vector [Number 1, Number 2, Number 3], vector [Number 4, Number 5, Number 6]]) `shouldReturn` pure (vector [Number 2, Number 3])
      describe "reshape" $ do
        it "reshapes a vector into a matrix" $ do
          d P.rho (vector [Number 2, Number 3]) (vector [Number 1, Number 2, Number 3, Number 4, Number 5, Number 6]) `shouldReturn` pure (fromMajorCells [vector [Number 1, Number 2, Number 3], vector [Number 4, Number 5, Number 6]])
        it "recycles elements when there aren't enough" $ do
          d P.rho (vector [Number 2, Number 3]) (vector [Number 1, Number 2, Number 3, Number 4]) `shouldReturn` pure (fromMajorCells [vector [Number 1, Number 2, Number 3], vector [Number 4, Number 1, Number 2]])
        it "discards extra elements" $ do
          d P.rho (vector [Number 2, Number 3]) (vector [Number 1, Number 2, Number 3, Number 4, Number 5, Number 6, Number 7, Number 8]) `shouldReturn` pure (fromMajorCells [vector [Number 1, Number 2, Number 3], vector [Number 4, Number 5, Number 6]])
        it "transforms empty vectors" $ do
          d P.rho (vector [Number 0]) (Array [0, 1] []) `shouldReturn` pure (vector [])
        it "fails when turning an empty vector into a nonempty vector" $ do
          e2m <$> d P.rho (vector [Number 1]) (vector []) `shouldReturn` Nothing
        it "takes the first item when reshaping to a scalar" $ do
          d P.rho (vector []) (vector [Number 1, Number 2, Number 3]) `shouldReturn` pure (scalar $ Number 1)
        it "fails when multiple negative numbers, or a negative number that's not ¯1" $ do
          e2m <$> d P.rho (vector [Number -2, Number 3]) (vector [Number 1, Number 2]) `shouldReturn` Nothing
          e2m <$> d P.rho (vector [Number 0, Number -1, Number -1]) (vector [Number 1, Number 2]) `shouldReturn` Nothing
        it "fails when there's a ¯1 and the amount of elements can't be fit" $ do
          e2m <$> d P.rho (vector [Number 2, Number -1]) (vector [Number 1, Number 2, Number 3]) `shouldReturn` Nothing
          e2m <$> d P.rho (vector [Number 0, Number -1]) (vector [Number 1, Number 2, Number 3]) `shouldReturn` Nothing
        it "reshapes arrays to a size that fits when a ¯1 is given" $ do
          d P.rho (vector [Number -1, Number 3]) (vector [Number 1, Number 2, Number 3, Number 4, Number 5, Number 6]) `shouldReturn` pure (fromMajorCells [vector [Number 1, Number 2, Number 3], vector [Number 4, Number 5, Number 6]])
          d P.rho (vector [Number 2, Number -1]) (vector [Number 1, Number 2, Number 3, Number 4, Number 5, Number 6]) `shouldReturn` pure (fromMajorCells [vector [Number 1, Number 2, Number 3], vector [Number 4, Number 5, Number 6]])
          d P.rho (vector [Number -1, Number 2]) (vector [Number 1, Number 2, Number 3, Number 4, Number 5, Number 6]) `shouldReturn` pure (fromMajorCells [vector [Number 1, Number 2], vector [Number 3, Number 4], vector [Number 5, Number 6]])
      
    describe [G.ravel] $ do
      describe "ravel" $ do
        it "returns the ravel of an array" $ do
          m P.ravel (vector [Number 1, Number 2, Number 3]) `shouldReturn` pure (vector [Number 1, Number 2, Number 3])
          m P.ravel (fromMajorCells [vector [Number 1, Number 2, Number 3], vector [Number 4, Number 5, Number 6]]) `shouldReturn` pure (vector [Number 1, Number 2, Number 3, Number 4, Number 5, Number 6])
          m P.ravel (scalar $ Number 1) `shouldReturn` pure (vector [Number 1])
      describe "laminate" $ do
        it "joins two arrays on a new axis" $ do
          d P.ravel (vector [Number 1, Number 2, Number 3]) (vector [Number 4, Number 5, Number 6]) `shouldReturn` pure (fromMajorCells [vector [Number 1, Number 2, Number 3], vector [Number 4, Number 5, Number 6]])
    
    describe [G.reverse] $ do
      describe "reverse" $ do
        it "reverses arrays" $ do
          m P.reverse (vector [Number 1, Number 2, Number 3]) `shouldReturn` pure (vector [Number 3, Number 2, Number 1])
          m P.reverse (fromMajorCells [vector [Number 1, Number 2, Number 3], vector [Number 4, Number 5, Number 6]]) `shouldReturn` pure (fromMajorCells [vector [Number 4, Number 5, Number 6], vector [Number 1, Number 2, Number 3]])
          m P.reverse (scalar $ Number 1) `shouldReturn` pure (scalar $ Number 1)
      describe "rotate" $ do
        it "rotates arrays" $ do
          d P.reverse (scalar $ Number 1) (vector [Number 1, Number 2, Number 3, Number 4]) `shouldReturn` pure (vector [Number 2, Number 3, Number 4, Number 1])
          d P.reverse (scalar $ Number 3) (vector [Number 1, Number 2, Number 3, Number 4]) `shouldReturn` pure (vector [Number 4, Number 1, Number 2, Number 3])
          d P.reverse (scalar $ Number -1) (vector [Number 1, Number 2, Number 3, Number 4]) `shouldReturn` pure (vector [Number 4, Number 1, Number 2, Number 3])
          d P.reverse (scalar $ Number 0) (vector [Number 1, Number 2, Number 3, Number 4]) `shouldReturn` pure (vector [Number 1, Number 2, Number 3, Number 4])
        it "rotates arrays along multiple axes" $ do
          d P.reverse (vector [Number 1, Number 2]) (fromMajorCells [vector [Number 1, Number 2, Number 3], vector [Number 4, Number 5, Number 6]]) `shouldReturn` pure (fromMajorCells [vector [Number 6, Number 4, Number 5], vector [Number 3, Number 1, Number 2]])
    
    describe [G.pair] $ do
      describe "singleton" $ do
        it "wraps an array into a box into a singleton vector" $ do
          m P.pair (vector [Number 1, Number 2, Number 3]) `shouldReturn` pure (vector [box $ vector [Number 1, Number 2, Number 3]])
      describe "pair" $ do
        it "wraps two arrays into boxes into a pair vector" $ do
          d P.pair (vector [Number 1, Number 2, Number 3]) (vector [Number 4, Number 5, Number 6]) `shouldReturn` pure (vector [box $ vector [Number 1, Number 2, Number 3], box $ vector [Number 4, Number 5, Number 6]])
    
    describe [G.enclose] $ do
      describe "enclose" $ do
        it "wraps arrays into boxes" $ do
          m P.enclose (vector [Number 1, Number 2, Number 3]) `shouldReturn` pure (scalar $ box $ vector [Number 1, Number 2, Number 3])
        it "leaves simple scalars untouched" $ do
          m P.enclose (scalar $ Number 1) `shouldReturn` pure (scalar $ Number 1)
      describe "partitioned enclose" $ do
        it "splits an array in positions marked by true value" $ do
          d P.enclose (vector [Number 1, Number 0, Number 1, Number 0, Number 0, Number 1]) (vector $ Character <$> "abcdef") `shouldReturn` pure (vector [box $ vector $ Character <$> "ab", box $ vector $ Character <$> "cde", box $ vector $ Character <$> "f"])
        it "introduces extra empty partitions with a larger left argument" $ do
          d P.enclose (vector [Number 1, Number 0, Number 2, Number 0]) (vector $ Character <$> "abcd") `shouldReturn` pure (vector [box $ vector $ Character <$> "ab", box $ vector [], box $ vector $ Character <$> "cd"])
        it "allows a short left argument" $ do
          d P.enclose (vector [Number 1, Number 0]) (vector $ Character <$> "abcd") `shouldReturn` pure (vector [box $ vector $ Character <$> "abcd"])
        it "allows a long left argument" $ do
          d P.enclose (vector [Number 1, Number 0, Number 1]) (vector $ Character <$> "ab") `shouldReturn` pure (vector [box $ vector $ Character <$> "ab", box $ vector []])
        it "drops leading entries when there is no zero" $ do 
          d P.enclose (vector [Number 0, Number 1, Number 0]) (vector $ Character <$> "abc") `shouldReturn` pure (vector [box $ vector $ Character <$> "bc"])
    
    describe [G.first] $ do
      describe "first" $ do
        it "returns the first element of a simple array" $ do
          m P.first (vector [Number 1, Number 2, Number 3]) `shouldReturn` pure (scalar $ Number 1)
        it "discloses boxes arrays" $ do
          m P.first (vector [box $ vector [Number 1, Number 2, Number 3], box $ vector [Number 4, Number 5, Number 6]]) `shouldReturn` pure (vector [Number 1, Number 2, Number 3])
        it "fails on an empty array" $ do
          e2m <$> m P.first (vector []) `shouldReturn` Nothing
      describe "keys" $ do
        it "returns the keys of a dictionary" $ do
          m P.first (dictionary [(Number 1, Number 2), (Number 3, Number 4)]) `shouldReturn` pure (vector [Number 1, Number 3])
    
    describe [G.last] $ do
      describe "last" $ do
        it "returns the last element of a simple array" $ do
          m P.last (vector [Number 1, Number 2, Number 3]) `shouldReturn` pure (scalar $ Number 3)
        it "discloses boxes arrays" $ do
          m P.last (vector [box $ vector [Number 1, Number 2, Number 3], box $ vector [Number 4, Number 5, Number 6]]) `shouldReturn` pure (vector [Number 4, Number 5, Number 6])
        it "fails on an empty array" $ do
          e2m <$> m P.last (vector []) `shouldReturn` Nothing
      describe "values" $ do
        it "returns the values of a dictionary" $ do
          m P.last (dictionary [(Number 1, Number 2), (Number 3, Number 4)]) `shouldReturn` pure (vector [Number 2, Number 4])
      describe "from" $ do
        it "indexes major cells of an array" $ do
          d P.last (vector [Number 0, Number -1]) (vector [Number 1, Number 2, Number 3]) `shouldReturn` pure (vector [Number 1, Number 3])
        it "does scatter indexing" $ do
          d P.last (fromMajorCells [vector [Number 0, Number 2], vector [Number 1, Number -1]]) (vector [Number 1, Number 2, Number 3]) `shouldReturn` pure (fromMajorCells [vector [Number 1, Number 3], vector [Number 2, Number 3]])
          d P.last (vector [box $ vector [Number 0, Number 0], box $ vector [Number 1, Number 1]]) (fromMajorCells [vector [Number 1, Number 2], vector [Number 3, Number 4]]) `shouldReturn` pure (vector [Number 1, Number 4])
    
    describe [G.take] $ do
      describe "take" $ do
        it "takes the first elements of arrays" $ do
          d P.take (scalar $ Number 2) (vector [Number 1, Number 2, Number 3]) `shouldReturn` pure (vector [Number 1, Number 2])
        it "takes the last elements of arrays" $ do
          d P.take (scalar $ Number -2) (vector [Number 1, Number 2, Number 3]) `shouldReturn` pure (vector [Number 2, Number 3])
        it "takes elements across multiple axes" $ do
          d P.take (vector [Number 1, Number 2]) (fromMajorCells [vector [Number 1, Number 2, Number 3], vector [Number 4, Number 5, Number 6]]) `shouldReturn` pure (fromMajorCells [vector [Number 1, Number 2]])
      describe "mix" $ do
        it "mixes nested arrays" $ do
          m P.take (vector [box $ vector [Number 1, Number 2], box $ vector [Number 3, Number 4]]) `shouldReturn` pure (fromMajorCells [vector [Number 1, Number 2], vector [Number 3, Number 4]])

    describe [G.drop] $ do
      describe "drop" $ do
        it "drops the first elements of arrays" $ do
          d P.drop (scalar $ Number 1) (vector [Number 1, Number 2, Number 3]) `shouldReturn` pure (vector [Number 2, Number 3])
        it "drops the last elements of arrays" $ do
          d P.drop (scalar $ Number -1) (vector [Number 1, Number 2, Number 3]) `shouldReturn` pure (vector [Number 1, Number 2])
        it "drops elements across multiple axes" $ do
          d P.drop (vector [Number 1, Number 2]) (fromMajorCells [vector [Number 1, Number 2, Number 3], vector [Number 4, Number 5, Number 6]]) `shouldReturn` pure (fromMajorCells [vector [Number 6]])
      describe "major cells" $ do
        it "returns major cells of an array" $ do
          m P.drop (fromMajorCells [vector [Number 1, Number 2], vector [Number 3, Number 4]]) `shouldReturn` pure (vector [box $ vector [Number 1, Number 2], box $ vector [Number 3, Number 4]])
      describe "key-value pairs" $ do
        it "returns key-value pairs of a dictionary" $ do
          m P.drop (dictionary [(Character 'a', Number 1), (Character 'b', Number 2)]) `shouldReturn` pure (vector [box $ vector [Character 'a', Number 1], box $ vector [Character 'b', Number 2]])

    describe [G.left] $ do
      describe "same" $ do
        it "returns the argument unchanged" $ do
          m P.left (scalar $ Number 1) `shouldReturn` pure (scalar $ Number 1)
      describe "left" $ do
        it "returns the left argument unchnanged" $ do
          d P.left (scalar $ Number 1) (scalar $ Number 2) `shouldReturn` pure (scalar $ Number 1)

    describe [G.right] $ do
      describe "same" $ do
        it "returns the argument unchanged" $ do
          m P.right (scalar $ Number 1) `shouldReturn` pure (scalar $ Number 1)
      describe "right" $ do
        it "returns the right argument unchnanged" $ do
          d P.right (scalar $ Number 1) (scalar $ Number 2) `shouldReturn` pure (scalar $ Number 2)
    
    describe [G.iota] $ do
      describe "index generator" $ do
        it "returns a vector of indices for scalar arguments" $ do
          m P.iota (scalar $ Number 3) `shouldReturn` pure (vector [Number 0, Number 1, Number 2])
        it "returns a higher-rank array for vector arguments" $ do
          m P.iota (vector [Number 2, Number 3]) `shouldReturn` pure (fromMajorCells
            [ vector [box $ vector [Number 0, Number 0], box $ vector [Number 0, Number 1], box $ vector [Number 0, Number 2]]
            , vector [box $ vector [Number 1, Number 0], box $ vector [Number 1, Number 1], box $ vector [Number 1, Number 2]] ])
        it "returns an empty vector on zero" $ do
          m P.iota (scalar $ Number 0) `shouldReturn` pure (vector [])
      describe "index of" $ do
        it "returns the index of the first occurrence of a cell of an array in the major cells of another" $ do
          d P.iota (vector $ Character <$> "hello world") (vector $ Character <$> "lw") `shouldReturn` pure (vector [Number 2, Number 6])
        it "returns one more than the tally when the cell is not found" $ do
          d P.iota (vector $ Character <$> "hello world") (scalar $ Character 'x') `shouldReturn` pure (scalar $ Number 11)
      describe "index of dictionary" $ do
        it "returns the key of the first occurrence of a value in a dictionary" $ do
          d P.iota (dictionary [(Number 1, Number 2), (Number 3, Number 4)]) (scalar $ Number 4) `shouldReturn` pure (scalar $ Number 3)
          e2m <$> d P.iota (dictionary [(Number 1, Number 2), (Number 3, Number 4)]) (scalar $ Number 5) `shouldReturn` Nothing
    
    describe [G.indices] $ do
      describe "where" $ do
        it "returns indices of true values in a vector" $ do
          m P.indices (vector [Number 0, Number 1, Number 0, Number 1]) `shouldReturn` pure (vector [Number 1, Number 3])
        it "returns indices of true values in a higher-rank array" $ do
          m P.indices (fromMajorCells [vector [Number 0, Number 1, Number 0], vector [Number 1, Number 0, Number 1]]) `shouldReturn` pure (vector
            [ box $ vector [Number 0, Number 1]
            , box $ vector [Number 1, Number 0]
            , box $ vector [Number 1, Number 2] ])
        it "returns multiple indices for natural values" $ do
          m P.indices (vector [Number 0, Number 1, Number 2]) `shouldReturn` pure (vector [Number 1, Number 2, Number 2])
      describe "interval index" $ do
        it "puts values in bins described by an array" $ do
          d P.indices (vector [Number 3, Number 7, Number 9]) (vector [Number 1, Number 5, Number 8, Number 10]) `shouldReturn` pure (vector [Number 0, Number 1, Number 2, Number 3])
        it "marks values equal to a bin as to their right" $ do
          d P.indices (vector [Number 3, Number 7, Number 9]) (scalar $ Number 7) `shouldReturn` pure (scalar $ Number 2)
        it "marks values in the rightmost bin when two bin definitions are equal" $ do
          d P.indices (vector [Number 1, Number 3, Number 3, Number 5]) (scalar $ Number 4) `shouldReturn` pure (scalar $ Number 3)
        it "fails on non-sorted left arguments" $ do
          e2m <$> d P.indices (vector [Number 9, Number 3]) (scalar $ Number 1) `shouldReturn` Nothing

    describe [G.replicate] $ do
      describe "replicate" $ do
        it "replicates a vector" $ do
          d P.replicate (vector [Number 1, Number 0, Number 2]) (vector [Number 1, Number 2, Number 3]) `shouldReturn` pure (vector [Number 1, Number 3, Number 3])
        it "replicates a higher-rank array" $ do
          d P.replicate (vector [Number 1, Number 0, Number 2]) (fromMajorCells [vector [Number 1, Number 2], vector [Number 3, Number 4], vector [Number 5, Number 6]]) `shouldReturn` pure (fromMajorCells [vector [Number 1, Number 2], vector [Number 5, Number 6], vector [Number 5, Number 6]])
        it "fails when the lengths of the arguments don't match" $ do
          e2m <$> d P.replicate (vector [Number 1, Number 0]) (vector [Number 2, Number 1, Number 5]) `shouldReturn` Nothing
      describe "dictionary replicate" $ do
        it "selects entries of a dictionary" $ do
          d P.replicate (dictionary [(Character 'a', Number 1), (Character 'b', Number 0)]) (dictionary [(Character 'a', Number 1), (Character 'b', Number 2), (Character 'c', Number 3)]) `shouldReturn` pure (dictionary [(Character 'a', Number 1)])

    describe [G.abs] $ do
      describe "abs" $ do
        it "returns the absolute value of real numbers" $ do
          m P.abs (vector [Number 1, Number -2, Number 0]) `shouldReturn` pure (vector [Number 1, Number 2, Number 0])
        it "returns the magnitude of complex numbers" $ do
          m P.abs (vector [Number (1 :+ 1), Number (3 :+ -4)]) `shouldReturn` pure (vector [Number $ sqrt 2, Number 5])
      describe "case fold" $ do
        it "case folds characters" $ do
          m P.abs (vector $ Character <$> "a.Aσς") `shouldReturn` pure (vector $ Character <$> "a.aσσ")
      describe "residue" $ do
        it "returns the residue of division" $ do
          d P.abs (vector [Number 2, Number 2, Number 0.5]) (vector [Number 4, Number -5, Number 3.2]) `shouldReturn` pure (vector [Number 0, Number 1, Number 0.2])
        it "returns the right argument with zero left argument" $ do
          d P.abs (scalar $ Number 0) (scalar $ Number 3) `shouldReturn` pure (scalar $ Number 3)
    
    describe [G.phase] $ do
      describe "phase" $ do
        it "returns the phase of complex numbers" $ do
          m P.phase (vector [Number 1, Number -1, Number (0 :+ 1), Number (1 :+ 1)]) `shouldReturn` pure (vector [Number 0, Number pi, Number $ pi / 2, Number $ pi / 4])
      describe "arctangent" $ do
        it "returns the dyadic arctangent of two complex numbers" $ do
          d P.phase (vector [Number 1, Number 1, Number 1]) (vector [Number 0, Number 1, Number -1]) `shouldReturn` pure (vector [Number $ pi / 2 :+ 0, Number $ pi / 4 :+ 0, Number $ 3 * pi / 4 :+ 0])

    describe [G.real] $ do
      describe "real part" $ do
        it "returns the real part of the argument" $ do
          m P.real (vector [Number 1, Number (2 :+ 3), Number (0 :+ 5)]) `shouldReturn` pure (vector [Number 1, Number 2, Number 0])
      
    describe [G.imag] $ do
      describe "imaginary part" $ do
        it "returns the imaginary part of the argument" $ do
          m P.imag (vector [Number 1, Number (2 :+ 3), Number (0 :+ 5)]) `shouldReturn` pure (vector [Number 0, Number 3, Number 5])
      
    describe [G.union] $ do
      describe "unique" $ do
        it "returns unique elements of an array" $ do
          m P.union (vector [Number 1, Number 2, Number 1, Number 3, Number 2]) `shouldReturn` pure (vector [Number 1, Number 2, Number 3])
      describe "union" $ do
        it "returns the union of two arrays" $ do
          d P.union (vector [Number 1, Number 1, Number 3]) (vector [Number 1, Number 2, Number 2, Number 3, Number 3]) `shouldReturn` pure (vector [Number 1, Number 1, Number 3, Number 2, Number 2])
    
    describe [G.intersection] $ do
      describe "intersection" $ do
        it "returns the intersection of two arrays" $ do
          d P.intersection (vector [Number 1, Number 2, Number 1]) (vector [Number 2, Number 2, Number 3]) `shouldReturn` pure (vector [Number 2])
    
    describe [G.difference] $ do
      describe "not" $ do
        it "inverts boolean values" $ do
          m P.difference (vector [Number 0, Number 1]) `shouldReturn` pure (vector [Number 1, Number 0])
        it "inverts probabilities" $ do
          m P.difference (vector [Number 0.25, Number 0.5, Number 0.9]) `shouldReturn` pure (vector [Number 0.75, Number 0.5, Number 0.1])
      describe "difference" $ do
        it "returns the difference of two arrays" $ do
          d P.difference (vector [Number 1, Number 1, Number 2, Number 2]) (vector [Number 2, Number 3]) `shouldReturn` pure (vector [Number 1, Number 1])
    
    describe [G.symdiff] $ do
      describe "symmetric difference" $ do
        it "returns the symmetric difference of two arrays" $ do
          d P.symdiff (vector [Number 1, Number 1, Number 2, Number 2]) (vector [Number 2, Number 3]) `shouldReturn` pure (vector [Number 1, Number 1, Number 3])
    
    describe [G.element] $ do
      describe "enlist" $ do
        it "flattens a nested array" $ do
          m P.element (fromMajorCells [vector [Number 1, box $ vector [Number 2, Number 3]], vector [Number 4, box $ vector [Number 5, box $ vector [Number 6, Number 7]]]]) `shouldReturn` pure (vector [Number 1, Number 2, Number 3, Number 4, Number 5, Number 6, Number 7])
      describe "element of" $ do
        it "checks whether cells of an array are members of major cells of another array" $ do
          d P.element (vector [Number 2, Number 4]) (vector [Number 1, Number 2, Number 3]) `shouldReturn` pure (vector [Number 1, Number 0])
          d P.element (fromMajorCells [vector $ Character <$> "high", vector $ Character <$> "rank"]) (vector $ Character <$> "list arg") `shouldReturn` pure (fromMajorCells [vector [Number 0, Number 1, Number 1, Number 0], vector [Number 1, Number 1, Number 0, Number 0]])
          d P.element (fromMajorCells [vector [Number 1, Number 2], vector [Number 3, Number 4], vector [Number 5, Number 6], vector [Number 7, Number 8]]) (fromMajorCells [vector [Number 1, Number 2], vector [Number 3, Number 4], vector [Number 1, Number 2], vector [Number 3, Number 4], vector [Number 1, Number 2]])
            `shouldReturn` pure (vector [Number 1, Number 1, Number 0, Number 0])
      describe "element of dictionary" $ do
        it "checks whether a value appears in a dictionary" $ do
          d P.element (scalar $ Number 3) (dictionary [(Number 1, Number 2), (Number 3, Number 4)]) `shouldReturn` pure (scalar $ Number 0)
          d P.element (scalar $ Number 4) (dictionary [(Number 1, Number 2), (Number 3, Number 4)]) `shouldReturn` pure (scalar $ Number 1)

    describe [G.histogram] $ do
      describe "count" $ do
        it "counts the number of occurrences cells of an array in the the major cells of another array" $ do
          d P.histogram (vector [Number 1, Number 2, Number 4]) (vector [Number 1, Number 3, Number 2, Number 1, Number 1, Number 5]) `shouldReturn` pure (vector [Number 3, Number 1, Number 0])
      describe "count dictionary" $ do
        it "counts the number of occurrences of a value in a dictionary" $ do
          d P.histogram (scalar $ Number 3) (dictionary [(Number 1, Number 2), (Number 3, Number 4)]) `shouldReturn` pure (scalar $ Number 0)
          d P.histogram (scalar $ Number 4) (dictionary [(Number 1, Number 2), (Number 3, Number 4)]) `shouldReturn` pure (scalar $ Number 1)
          d P.histogram (scalar $ Number 4) (dictionary [(Number 1, Number 4), (Number 3, Number 4)]) `shouldReturn` pure (scalar $ Number 2)

    describe [G.squad] $ do
      describe "index" $ do
        it "indexes cells of an array" $ do
          d P.squad (vector [Number 0, box $ vector [Number 0, Number -2]]) (fromMajorCells [vector [Number 1, Number 2, Number 3], vector [Number 4, Number 5, Number 6]]) `shouldReturn` pure (vector [Number 1, Number 2])

    describe [G.rank] $ do
      describe "rank" $ do
        it "returns the rank of an array" $ do
          m P.rank (scalar $ Number 1) `shouldReturn` pure (scalar $ Number 0)
          m P.rank (vector [Number 1, Number 2, Number 3]) `shouldReturn` pure (scalar $ Number 1)
          m P.rank (fromMajorCells [vector [Number 1, Number 2], vector [Number 3, Number 4]]) `shouldReturn` pure (scalar $ Number 2)
      describe "rerank" $ do
        it "leaves arrays unchanged if argument is equal to the rank" $ do
          d P.rank (scalar $ Number 1) (vector [Number 1, Number 2]) `shouldReturn` pure (vector [Number 1, Number 2])
        it "promotes arrays if argument is greater than the rank" $ do
          d P.rank (scalar $ Number 2) (vector [Number 1, Number 2]) `shouldReturn` pure (fromMajorCells [vector [Number 1, Number 2]])
          d P.rank (scalar $ Number 2) (scalar $ Number 1) `shouldReturn` pure (fromMajorCells [vector [Number 1]])
        it "demotes arrays if argument is less than the rank" $ do
          d P.rank (scalar $ Number 1) (fromMajorCells [vector [Number 1, Number 2], vector [Number 3, Number 4]]) `shouldReturn` pure (vector [Number 1, Number 2, Number 3, Number 4])
          d P.rank (scalar $ Number 0) (fromMajorCells [vector [Number 1, Number 2], vector [Number 3, Number 4]]) `shouldReturn` pure (scalar $ Number 1)

    describe [G.catenate] $ do
      describe "join" $ do
        it "joins a boxed vector into a flat one" $ do
          m P.catenate (vector [box $ vector $ Character <$> "ab", box $ vector $ Character <$> "cdef"]) `shouldReturn` pure (vector $ Character <$> "abcdef")
        it "returns an empty vector for an empty vector" $ do
          m P.catenate (vector []) `shouldReturn` pure (vector [])

      describe "catenate" $ do
        it "pairs scalars" $ do
          d P.catenate (scalar $ Number 1) (scalar $ Number 2) `shouldReturn` pure (vector [Number 1, Number 2])
        it "catenates arrays of equal rank" $ do
          d P.catenate (vector [Number 1, Number 2]) (vector [Number 3, Number 4]) `shouldReturn` pure (vector [Number 1, Number 2, Number 3, Number 4])
          d P.catenate (fromMajorCells [vector [Number 1, Number 2], vector [Number 3, Number 4]]) (fromMajorCells [vector [Number 5, Number 6], vector [Number 7, Number 8]]) `shouldReturn`
            pure (fromMajorCells [vector [Number 1, Number 2], vector [Number 3, Number 4], vector [Number 5, Number 6], vector [Number 7, Number 8]])
        it "promotes arrays once if necessary" $ do
          d P.catenate (vector [Number 1, Number 2]) (fromMajorCells [vector [Number 3, Number 4], vector [Number 5, Number 6]]) `shouldReturn`
            pure (fromMajorCells [vector [Number 1, Number 2], vector [Number 3, Number 4], vector [Number 5, Number 6]])
          d P.catenate (fromMajorCells [vector [Number 1, Number 2], vector [Number 3, Number 4]]) (vector [Number 5, Number 6]) `shouldReturn`
            pure (fromMajorCells [vector [Number 1, Number 2], vector [Number 3, Number 4], vector [Number 5, Number 6]])
        it "reshapes scalars" $ do
          d P.catenate (scalar $ Number 10) (fromMajorCells [vector [Number 1, Number 2], vector [Number 3, Number 4]]) `shouldReturn`
            pure (fromMajorCells [vector [Number 10, Number 10], vector [Number 1, Number 2], vector [Number 3, Number 4]])
          d P.catenate (fromMajorCells [vector [Number 1, Number 2], vector [Number 3, Number 4]]) (scalar $ Number 10) `shouldReturn`
            pure (fromMajorCells [vector [Number 1, Number 2], vector [Number 3, Number 4], vector [Number 10, Number 10]])
        it "fails with mismatched trailing shapes" $ do
          e2m <$> d P.catenate (fromMajorCells [vector [Number 1, Number 2], vector [Number 3, Number 4]]) (fromMajorCells [vector [Number 5, Number 6, Number 7], vector [Number 8, Number 9, Number 10]]) `shouldReturn` Nothing

    describe [G.gradeUp] $ do
      describe "grade up" $ do
        it "grades an array ascending" $ do
          m P.gradeUp (vector [Number 4, Number 2, Number 10]) `shouldReturn` pure (vector [Number 1, Number 0, Number 2])
        it "is stable" $ do
          m P.gradeUp (vector [Number 1, Number 5, Number 1]) `shouldReturn` pure (vector [Number 0, Number 2, Number 1])
      describe "sort by up" $ do
        it "sorts an array according to the ascending grade of another" $ do
          d P.gradeUp (vector $ Character <$> "hello") (vector [Number 5, Number 9, Number -2, Number 10, Number 3]) `shouldReturn` pure (vector $ Character <$> "lohel")
        it "is stable" $ do
          d P.gradeUp (vector $ Character <$> "hi!") (vector [Number 9, Number 2, Number 2]) `shouldReturn` pure (vector $ Character <$> "i!h")

    describe [G.gradeDown] $ do
      describe "grade down" $ do
        it "grades an array descending" $ do
          m P.gradeDown (vector [Number 4, Number 2, Number 10]) `shouldReturn` pure (vector [Number 2, Number 0, Number 1])
        it "is stable" $ do
          m P.gradeDown (vector [Number 1, Number 5, Number 1]) `shouldReturn` pure (vector [Number 1, Number 0, Number 2])
      describe "sort by down" $ do
        it "sorts an array according to the descending grade of another" $ do
          d P.gradeDown (vector $ Character <$> "hello") (vector [Number 5, Number 9, Number -2, Number 10, Number 3]) `shouldReturn` pure (vector $ Character <$> "lehol")
        it "is stable" $ do
          d P.gradeDown (vector $ Character <$> "hi!") (vector [Number 9, Number 2, Number 2]) `shouldReturn` pure (vector $ Character <$> "hi!")

    describe [G.transpose] $ do
      describe "monad transpose" $ do
        it "transposes an array" $ do
          m P.transpose (fromMajorCells [vector [Number 1, Number 2], vector [Number 3, Number 4]]) `shouldReturn` pure (fromMajorCells [vector [Number 1, Number 3], vector [Number 2, Number 4]])
      describe "dyad transpose" $ do
        it "reorders axes of an array" $ do
          d P.transpose (vector [Number 1, Number 0]) (fromMajorCells [vector [Number 1, Number 2], vector [Number 3, Number 4]]) `shouldReturn` pure (fromMajorCells [vector [Number 1, Number 3], vector [Number 2, Number 4]])
        it "extracts diagonals" $ do
          d P.transpose (vector [Number 0, Number 0]) (fromMajorCells [vector [Number 1, Number 2], vector [Number 3, Number 4]]) `shouldReturn` pure (vector [Number 1, Number 4])
      describe "inverted table" $ do
        it "returns the inverted table of a dictionary" $ do
          m P.transpose (dictionary [(Character 'a', Number 1), (Character 'b', Number 2)]) `shouldReturn` pure (vector [box $ vector [Character 'a', Character 'b'], box $ vector [Number 1, Number 2]])

    describe [G.matrixInverse] $ do
      describe "matrix inverse" $ do
        it "returns the inverse of a square matrix" $ do
          m P.matrixInverse (fromMajorCells [vector [Number 0, Number 0, Number 1], vector [Number 1, Number 0, Number 0], vector [Number 0, Number 1, Number 0]]) `shouldReturn`
            pure (fromMajorCells [vector [Number 0, Number 1, Number 0], vector [Number 0, Number 0, Number 1], vector [Number 1, Number 0, Number 0]])
        it "returns the left inverse of a non-square matrix" $ do
          m P.matrixInverse (fromMajorCells [vector [Number 1, Number 2], vector [Number 3, Number 4], vector [Number 5, Number 6]]) `shouldReturn`
            pure (fromMajorCells [vector [Number (-4 / 3), Number (-1 / 3), Number (2 / 3)], vector [Number (13 / 12), Number (1 / 3), Number (-5 / 12)]])
        it "returns the reciprocal of a scalar" $ do
          m P.matrixInverse (scalar $ Number 2) `shouldReturn` pure (scalar $ Number 0.5)
        it "returns the invere of the transpose of a vector" $ do
          m P.matrixInverse (vector [Number 1, Number 2, Number 1]) `shouldReturn` pure (vector [Number (1 / 6), Number (1 / 3), Number (1 / 6)])
      describe "matrix divide" $ do
        it "divides two matrices" $ do
          d P.matrixInverse
            (fromMajorCells [vector [Number 2, Number 1], vector [Number 3, Number 5], vector [Number 9, Number 1]])
            (fromMajorCells [vector [Number 3, Number 1, Number 4], vector [Number 1, Number 5, Number 9], vector [Number 2, Number 6, Number 5]]) `shouldReturn`
            pure (fromMajorCells [vector [Number (10 / 9), Number (-11 / 18)], vector [Number (16 / 9), Number (-5 / 18)], vector [Number (-7 / 9), Number (7 / 9)]])

    describe [G.factorial] $ do
      describe "factorial" $ do
        it "returns the factorial of a natural number" $ do
          m P.factorial (scalar $ Number 3) `shouldReturn` pure (scalar $ Number 6)
        it "returns the factorial of a complex number" $ do
          m P.factorial (scalar $ Number (3 :+ 2)) `shouldReturn` pure (scalar $ Number $ -3.01154037032662022167363895320352158969957852249181859280699134 :+ 1.77016819446711624877987093755267094949031742656012402258841406)
        it "fails for negative integers" $ do
          e2m <$> m P.factorial (scalar $ Number (-1)) `shouldReturn` Nothing
      describe "binomial" $ do
        it "returns the binomial coefficient of two natural numbers" $ do
          d P.factorial (scalar $ Number 3) (scalar $ Number 5) `shouldReturn` pure (scalar $ Number 10)
          d P.factorial (scalar $ Number 5) (scalar $ Number 2) `shouldReturn` pure (scalar $ Number 0)
        it "returns the binomial coefficient of integers" $ do
          d P.factorial (scalar $ Number 5) (scalar $ Number -2) `shouldReturn` pure (scalar $ Number -6)
          d P.factorial (scalar $ Number -1) (scalar $ Number -2) `shouldReturn` pure (scalar $ Number 0)
          d P.factorial (scalar $ Number -8) (scalar $ Number -2) `shouldReturn` pure (scalar $ Number 7)
    
    describe [G.raise] $ do
      describe "raise" $ do
        it "does nothing if the left argument is 0" $ do
          d P.raise (scalar $ Number 0) (vector $ Character <$> "error") `shouldReturn` pure (vector [])
        it "raises an error if the left argument is not 0" $ do
          d P.raise (scalar $ Number 1) (vector $ Character <$> "error") `shouldReturn` throwError (UserError "error")
          d P.raise (scalar $ Number 2) (vector $ Character <$> "error") `shouldReturn` throwError (DomainError "error")
          d P.raise (scalar $ Number 3) (vector $ Character <$> "error") `shouldReturn` throwError (LengthError "error")
          d P.raise (scalar $ Number 4) (vector $ Character <$> "error") `shouldReturn` throwError (RankError "error")
          d P.raise (scalar $ Number 5) (vector $ Character <$> "error") `shouldReturn` throwError (NYIError "error")
          d P.raise (scalar $ Number 6) (vector $ Character <$> "error") `shouldReturn` throwError (SyntaxError "error")
          d P.raise (scalar $ Number 7) (vector $ Character <$> "error") `shouldReturn` throwError (AssertionError "error")
          d P.raise (scalar $ Number 8) (vector $ Character <$> "error") `shouldReturn` throwError (IndexError "error")
          d P.raise (scalar $ Number 9) (vector $ Character <$> "error") `shouldReturn` throwError (IOError "error")
        it "defaults to an user error" $ do
          m P.raise (vector $ Character <$> "error") `shouldReturn` throwError (UserError "error")

    describe [G.decode] $ do
      describe "decode" $ do
        it "decodes a vector of numbers in the mixed base defined by another" $ do
          d P.decode (vector [Number 4, Number 9]) (vector [Number 2, Number 3]) `shouldReturn` pure (scalar $ Number 21)
      describe "scalar decode" $ do
        it "decodes a vector of numbers in the base defined by a number" $ do
          d P.decode (scalar $ Number 4) (vector [Number 2, Number 3]) `shouldReturn` pure (scalar $ Number 11)
      describe "base 2 decode" $ do
        it "decodes a vector of numbers in base 2" $ do
          m P.decode (vector [Number 1, Number 0, Number 1, Number 0]) `shouldReturn` pure (scalar $ Number 10)

    describe [G.encode] $ do
      describe "encode" $ do
        it "encodes a number to the mixed base defined by a vector of numbers" $ do
          d P.encode (vector [Number 4, Number 9]) (scalar $ Number 21) `shouldReturn` pure (vector [Number 2, Number 3])
      describe "scalar encode" $ do
        it "encodes a number to the base defined by a number" $ do
          d P.encode (scalar $ Number 4) (scalar $ Number 11) `shouldReturn` pure (vector [Number 2, Number 3])
        it "exits when reaching a loop" $ do
          d P.encode (scalar $ Number 1.5) (scalar $ Number -12) `shouldReturn` pure (vector [Number -2, Number 0, Number 0.5, Number 0, Number 1, Number 0])
      describe "base 2 encode" $ do
        it "encodes a number to base 2" $ do
          m P.encode (scalar $ Number 10) `shouldReturn` pure (vector [Number 1, Number 0, Number 1, Number 0])

    describe [G.increment] $ do
      describe "increment" $ do
        it "increments a number" $ do
          m P.increment (vector [Number 0, Number 3, Number (2 :+ 1)]) `shouldReturn` pure (vector [Number 1, Number 4, Number (3 :+ 1)])

    describe [G.decrement] $ do
      describe "decrement" $ do
        it "decrements a number" $ do
          m P.decrement (vector [Number 0, Number 3, Number (2 :+ 1)]) `shouldReturn` pure (vector [Number -1, Number 2, Number (1 :+ 1)])
      describe "span" $ do
        it "returns the span between two numbers" $ do
          d P.decrement (scalar $ Number 10) (vector [Number 7, Number 12]) `shouldReturn` pure (vector [Number 4, Number -1])

    describe [G.range] $ do
      describe "one range" $ do
        it "makes a range starting at 1 and ending at the number" $ do
          m P.range (scalar $ Number 4) `shouldReturn` pure (vector [Number 1, Number 2, Number 3, Number 4])
          m P.range (vector [Number 3, Number 2]) `shouldReturn` pure (fromMajorCells
            [ vector [box $ vector [Number 1, Number 1], box $ vector [Number 1, Number 2]]
            , vector [box $ vector [Number 2, Number 1], box $ vector [Number 2, Number 2]]
            , vector [box $ vector [Number 3, Number 1], box $ vector [Number 3, Number 2]] ])
      
      describe "range" $ do
        it "makes a range between two numbers" $ do
          d P.range (scalar $ Number 2) (scalar $ Number 4) `shouldReturn` pure (vector [Number 2, Number 3, Number 4])
          d P.range (vector [Number 2, Number 3]) (vector [Number 4, Number 5]) `shouldReturn` pure (fromMajorCells
            [ vector [box $ vector [Number 2, Number 3], box $ vector [Number 2, Number 4], box $ vector [Number 2, Number 5]]
            , vector [box $ vector [Number 3, Number 3], box $ vector [Number 3, Number 4], box $ vector [Number 3, Number 5]]
            , vector [box $ vector [Number 4, Number 3], box $ vector [Number 4, Number 4], box $ vector [Number 4, Number 5]] ])

    describe [G.keyValue] $ do
      describe "key-value pair" $ do
        it "makes a key-value pair" $ do
          d P.keyValue (vector $ Character <$> "abc") (scalar $ Number 1) `shouldReturn` pure (dictionary [(box $ vector $ Character <$> "abc", box $ scalar $ Number 1)])
      describe "from pairs" $ do
        it "converts a vector ofca pairs to a dictionary" $ do
          m P.keyValue (vector [box $ vector [Character 'a', Character 'b'], box $ vector [Character 'c', Character 'd']]) `shouldReturn` pure (dictionary [(Character 'a', Character 'b'), (Character 'c', Character 'd')])
        it "converts a 2-column matrix to a dictionary" $ do
          m P.keyValue (fromMajorCells [vector [Character 'a', Character 'b'], vector [Character 'c', Character 'd'], vector [Character 'e', Character 'f']]) `shouldReturn` pure (dictionary [(Character 'a', Character 'b'), (Character 'c', Character 'd'), (Character 'e', Character 'f')])

    describe [G.invertedTable] $ do
      describe "from keys and values" $ do
        it "converts vectors of keys and values to a dictionary" $ do
          d P.invertedTable (vector [Character 'a', Character 'b']) (vector [Number 1, Number 2]) `shouldReturn` pure (dictionary [(Character 'a', Number 1), (Character 'b', Number 2)])
      describe "from inverted table" $ do
        it "converts a pair of vectors to a dictionary" $ do
          m P.invertedTable (vector [box $ vector [Character 'a', Character 'b'], box $ vector [Character 'c', Character 'd']]) `shouldReturn` pure (dictionary [(Character 'a', Character 'c'), (Character 'b', Character 'd')])
        it "converts a 2-row matrix to a dictionary" $ do
          m P.invertedTable (fromMajorCells [vector [Character 'a', Character 'b', Character 'c'], vector [Character 'd', Character 'e', Character 'f']]) `shouldReturn` pure (dictionary [(Character 'a', Character 'd'), (Character 'b', Character 'e'), (Character 'c', Character 'f')])

    describe [G.group] $ do
      describe "group" $ do
        it "groups elements of an array by " $ do
          d P.group (vector [Number 0, Number 3, Number 3, Number 1, Number -1]) (vector $ Character <$> "abcde")`shouldReturn` pure (vector [box $ vector $ Character <$> "a", box $ vector $ Character <$> "d", box $ vector [], box $ vector $ Character <$> "bc"])

    describe [G.partition] $ do
      describe "partition" $ do
        it "splits an array where the numbers change" $ do
          d P.partition (vector [Number 1, Number 1, Number 2, Number 2, Number 1]) (vector $ Character <$> "abcde") `shouldReturn` pure (vector [box $ vector $ Character <$> "ab", box $ vector $ Character <$> "cd", box $ vector $ Character <$> "e"])
        it "drops zeros"$ do
          d P.partition (vector [Number 1, Number 1, Number 0, Number 0, Number 4]) (vector $ Character <$> "abcde") `shouldReturn` pure (vector [box $ vector $ Character <$> "ab", box $ vector $ Character <$> "e"])

    describe [G.find] $ do
      describe "find" $ do
        it "marks the top left corner of subarrays in arrays" $ do
          d P.find (scalar $ Number 2) (vector [Number 1, Number 2, Number 3, Number 2, Number 1]) `shouldReturn` pure (vector [Number 0, Number 1, Number 0, Number 1, Number 0])
          d P.find (scalar $ Number 2) (fromMajorCells [vector [Number 1, Number 2, Number 3], vector [Number 4, Number 2, Number 0]]) `shouldReturn` pure (fromMajorCells [vector [Number 0, Number 1, Number 0], vector [Number 0, Number 1, Number 0]])
          d P.find (vector [Number 1, Number 1]) (vector [Number 1, Number 1, Number 1, Number 3]) `shouldReturn` pure (vector [Number 1, Number 1, Number 0, Number 0])
          d P.find (vector [Number 2, Number 4]) (fromMajorCells [vector [Number 1, Number 2, Number 4], vector [Number 2, Number 4, Number 0]]) `shouldReturn` pure (fromMajorCells [vector [Number 0, Number 1, Number 0], vector [Number 1, Number 0, Number 0]])
          d P.find (fromMajorCells [vector [Number 1, Number 2], vector [Number 2, Number 0]]) (fromMajorCells [vector [Number 0, Number 1, Number 2, Number 0], vector [Number 1, Number 2, Number 0, Number 1], vector [Number 2, Number 0, Number 1, Number 2], vector [Number 0, Number 1, Number 2, Number 0]])
            `shouldReturn` pure (fromMajorCells [vector [Number 0, Number 1, Number 0, Number 0], vector [Number 1, Number 0, Number 0, Number 0], vector [Number 0, Number 0, Number 1, Number 0], vector [Number 0, Number 0, Number 0, Number 0]])

  describe "adverbs" $ do
    describe [G.selfie] $ do
      describe "constant" $ do
        it "always returns the operand" $ do
          aam P.selfie (scalar $ Number 1) (scalar $ Number 2) `shouldReturn` pure (scalar $ Number 1)
          aad P.selfie (scalar $ Number 1) (scalar $ Number 2) (scalar $ Number 3) `shouldReturn` pure (scalar $ Number 1)
      describe "duplicate" $ do
        it "calls the operand with the same argument on both sides" $ do
          afm P.selfie P.plus (scalar $ Number 2) `shouldReturn` pure (scalar $ Number 4)
      describe "commute" $ do
        it "calls the operand with the arguments swapped" $ do
          afd P.selfie P.minus (scalar $ Number 2) (scalar $ Number 3) `shouldReturn` pure (scalar $ Number 1)

    describe [G.reduce] $ do
      describe "reduce" $ do
        it "reduces an array left-to-right" $ do
          afm P.reduce P.plus (fromMajorCells [vector [Number 1, Number 2, Number 3], vector [Number 4, Number 5, Number 6]]) `shouldReturn` pure (vector [Number 5, Number 7, Number 9])
          afm P.reduce P.minus (vector [Number 1, Number 2, Number 3]) `shouldReturn` pure (scalar $ Number -4)
      describe "fold" $ do
        it "folds an array left-to-right" $ do
          afd P.reduce P.plus (scalar $ Number 0) (vector [Number 1, Number 2, Number 3]) `shouldReturn` pure (scalar $ Number 6)
          afd P.reduce P.plus (scalar $ Number 0) (vector []) `shouldReturn` pure (scalar $ Number 0)
          afd P.reduce P.minus (scalar $ Number 12) (vector [Number 1, Number 2, Number 3]) `shouldReturn` pure (scalar $ Number 6)

    describe [G.onPrefixes] $ do
      describe "on prefixes" $ do
        it "applies a function to prefixes of an array" $ do
          afm P.onPrefixes P.enclose (vector [Number 1, Number 2, Number 3]) `shouldReturn` pure (vector [box $ vector [Number 1], box $ vector [Number 1, Number 2], box $ vector [Number 1, Number 2, Number 3]])

    describe [G.each] $ do
      describe "monad each" $ do
        it "applies a function to each element of an array" $ do
          afm P.each P.iota (vector [Number 1, Number 2, Number 3]) `shouldReturn` pure (vector [box $ vector [Number 0], box $ vector [Number 0, Number 1], box $ vector [Number 0, Number 1, Number 2]])
      describe "dyad each" $ do
        it "applies a function to each pair of elements of two arrays" $ do
          afd P.each P.intersection (vector [box $ vector [Number 1, Number 2], box $ vector [Number 1, Number 3]]) (vector [box $ vector [Number 1], box $ vector [Number 1, Number 2, Number 3]]) `shouldReturn` pure (vector [box $ vector [Number 1], box $ vector [Number 1, Number 3]])

    describe [G.eachLeft] $ do
      describe "each left" $ do
        it "applies a function to each element of the left argument and to the right argument" $ do
          afd P.eachLeft P.intersection (vector [box $ vector [Number 1, Number 2, Number 4], box $ vector [Number 1, Number 3]]) (vector [Number 1, Number 2, Number 3]) `shouldReturn` pure (vector [box $ vector [Number 1, Number 2], box $ vector [Number 1, Number 3]])

    describe [G.eachRight] $ do
      describe "each right" $ do
        it "applies a function to the left argument and to each element of the right argument" $ do
          afd P.eachRight P.intersection (vector [Number 1, Number 2, Number 3]) (vector [box $ vector [Number 2, Number 3], box $ vector [Number 2, Number 1, Number 2]]) `shouldReturn` pure (vector [box $ vector [Number 2, Number 3], box $ vector [Number 1, Number 2]])

    describe [G.key] $ do
      describe "key" $ do
        it "applies a function to each unique element of the left argument and the corresponding elements of the right argument" $ do
          afd P.key P.pair (vector $ Character <$> "mississippi") (vector $ Number <$> [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]) `shouldReturn` pure (fromMajorCells [vector [Character 'm', box $ vector [Number 1]], vector [Character 'i', box $ vector [Number 2, Number 5, Number 8, Number 11]], vector [Character 's', box $ vector [Number 3, Number 4, Number 6, Number 7]], vector [Character 'p', box $ vector [Number 9, Number 10]]])
        it "fails when the arguments have mismatched lengths" $ do
          e2m <$> afd P.key P.pair (vector []) (vector [Number 1]) `shouldReturn` Nothing
      describe "monad key" $ do
        it "applies a function to each unique element of the argument and the corresponding indices" $ do
          afm P.key P.pair (vector $ Character <$> "mississippi") `shouldReturn` pure (fromMajorCells [vector [Character 'm', box $ vector [Number 0]], vector [Character 'i', box $ vector [Number 1, Number 4, Number 7, Number 10]], vector [Character 's', box $ vector [Number 2, Number 3, Number 5, Number 6]], vector [Character 'p', box $ vector [Number 8, Number 9]]])

    describe [G.onCells] $ do
      describe "on cells" $ do
        it "applies a function to major cells of arrays" $ do
          afm P.onCells P.enclose (fromMajorCells [vector [Number 1, Number 2], vector [Number 3, Number 4]]) `shouldReturn` pure (vector [box $ vector [Number 1, Number 2], box $ vector [Number 3, Number 4]])
          afd P.onCells P.pair (fromMajorCells [vector [Number 1, Number 2], vector [Number 3, Number 4]]) (fromMajorCells [vector [Number 5, Number 6], vector [Number 7, Number 8]]) `shouldReturn` pure (fromMajorCells [vector [box $ vector [Number 1, Number 2], box $ vector [Number 5, Number 6]], vector [box $ vector [Number 3, Number 4], box $ vector [Number 7, Number 8]]])
        it "replaces major cells of an array with a value" $ do
          aam P.onCells (scalar $ Number 3) (fromMajorCells [vector [Number 1, Number 2], vector [Number 3, Number 4]]) `shouldReturn` pure (vector [Number 3, Number 3])

    describe [G.cellsLeft] $ do
      describe "cells left" $ do
        it "applies a function to cells of an array and the whole other array" $ do
          afd P.cellsLeft P.intersection (fromMajorCells [vector [Number 1, Number 2, Number 4], vector [Number 1, Number 3, Number 5]]) (vector [Number 1, Number 2, Number 3]) `shouldReturn` pure (fromMajorCells [vector [Number 1, Number 2], vector [Number 1, Number 3]])

    describe [G.cellsRight] $ do
      describe "cells left" $ do
        it "applies a function to cells of an array and the whole other array" $ do
          afd P.cellsRight P.intersection (vector [Number 1, Number 2, Number 3]) (fromMajorCells [vector [Number 1, Number 2, Number 4], vector [Number 1, Number 3, Number 5]]) `shouldReturn` pure (fromMajorCells [vector [Number 1, Number 2], vector [Number 1, Number 3]])
    
    describe [G.onScalars] $ do
      describe "on scalars" $ do
        it "applies a function to scalars of arrays" $ do
          afm P.onScalars P.pair (fromMajorCells [vector [Number 1, Number 2], vector [Number 3, Number 4]]) `shouldReturn` pure (fromMajorCells [fromMajorCells [vector [Number 1], vector [Number 2]], fromMajorCells [vector [Number 3], vector [Number 4]]])
        it "replaces scalars of an array with a value" $ do
          aam P.onScalars (scalar $ Number 5) (fromMajorCells [vector [Number 1, Number 2], vector [Number 3, Number 4]]) `shouldReturn` pure (fromMajorCells [vector [Number 5, Number 5], vector [Number 5, Number 5]])

    describe [G.onSimpleScalars] $ do
      describe "on simple scalars" $ do
        it "applies a function to simple scalars of arrays" $ do
          afd P.onSimpleScalars P.pair (vector [box $ vector [Number 1, Number 2], Number 3]) (vector [Number 4, box $ vector [Number 5, Number 6]]) `shouldReturn` pure (vector [box $ vector [box $ vector [Number 1, Number 4], box $ vector [Number 2, Number 4]], box $ vector [box $ vector [Number 3, Number 5], box $ vector [Number 3, Number 6]]])
        it "replaces simple scalars of an array with a value" $ do
          aam P.onSimpleScalars (scalar $ Number 5) (vector [box $ vector [Number 1, Number 2], Number 3]) `shouldReturn` pure (vector [box $ vector [Number 5, Number 5], Number 5])

    describe [G.boxed] $ do
      describe "boxed" $ do
        it "encloses the result of a function" $ do
          afm P.boxed P.pair (scalar $ Number 3) `shouldReturn` pure (scalar $ box $ vector [Number 3])
          afd P.boxed P.pair (scalar $ Number 1) (scalar $ Number 9) `shouldReturn` pure (scalar $ box $ vector [Number 1, Number 9])
      
    describe [G.onContents] $ do
      describe "on contents" $ do
        it "discloses the arguments of a function" $ do
          afm P.onContents P.minus (scalar $ box $ vector [Number 1, Number 2]) `shouldReturn` pure (vector [Number -1, Number -2])
          afd P.onContents P.plus (scalar $ box $ vector [Number 1, Number 2]) (scalar $ box $ vector [Number 3, Number 4]) `shouldReturn` pure (vector [Number 4, Number 6])

    describe [G.table] $ do
      describe "table" $ do
        it "computes the outer product of two arrays" $ do
          Right dr <- cff P.atop P.first P.right
          afd P.table P.pair (vector [Number 1, Number 2]) (vector [Number 3, Number 4]) `shouldReturn` pure (fromMajorCells [fromMajorCells [vector [Number 1, Number 3], vector [Number 1, Number 4]], fromMajorCells [vector [Number 2, Number 3], vector [Number 2, Number 4]]])
          afd P.table dr (vector [box $ vector [Number 1, Number 2], box $ vector [Number 3, Number 4]]) (vector [box $ vector [Number 5, Number 6], box $ vector [Number 7, Number 8]]) `shouldReturn`
            pure (fromMajorCells [fromMajorCells [vector [Number 5, Number 6], vector [Number 7, Number 8]], fromMajorCells [vector [Number 5, Number 6], vector [Number 7, Number 8]]])

    describe [G.ident] $ do
      describe "ident" $ do
        it "returns the function operand" $ do
          afm P.ident P.minus (scalar $ Number 3) `shouldReturn` pure (scalar $ Number -3)
          afd P.ident P.minus (scalar $ Number 3) (scalar $ Number 5) `shouldReturn` pure (scalar $ Number -2)
        it "makes an array operand into a constant function" $ do
          aam P.ident (scalar $ Character 'a') (scalar $ Number 1) `shouldReturn` pure (scalar $ Character 'a')
          aad P.ident (scalar $ Character 'a') (scalar $ Number 1) (scalar $ Number 2) `shouldReturn` pure (scalar $ Character 'a')

    describe [G.bitwise] $ do
      describe "monad bitwise" $ do
        it "applies a bitwise operation to a number" $ do
          Right sum <- af P.reduce P.plus
          afm P.bitwise P.difference (scalar $ Number 10) `shouldReturn` pure (scalar $ Number -11)
          afm P.bitwise P.difference (scalar $ Number -10) `shouldReturn` pure (scalar $ Number 9)
          afm P.bitwise P.right (scalar $ Number 10) `shouldReturn` pure (scalar $ Number 10)
          afm P.bitwise P.right (scalar $ Number -10) `shouldReturn` pure (scalar $ Number -10)
          afm P.bitwise P.notIdentical (scalar $ Number 10) `shouldReturn` pure (scalar $ Number 4)
          afm P.bitwise sum (scalar $ Number 10) `shouldReturn` pure (scalar $ Number 2)
      describe "dyad bitwise" $ do
        it "applies a bitwise operation to two numbers" $ do
          afd P.bitwise P.and (scalar $ Number 7) (scalar $ Number 9) `shouldReturn` pure (scalar $ Number 1)
          afd P.bitwise P.and (scalar $ Number -7) (scalar $ Number 9) `shouldReturn` pure (scalar $ Number 9)
          afd P.bitwise P.and (scalar $ Number 7) (scalar $ Number -9) `shouldReturn` pure (scalar $ Number 7)
          afd P.bitwise P.and (scalar $ Number -7) (scalar $ Number -9) `shouldReturn` pure (scalar $ Number -15)
          afd P.bitwise P.or (scalar $ Number 3) (scalar $ Number -11) `shouldReturn` pure (scalar $ Number -9)

  describe "conjunctions" $ do
    describe [G.atop] $ do
      describe "atop" $ do
        it "monadically composes functions with F(Gy)" $ do
          cffm P.atop P.times P.minus (scalar $ Number 3) `shouldReturn` pure (scalar $ Number -1)
        it "dyadically composes functions with F(xGy)" $ do
          cffd P.atop P.times P.minus (scalar $ Number 3) (scalar $ Number 5) `shouldReturn` pure (scalar $ Number -1)

      describe "bind" $ do
        it "binds the left argument to a dyad" $ do
          cafm P.atop (scalar $ Number 1) P.minus (scalar $ Number 2) `shouldReturn` pure (scalar $ Number -1)
        it "binds the right argument to a dyad" $ do
          cfam P.atop P.minus (scalar $ Number 1) (scalar $ Number 2) `shouldReturn` pure (scalar $ Number 1)
        it "fails if called dyadically" $ do
          e2m <$> cafd P.atop (scalar $ Number 1) P.minus (scalar $ Number 2) (scalar $ Number 3) `shouldReturn` Nothing
          e2m <$> cfad P.atop P.minus (scalar $ Number 1) (scalar $ Number 2) (scalar $ Number 3) `shouldReturn` Nothing
    
    describe [G.over] $ do
      describe "over" $ do
        it "monadically composes functions with F(Gy)" $ do
          cffm P.over P.times P.minus (scalar $ Number 3) `shouldReturn` pure (scalar $ Number -1)
        it "dyadically composes functions with (Gx)F(Gy)" $ do
          cffd P.over P.plus P.minus (scalar $ Number 1) (scalar $ Number 2) `shouldReturn` pure (scalar $ Number -3)

      describe "default bind" $ do
        it "binds the argument to a function when called monadically" $ do
          cafm P.over (scalar $ Number 1) P.minus (scalar $ Number 2) `shouldReturn` pure (scalar $ Number -1)
          cfam P.over P.minus (scalar $ Number 1) (scalar $ Number 2) `shouldReturn` pure (scalar $ Number 1)
        it "ignores the operand when called dyadically" $ do
          cafd P.over (scalar $ Number 1) P.plus (scalar $ Number 2) (scalar $ Number 3) `shouldReturn` pure (scalar $ Number 5)
          cfad P.over P.minus (scalar $ Number 1) (scalar $ Number 5) (scalar $ Number 2) `shouldReturn` pure (scalar $ Number 3)
    
    describe [G.reverseAtop] $ do
      describe "reverse atop" $ do
        it "monadically composes functions with G(Fy)" $ do
          cffm P.reverseAtop P.minus P.times (scalar $ Number 3) `shouldReturn` pure (scalar $ Number -1)  
        it "dyadically composes functions with G(xFy)" $ do
          cffd P.reverseAtop P.minus P.times (scalar $ Number 3) (scalar $ Number 5) `shouldReturn` pure (scalar $ Number -1)

    describe [G.reverseOver] $ do
      describe "reverse over" $ do
        it "monadically composes functions with G(Fy)" $ do
          cffm P.reverseOver P.minus P.times (scalar $ Number 3) `shouldReturn` pure (scalar $ Number -1)
        it "dyadically composes functions with (Fx)G(Fy)" $ do
          cffd P.reverseOver P.minus P.plus (scalar $ Number 1) (scalar $ Number 2) `shouldReturn` pure (scalar $ Number -3)
    
    describe [G.leftHook] $ do
      describe "left hook" $ do
        it "monadically composes functions with (Fx)Gx" $ do
          cffm P.leftHook P.minus P.plus (scalar $ Number 3) `shouldReturn` pure (scalar $ Number 0)
        it "dyadically composes functions with (Fx)Gy" $ do
          cffd P.leftHook P.minus P.plus (scalar $ Number 1) (scalar $ Number 2) `shouldReturn` pure (scalar $ Number 1)

    describe [G.rightHook] $ do
      describe "right hook" $ do
        it "monadically composes functions with xF(Gx)" $ do
          cffm P.rightHook P.plus P.minus (scalar $ Number 5) `shouldReturn` pure (scalar $ Number 0)
        it "dyadically composes functions with xF(Gy)" $ do
          cffd P.rightHook P.plus P.minus (scalar $ Number 2) (scalar $ Number 1) `shouldReturn` pure (scalar $ Number 1)

    describe [G.mirror] $ do
      describe "mirror" $ do
        it "dyadically composes functions with (yGx)F(xGy)" $ do
          cffd P.mirror P.pair P.minus (scalar $ Number 3) (scalar $ Number 4) `shouldReturn` pure (vector [Number 1, Number -1])

    describe [G.leftFork] $ do
      describe "left fork" $ do
        it "monadically composes functions with (Fx)Gx" $ do
          cffm P.leftFork P.minus P.plus (scalar $ Number 3) `shouldReturn` pure (scalar $ Number 0)
        it "dyadically composes functions with (xFy)Gy" $ do
          cffd P.leftFork P.minus P.plus (scalar $ Number 1) (scalar $ Number 2) `shouldReturn` pure (scalar $ Number 1)

    describe [G.rightFork] $ do
      describe "right fork" $ do
        it "monadically composes functions with xF(Gx)" $ do
          cffm P.rightFork P.plus P.minus (scalar $ Number 5) `shouldReturn` pure (scalar $ Number 0)
        it "dyadically composes functions with xF(xGy)" $ do
          cffd P.rightFork P.plus P.minus (scalar $ Number 2) (scalar $ Number 1) `shouldReturn` pure (scalar $ Number 3)

    describe [G.atRank] $ do
      describe "at rank" $ do
        it "applies functions to cells of specified rank" $ do
          let a = Array [2, 3, 4] $ Number <$> [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24]
          let b = Array [2, 3, 4] $ Number <$> [24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1]

          cfam P.atRank P.enclose (scalar $ Number -1) a `shouldReturn` pure (vector [box $ Array [3, 4] (Number <$> [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]), box $ Array [3, 4] (Number <$> [13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24])])
          cfam P.atRank P.enclose (scalar $ Number 2) a `shouldReturn` pure (vector [box $ Array [3, 4] (Number <$> [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]), box $ Array [3, 4] (Number <$> [13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24])])
          cfam P.atRank P.enclose (scalar $ Number 1) a `shouldReturn` pure (Array [2, 3]
            [ box $ vector $ Number <$> [1, 2, 3, 4], box $ vector $ Number <$> [5, 6, 7, 8], box $ vector $ Number <$> [9, 10, 11, 12]
            , box $ vector $ Number <$> [13, 14, 15, 16], box $ vector $ Number <$> [17, 18, 19, 20], box $ vector $ Number <$> [21, 22, 23, 24] ])
          
          cfad P.atRank P.pair (scalar $ Number -1) a b `shouldReturn` pure (Array [2, 2]
            [ box $ Array [3, 4] $ Number <$> [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12], box $ Array [3, 4] $ Number <$> [24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13]
            , box $ Array [3, 4] $ Number <$> [13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24], box $ Array [3, 4] $ Number <$> [12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1] ])
          cfad P.atRank P.pair (scalar $ Number 1) a b `shouldReturn` pure (Array [2, 3, 2]
            [ box $ vector $ Number <$> [1, 2, 3, 4], box $ vector $ Number <$> [24, 23, 22, 21]
            , box $ vector $ Number <$> [5, 6, 7, 8], box $ vector $ Number <$> [20, 19, 18, 17]
            , box $ vector $ Number <$> [9, 10, 11, 12], box $ vector $ Number <$> [16, 15, 14, 13]
            
            , box $ vector $ Number <$> [13, 14, 15, 16], box $ vector $ Number <$> [12, 11, 10, 9]
            , box $ vector $ Number <$> [17, 18, 19, 20], box $ vector $ Number <$> [8, 7, 6, 5]
            , box $ vector $ Number <$> [21, 22, 23, 24], box $ vector $ Number <$> [4, 3, 2, 1] ])
          cfad P.atRank P.pair (vector [Number 1, Number 2]) (Array [2, 3] $ Number <$> [1, 2, 3, 4, 5, 6]) b `shouldReturn` pure (Array [2, 2]
            [ box $ vector $ Number <$> [1, 2, 3], box $ Array [3, 4] $ Number <$> [24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13]
            , box $ vector $ Number <$> [4, 5, 6], box $ Array [3, 4] $ Number <$> [12, 11, 10, 9, 8,7, 6, 5, 4, 3, 2, 1] ])

    describe [G.atDepth] $ do
      describe "at depth" $ do
        it "applies functions to nested elements of specified depth" $ do
          -- Adapted from https://github.com/abrudz/primitives/blob/main/depthQA.aplf

          let a = vector [box $ vector [Number 1, Number 2], box $ vector [Number 3, Number 4], box $ vector [Number 5, Number 6]]
          let b = vector [box $ vector [Number 10, Number 20, Number 30], box $ vector [Number 40, Number 50, Number 60]]
          let c = vector [box $ vector $ Character <$> "abc", box $ vector [box $ vector $ Character <$> "def", box $ vector $ Character <$> "ghi"]]

          do
            Right factImpl <- do
              prod <- fromRight' <$> af P.reduce P.times
              incr <- fromRight' <$> caf P.atop 1 P.plus
              nums <- fromRight' <$> cff P.atop incr P.iota
              cff P.atop prod nums
            Right factA <- m P.factorial a
            cfam P.atDepth factImpl (scalar $ Number 0) a `shouldReturn` pure factA

          do
            Right encloseB <- m P.enclose b
            Right aPlusEncloseB <- d P.plus a encloseB
            cfad P.atDepth P.plus (vector [Number 1, Number 2]) a b `shouldReturn` pure aPlusEncloseB
            cfad P.atDepth P.plus (vector [Number 1, Number 3]) a b `shouldReturn` pure aPlusEncloseB
            cfad P.atDepth P.plus (vector [Number -1, Number 2]) a b `shouldReturn` pure aPlusEncloseB

          do
            Right reverseEach <- af P.each P.reverse
            Right reverseEachEach <- af P.each reverseEach
            Right reverseC <- m P.reverse c
            Right reverseEachC <- m reverseEach c
            Right reverseEachEachC <- m reverseEachEach c
            Right firstC <- m P.first c
            Right lastC <- m P.last c
            Right reverseFirstC <- m P.reverse firstC
            Right reverseEachLastC <- afm P.each P.reverse lastC
            cfam P.atDepth P.reverse (scalar $ Number -1) c `shouldReturn` pure reverseEachC
            cfam P.atDepth P.reverse (scalar $ Number -2) c `shouldReturn` pure reverseEachEachC
            cfam P.atDepth P.reverse (scalar $ Number -3) c `shouldReturn` pure c
            cfam P.atDepth P.reverse (scalar $ Number 0) c `shouldReturn` pure c
            cfam P.atDepth P.reverse (scalar $ Number 1) c `shouldReturn` pure (vector [box $ reverseFirstC, box $ reverseEachLastC])
            cfam P.atDepth P.reverse (scalar $ Number 2) c `shouldReturn` pure reverseEachC
            cfam P.atDepth P.reverse (scalar $ Number 3) c `shouldReturn` pure reverseC

          do
            Right catEach <- af P.each P.catenate
            Right encloseEachC <- afm P.each P.enclose c
            Right res <- afd P.each catEach b encloseEachC
            cfad P.atDepth P.catenate (vector [Number 0, Number -1]) b c `shouldReturn` pure res
            cfad P.atDepth P.catenate (vector [Number 0, Number 2]) b c `shouldReturn` pure res

          do
            cfam P.atDepth P.element (scalar $ Number -1) c `shouldReturn` pure (vector [box $ vector $ Character <$> "abc", box $ vector $ Character <$> "defghi"])
            cfam P.atDepth P.element (scalar $ Number -2) c `shouldReturn` pure (vector [box $ vector (box . vector . singleton . Character <$> "abc"), box $ vector [box $ vector $ Character <$> "def", box $ vector $ Character <$> "ghi"]])
            cfam P.atDepth P.element (scalar $ Number -3) c `shouldReturn` pure (vector [box $ vector (box . vector . singleton . Character <$> "abc"), box $ vector [box $ vector (box . vector . singleton . Character <$> "def"), box $ vector (box . vector . singleton . Character <$> "ghi")]])
            cfam P.atDepth P.element (scalar $ Number -4) c `shouldReturn` pure (vector [box $ vector (box . vector . singleton . Character <$> "abc"), box $ vector [box $ vector (box . vector . singleton . Character <$> "def"), box $ vector (box . vector . singleton . Character <$> "ghi")]])
            cfam P.atDepth P.element (scalar $ Number -0) c `shouldReturn` pure (vector [box $ vector (box . vector . singleton . Character <$> "abc"), box $ vector [box $ vector (box . vector . singleton . Character <$> "def"), box $ vector (box . vector . singleton . Character <$> "ghi")]])
            cfam P.atDepth P.element (scalar $ Number 1) c `shouldReturn` pure c
            cfam P.atDepth P.element (scalar $ Number 2) c `shouldReturn` pure (vector [box $ vector $ Character <$> "abc", box $ vector $ Character <$> "defghi"])
            cfam P.atDepth P.element (scalar $ Number 3) c `shouldReturn` pure (vector $ Character <$> "abcdefghi")
            cfam P.atDepth P.element (scalar $ Number 4) c `shouldReturn` pure (vector $ Character <$> "abcdefghi")
    
    describe [G.repeat] $ do
      describe "repeat" $ do
        it "repeats a function n times" $ do
          Right inc <- cfa P.atop P.plus (scalar $ Number 1)
          cfam P.repeat inc (scalar $ Number 3) (scalar $ Number 5) `shouldReturn` pure (scalar $ Number 8)
          cfad P.repeat P.plus (scalar $ Number 5) (scalar $ Number 2) (scalar $ Number 1) `shouldReturn` pure (scalar $ Number 11)
      describe "until" $ do
        it "applies a function until a condition is met" $ do
          Right step <- cff P.rightHook P.plus P.divide
          cffd P.repeat step P.identical (scalar $ Number 1) (scalar $ Number 1) `shouldReturn` pure (scalar $ Number 1.618033988749897)

    describe [G.valences] $ do
      describe "valences" $ do
        it "calls the left funtion when called monadically" $ do
          cffm P.valences P.minus P.plus (scalar $ Number 2) `shouldReturn` pure (scalar $ Number -2)
        it "calls the right function when called dyadically" $ do
          cffd P.valences P.minus P.plus (scalar $ Number 2) (scalar $ Number 1) `shouldReturn` pure (scalar $ Number 3)

    describe [G.under] $ do
      describe "under" $ do
        it "applies a function under another" $ do
          Right inc <- cfa P.atop P.plus (scalar $ Number 1)
          Right t3 <- caf P.atop (scalar $ Number 3) P.take
          cffm P.under inc t3 (vector [Number 3, Number 2, Number 9, Number 5, Number 6]) `shouldReturn` pure (vector [Number 4, Number 3, Number 10, Number 5, Number 6])
        it "substitutes elements of the argument" $ do
          Right t3 <- caf P.atop (scalar $ Number 3) P.take
          cafm P.under (vector [Number 10, Number 11, Number 12]) t3 (vector [Number 2, Number 4, Number 1, Number 5, Number 3]) `shouldReturn` pure (vector [Number 10, Number 11, Number 12, Number 5, Number 3])
          cafm P.under (scalar $ Number 7) t3 (vector [Number 2, Number 9, Number 3, Number 0, Number 1]) `shouldReturn` pure (vector [Number 7, Number 7, Number 7, Number 0, Number 1])
        it "works with sort up right argument" $ do
          Right op <- (fromRight' <$> cff P.atop P.iota P.notIdentical) >>= cff P.rightHook P.plus
          cffm P.under op P.precedesOrIdentical (vector [Number 9, Number 7, Number 1, Number 4, Number 10]) `shouldReturn` pure (vector [Number 12, Number 9, Number 1, Number 5, Number 14])

    describe [G.innerProduct] $ do
      describe "inner product" $ do
        it "computes the inner product of two arrays" $ do
          Right pr <- af P.reduce P.plus
          cffd P.innerProduct pr P.times (fromMajorCells [vector [Number 1, Number 2], vector [Number 3, Number 4]]) (fromMajorCells [vector [Number 5, Number 6], vector [Number 7, Number 8]]) `shouldReturn` pure (fromMajorCells [vector [Number 19, Number 22], vector [Number 43, Number 50]])
          
    describe [G.lev] $ do
      describe "lev" $ do
        it "returns the left operand function" $ do
          cffm P.lev P.minus P.times (scalar $ Number 3) `shouldReturn` pure (scalar $ Number -3)
          cffd P.lev P.minus P.times (scalar $ Number 3) (scalar $ Number 5) `shouldReturn` pure (scalar $ Number -2)
          cfam P.lev P.minus (scalar $ Character 'b') (scalar $ Number 5) `shouldReturn` pure (scalar $ Number -5)
        it "makes an array left operand into a constant function" $ do
          cafm P.lev (scalar $ Character 'a') P.times (scalar $ Number 3) `shouldReturn` pure (scalar $ Character 'a')
          cafd P.lev (scalar $ Character 'a') P.times (scalar $ Number 3) (scalar $ Number 5) `shouldReturn` pure (scalar $ Character 'a')
          caam P.lev (scalar $ Character 'a') (scalar $ Character 'b') (scalar $ Number 3) `shouldReturn` pure (scalar $ Character 'a')
          caad P.lev (scalar $ Character 'a') (scalar $ Character 'b') (scalar $ Number 3) (scalar $ Number 5) `shouldReturn` pure (scalar $ Character 'a')

    describe [G.dex] $ do
      describe "dex" $ do
        it "returns the right operand function" $ do
          cffm P.dex P.minus P.times (scalar $ Number 3) `shouldReturn` pure (scalar $ Number 1)
          cffd P.dex P.minus P.times (scalar $ Number 3) (scalar $ Number 5) `shouldReturn` pure (scalar $ Number 15)
          cafm P.dex (scalar $ Character 'a') P.times (scalar $ Number 5) `shouldReturn` pure (scalar $ Number 1)
        it "makes an array right operand into a constant function" $ do
          cfam P.dex P.minus (scalar $ Character 'b') (scalar $ Number 3) `shouldReturn` pure (scalar $ Character 'b')
          cfad P.dex P.minus (scalar $ Character 'b') (scalar $ Number 3) (scalar $ Number 5) `shouldReturn` pure (scalar $ Character 'b')
          caam P.dex (scalar $ Character 'a') (scalar $ Character 'b') (scalar $ Number 3) `shouldReturn` pure (scalar $ Character 'b')
          caad P.dex (scalar $ Character 'a') (scalar $ Character 'b') (scalar $ Number 3) (scalar $ Number 5) `shouldReturn` pure (scalar $ Character 'b')