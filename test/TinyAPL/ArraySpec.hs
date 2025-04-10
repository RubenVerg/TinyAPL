module TinyAPL.ArraySpec where

import TinyAPL.Noun
import TinyAPL.Tolerant
import TinyAPL.Error
import TinyAPL.Util (MonadShow(showM))

import Test.Hspec
import Data.List.NonEmpty
import TinyAPL.Complex
import Numeric.Natural
import Data.Functor.Identity (Identity(runIdentity))

instance Show ScalarValue where
  show = runIdentity . showM

instance Show Noun where
  show = runIdentity . showM

spec :: Spec
spec = do
  let vec = vector $ Number <$> [1, 2, 3]
  let matrix = Array [2, 2] $ Character <$> "abcd"
  let num = Number 5
  let char = Character 'x'
  let invalidValue = DomainError "Invalid value!"

  describe "box" $ do
    it "boxes an arbitrary array" $ do
      box vec `shouldBe` Box vec
      box matrix `shouldBe` Box matrix
    it "doesn't box a non-box scalar" $ do
      box (scalar num) `shouldBe` num
      box (scalar char) `shouldBe` char
    it "boxes an boxed scalar" $ do
      box (scalar $ box vec) `shouldBe` Box (scalar $ Box vec)
      box (scalar $ box matrix) `shouldBe` Box (scalar $ Box matrix)

  describe "scalar" $ do
    it "turns a ScalarValue into a scalar array" $ do
      scalar num `shouldBe` Array [] [num]
      scalar char `shouldBe` Array [] [char]
  
  describe "vector" $ do
    it "turns a list of ScalarValues into a vector" $ do
      vector [num, char] `shouldBe` Array [2] [num, char]
    it "turns an empty list into an empty vector" $ do
      vector [] `shouldBe` Array [0] []
  
  describe "arrayOf" $ do
    it "returns an array when the shape matches the content list" $ do
      arrayOf [2, 2] [num, num, char, char] `shouldBe` Just (Array [2, 2] [num, num, char, char])
    it "returns Nothing when the shape doesn't match the contents" $ do
      arrayOf [2, 2] [num, num, num] `shouldBe` Nothing

  describe "arrayReshaped" $ do
    it "returns an array from the contents when they match the shape" $ do
      arrayReshaped [2, 2] [num, num, num, num] `shouldBe` Just (Array [2, 2] [num, num, num, num])
    it "returns an array with the contents replicated when they don't match the shape" $ do
      arrayReshaped [2, 2] [num, char] `shouldBe` Just (Array [2, 2] [num, char, num, char])
    it "returns an array with the first elements when there are too many" $ do
      arrayReshaped [1] [num, char] `shouldBe` Just (Array [1] [num])
    it "returns an empty array with an empty shape" $ do
      arrayReshaped [0, 2] [num, char] `shouldBe` Just (Array [0, 2] [])
      arrayReshaped [0, 1] [] `shouldBe` Just (Array [0, 1] [])
    it "returns a Nothing when the shape isn't empty but the contents are" $ do
      arrayReshaped [1, 2] [] `shouldBe` Nothing

  describe "arrayReshapedNE" $ do
    it "behaves like arrayReshaped" $ do
      Just (arrayReshapedNE [1, 2] (num :| [char])) `shouldBe` arrayReshaped [1, 2] [num, char]

  describe "majorCells" $ do
    it "returns major cells of rank >0 arrays" $ do
      majorCells matrix `shouldBe` [vector $ Character <$> "ab", vector $ Character <$> "cd"]
      majorCells vec `shouldBe` scalar . Number <$> [1, 2, 3]
    it "returns the scalar for scalar arrays" $ do
      majorCells (scalar char) `shouldBe` [scalar char]

  describe "fromMajorCells" $ do
    it "returns an empty vector when the cells are empty" $ do
      fromMajorCells [] `shouldBe` vector []
    it "is an inverse to majorCells" $ do
      fromMajorCells (majorCells matrix) `shouldBe` matrix
      fromMajorCells (majorCells vec) `shouldBe` vec

  describe "realEqual and complexEqual" $ do
    it "returns False for different numbers" $ do
      realEqual 0 1 `shouldBe` False
      complexEqual (0 :+ 2) ((-1) :+ 3) `shouldBe` False
    it "returns True for same numbers" $ do
      realEqual 0 0 `shouldBe` True
      complexEqual (0 :+ 3) (0 :+ 3) `shouldBe` True
    it "returns True for numbers within the comparison tolerance" $ do
      realEqual 1 (1 + 5e-15) `shouldBe` True
      complexEqual 1 (1 :+ 3e-16) `shouldBe` True
  
  describe "isReal" $ do
    it "returns True for numbers with negligible imaginary part" $ do
      isReal 0 `shouldBe` True
      isReal 1e-15 `shouldBe` True
    it "returns False for complex numbers" $ do
      isReal (0 :+ 1) `shouldBe` False

  describe "Eq ScalarValue" $ do
    it "calls equal values equal" $ do
      char == char `shouldBe` True
      num == num `shouldBe` True
      box matrix == box matrix `shouldBe` True
    it "calls different values not equal" $ do
      Number 1 == Number 2 `shouldBe` False
      Character 'x' == Character 'y' `shouldBe` False
      box matrix == box vec `shouldBe` False
    it "calls values of different tyeps not equal" $ do
      Number 1 == Character '1' `shouldBe` False

  describe "Ord ScalarValue" $ do
    it "compares numbers lexicographically" $ do
      Number 1 < Number 2 `shouldBe` True
      Number (1 :+ 1) < Number (1 :+ 2) `shouldBe` True
    it "compares characters by codepoint" $ do
      Character 'x' < Character 'y' `shouldBe` True
    it "compares boxes by their contents" $ do
      box vec < box matrix `shouldBe` vec < matrix
    it "orders values as Number < Character < Box" $ do
      num < char `shouldBe` True
      num < box vec `shouldBe` True
      char < box vec `shouldBe` True

  describe "Eq Array" $ do
    it "calls equivalent arrays equal" $ do
      vec == vec `shouldBe` True
      matrix == matrix `shouldBe` True
    it "calls arrays with different shape not equal" $ do
      Array [2, 2] (Number <$> [1, 2, 3, 4]) == Array [4] (Number <$> [1, 2, 3, 4]) `shouldBe` False
    it "calls arrays with different contents not equal" $ do
      vector [num] == vector [char] `shouldBe` False
    
  describe "Ord Array" $ do
    it "orders arrays" $ do
      scalar (Character 'a') `compare` scalar (Character 'b') `shouldBe` LT
      vector (Character <$> "abc") `compare` vector (Character <$> "abc") `shouldBe` EQ
      vector (Character <$> "ABC") `compare` vector (Character <$> "abc") `shouldBe` LT
      vector (Character <$> "abc ") `compare` vector (Character <$> "xyz") `shouldBe` LT
      vector (Character <$> "abc ") `compare` vector (Character <$> "abc") `shouldBe` GT
      vector (Character <$> "abc\x00") `compare` vector (Character <$> "abc") `shouldBe` GT
      vector (Character <$> "abc") `compare` scalar (Character 'z') `shouldBe` LT
      Array [1, 3] (Character <$> "abc") `compare` vector (Character <$> "xyz") `shouldBe` LT

      scalar (Number 3) `compare` vector [Number 3] `shouldBe` LT
      vector (Character <$> "abc") `compare` Array [1, 3] (Character <$> "abc") `shouldBe` LT
      scalar (box $ vector $ Character <$> "ab") `compare` Array [1, 1, 1] [box $ vector $ Character <$> "ab"] `shouldBe` LT
  
  describe "isInt" $ do
    it "returns True for integers" $ do
      isInt 0 `shouldBe` True
      isInt (1 + 1e-15) `shouldBe` True
    it "returns False for non-integers" $ do
      isInt 2.5 `shouldBe` False

  describe "boolToScalar" $ do
    it "returns 1 for True" $ do
      boolToScalar True `shouldBe` Number 1
    it "returns 0 for False" $ do
      boolToScalar False `shouldBe` Number 0

  describe "asBool" $ do
    it "returns True for 1" $ do
      asBool invalidValue (Number 1) `shouldBe` (pure True :: Result Bool)
    it "returns False for 0" $ do
      asBool invalidValue (Number 0) `shouldBe` (pure False :: Result Bool)
    it "errors for other values" $ do
      asBool invalidValue (Number 5) `shouldBe` (throwError invalidValue :: Result Bool)
      asBool invalidValue (Character 'b') `shouldBe` (throwError invalidValue :: Result Bool)

  describe "asNumber" $ do
    it "returns numbers unwrapped" $ do
      asNumber invalidValue (Number (2 :+ 3.5)) `shouldBe` (pure (2 :+ 3.5) :: Result (Complex Double))
    it "errors for other values" $ do
      asNumber invalidValue (Character 'c') `shouldBe` (throwError invalidValue :: Result (Complex Double))

  describe "asReal" $ do
    it "returns real numbers unwrapped" $ do
      asReal invalidValue (3 :+ 0) `shouldBe` (pure 3 :: Result Double)
    it "errors for complex numbers" $ do
      asReal invalidValue (3 :+ 1) `shouldBe` (throwError invalidValue :: Result Double)

  describe "asInt and asInt'" $ do
    it "returns integers unwrapped" $ do
      asInt' invalidValue 3 `shouldBe` (pure 3 :: Result Integer)
    it "errors for non-integers" $ do
      asInt' invalidValue 2.5 `shouldBe` (throwError invalidValue :: Result Integer)
      asInt invalidValue (3 :+ 2) `shouldBe` (throwError invalidValue :: Result Integer)
  
  describe "asNat and asNat'" $ do
    it "returns naturals unwrapped" $ do
      asNat' invalidValue 5 `shouldBe` (pure 5 :: Result Natural)
    it "errors for non-naturals" $ do
      asNat' invalidValue (-3) `shouldBe` (throwError invalidValue :: Result Natural)
      asNat invalidValue (3 :+ 1) `shouldBe` (throwError invalidValue :: Result Natural)
    
  describe "isScalar" $ do
    it "returns True for scalar arrays" $ do
      isScalar (scalar num) `shouldBe` True
      isScalar (scalar $ box vec) `shouldBe` True
    it "returns False for non-scalar arrays" $ do
      isScalar vec `shouldBe` False

  describe "isEmpty" $ do
    it "returns True for empty arrays" $ do
      isEmpty (vector []) `shouldBe` True
    it "returns False for non-empty arrays" $ do
      isEmpty vec `shouldBe` False

  describe "asVector" $ do
    it "returns the entries of a vector" $ do
      asVector invalidValue (vector [num, char]) `shouldBe` (pure [num, char] :: Result [ScalarValue])
    it "returns a scalar wrapped in a singleton list" $ do
      asVector invalidValue (scalar num) `shouldBe` (pure [num] :: Result [ScalarValue])
    it "errors with higher-rank arguments" $ do
      asVector invalidValue matrix `shouldBe` (throwError invalidValue :: Result [ScalarValue])


