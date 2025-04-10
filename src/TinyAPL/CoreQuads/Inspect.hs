{-# LANGUAGE FlexibleContexts #-}

module TinyAPL.CoreQuads.Inspect where  

import TinyAPL.Adverb
import TinyAPL.Conjunction
import TinyAPL.Context
import TinyAPL.Noun
import TinyAPL.Error
import TinyAPL.Function
import TinyAPL.Quads
import TinyAPL.Value
import qualified TinyAPL.Glyphs as G

import Control.Monad.Error.Class (MonadError)
import Data.Maybe

inspectKeyNoun :: ScalarValue
inspectKeyNoun = Character 'n'

inspectKeyDefined :: ScalarValue
inspectKeyDefined = Character 'd'

inspectKeyPrimitive :: ScalarValue
inspectKeyPrimitive = Character 'p'

inspectKeyDerived :: ScalarValue
inspectKeyDerived = Character 'r'

inspectKeyUnwrapArrayFunction :: ScalarValue
inspectKeyUnwrapArrayFunction = Character 'U'

inspectKeyTrain :: ScalarValue
inspectKeyTrain = Character 't'

inspectKeyExtraArgs :: ScalarValue
inspectKeyExtraArgs = Character 'A'

inspectValue :: Bool -> Value -> Noun
inspectValue _ (VNoun n) = inspectNoun n
inspectValue deep (VFunction f) = inspectFunction deep f
inspectValue deep (VAdverb adv) = inspectAdverb deep adv
inspectValue deep (VConjunction conj) = inspectConjunction deep conj

inspectMaybeValue :: Bool -> Maybe Value -> Noun
inspectMaybeValue _ Nothing = vector []
inspectMaybeValue deep (Just v) = inspectValue deep v

boxAValue :: Value -> ScalarValue
boxAValue (VNoun n) = box n
boxAValue (VFunction f) = Wrap f
boxAValue (VAdverb adv) = AdverbWrap adv
boxAValue (VConjunction conj) = ConjunctionWrap conj

inspectNoun :: Noun -> Noun
inspectNoun n = vector [inspectKeyNoun, box n]

inspectFunction :: Bool -> Function -> Noun
inspectFunction _ f@DefinedFunction{} = vector [inspectKeyDefined, Wrap f]
inspectFunction _ f@PrimitiveFunction{} = vector [inspectKeyPrimitive, Wrap f]
inspectFunction _ PartialFunction{} = vector []
inspectFunction deep DerivedFunctionNoun{ derivedFunctionAdverb = adv, derivedFunctionNounLeft = u } =
  vector [inspectKeyDerived, if deep then box $ inspectAdverb deep adv else AdverbWrap adv, if deep then box $ inspectNoun u else box u]
inspectFunction deep DerivedFunctionFunction{ derivedFunctionAdverb = adv, derivedFunctionFunctionLeft = u } =
  vector [inspectKeyDerived, if deep then box $ inspectAdverb deep adv else AdverbWrap adv, if deep then box $ inspectFunction deep u else Wrap u]
inspectFunction deep DerivedFunctionNounNoun{ derivedFunctionConjunction = conj, derivedFunctionNounLeft = u, derivedFunctionNounRight = v } =
  vector [inspectKeyDerived, if deep then box $ inspectConjunction deep conj else ConjunctionWrap conj, if deep then box $ inspectNoun u else box u, if deep then box $ inspectNoun v else box v]
inspectFunction deep DerivedFunctionNounFunction{ derivedFunctionConjunction = conj, derivedFunctionNounLeft = u, derivedFunctionFunctionRight = v } =
  vector [inspectKeyDerived, if deep then box $ inspectConjunction deep conj else ConjunctionWrap conj, if deep then box $ inspectNoun u else box u, if deep then box $ inspectFunction deep v else Wrap v]
inspectFunction deep DerivedFunctionFunctionNoun{ derivedFunctionConjunction = conj, derivedFunctionFunctionLeft = u, derivedFunctionNounRight = v } =
  vector [inspectKeyDerived, if deep then box $ inspectConjunction deep conj else ConjunctionWrap conj, if deep then box $ inspectFunction deep u else Wrap u, if deep then box $ inspectNoun v else box v]
inspectFunction deep DerivedFunctionFunctionFunction{ derivedFunctionConjunction = conj, derivedFunctionFunctionLeft = u, derivedFunctionFunctionRight = v } =
  vector [inspectKeyDerived, if deep then box $ inspectConjunction deep conj else ConjunctionWrap conj, if deep then box $ inspectFunction deep u else Wrap u, if deep then box $ inspectFunction deep v else Wrap v]
inspectFunction _ UnwrapArrayFunction{ unwrapFunctionArray = arr } = vector [inspectKeyUnwrapArrayFunction, box arr]
inspectFunction True TrainFunction{ trainFunctionTines = tines } = vector $ inspectKeyTrain : (box . inspectMaybeValue True <$> tines)
inspectFunction False TrainFunction{ trainFunctionTines = tines } = vector $ inspectKeyTrain : (boxAValue . fromMaybe (VNoun $ vector []) <$> tines)
inspectFunction deep ExtraArgsFunction{ extraArgsFunctionExtraArgs = args, extraArgsFunctionFunction = fn } =
  vector [inspectKeyExtraArgs, if deep then box $ inspectFunction deep fn else Wrap fn, box $ dictionary args]

inspectAdverb :: Bool -> Adverb -> Noun
inspectAdverb _ adv@DefinedAdverb{} = vector [inspectKeyDefined, AdverbWrap adv]
inspectAdverb _ adv@PrimitiveAdverb{} = vector [inspectKeyPrimitive, AdverbWrap adv]
inspectAdverb _ PartialAdverb{} = vector []
inspectAdverb True TrainAdverb{ trainAdverbTines = tines } = vector $ inspectKeyTrain : (box . inspectMaybeValue True <$> tines)
inspectAdverb False TrainAdverb{ trainAdverbTines = tines } = vector $ inspectKeyTrain : (boxAValue . fromMaybe (VNoun $ vector []) <$> tines)
inspectAdverb deep ExtraArgsAdverb{ extraArgsAdverbExtraArgs = args, extraArgsAdverbAdverb = adv } =
  vector [inspectKeyExtraArgs, if deep then box $ inspectAdverb deep adv else AdverbWrap adv, box $ dictionary args]

inspectConjunction :: Bool -> Conjunction -> Noun
inspectConjunction _ conj@DefinedConjunction{} = vector [inspectKeyDefined, ConjunctionWrap conj]
inspectConjunction _ conj@PrimitiveConjunction{} = vector [inspectKeyPrimitive, ConjunctionWrap conj]
inspectConjunction True TrainConjunction{ trainConjunctionTines = tines } = vector $ inspectKeyTrain : (box . inspectMaybeValue True <$> tines)
inspectConjunction False TrainConjunction{ trainConjunctionTines = tines } = vector $ inspectKeyTrain : (boxAValue . fromMaybe (VNoun $ vector []) <$> tines)
inspectConjunction deep ExtraArgsConjunction{ extraArgsConjunctionExtraArgs = args, extraArgsConjunctionConjunction = conj } =
  vector [inspectKeyExtraArgs, if deep then box $ inspectConjunction deep conj else ConjunctionWrap conj, box $ dictionary args]

inspectNamespace :: Nilad
inspectNamespace = Nilad (Just $ do
  scope <- createRef $ Scope
    [ ("noun", (VariableConstant, scalar inspectKeyNoun))
    , ("defined", (VariableConstant, scalar inspectKeyDefined))
    , ("primitive", (VariableConstant, scalar inspectKeyPrimitive))
    , ("derived", (VariableConstant, scalar inspectKeyDerived))
    , ("unwrapArray", (VariableConstant, scalar inspectKeyUnwrapArrayFunction))
    , ("train", (VariableConstant, scalar inspectKeyTrain))
    , ("extraArgs", (VariableConstant, scalar inspectKeyExtraArgs)) ]
    [] [] [] Nothing
  ctx <- getContext
  pure $ scalar $ Struct $ ctx{ contextScope = scope }) Nothing (G.quad : "inspect") Nothing

inspect :: Bool -> Noun -> Noun
inspect deep (Array [] [Wrap f]) = inspectFunction deep f
inspect deep (Array [] [AdverbWrap adv]) = inspectAdverb deep adv
inspect deep (Array [] [ConjunctionWrap conj]) = inspectConjunction deep conj
inspect _ x = inspectNoun x

inspect' :: MonadError Error m => Noun -> Noun -> m Noun
inspect' deep x = do
  let err = DomainError "Inspect left argument must be a "
  deep' <- asScalar err deep >>= asBool err
  pure $ inspect deep' x

inspectMonad :: MonadError Error m => Noun -> m Noun
inspectMonad = inspect' $ scalar $ boolToScalar True

inspectF :: Function
inspectF = PrimitiveFunction (Just $ const inspectMonad) (Just $ const inspect') Nothing Nothing Nothing Nothing Nothing Nothing (G.quad : "Inspect") Nothing
