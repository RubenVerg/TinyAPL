{-# LANGUAGE FlexibleContexts #-}

module TinyAPL.CoreQuads.Math (math) where

import TinyAPL.ArrayFunctionOperator
import TinyAPL.Error
import qualified TinyAPL.Glyphs as G
import Control.Monad.Error.Class (MonadError)

piA :: Noun
piA = scalar $ Number $ pi

complementaryS :: MonadError Error m => ScalarValue -> m ScalarValue
complementaryS (Number y) = pure $ Number $ sqrt $ 1 - y * y
complementaryS _ = throwError $ DomainError "Complementary argument must be a number"

complementaryF :: Function
complementaryF = PrimitiveFunction (Just $ const $ scalarMonad complementaryS) Nothing "Complementary" Nothing

sinS :: MonadError Error m => ScalarValue -> m ScalarValue
sinS (Number y) = pure $ Number $ sin y
sinS _ = throwError $ DomainError "Sine argument must be a number"

sinF :: Function
sinF = PrimitiveFunction (Just $ const $ scalarMonad sinS) Nothing "Sin" Nothing

arcsinS :: MonadError Error m => ScalarValue -> m ScalarValue
arcsinS (Number y) = pure $ Number $ asin y
arcsinS _ = throwError $ DomainError "Arcsine argument must be a number"

arcsinF :: Function
arcsinF = PrimitiveFunction (Just $ const $ scalarMonad arcsinS) Nothing "Arcsin" Nothing

cosS :: MonadError Error m => ScalarValue -> m ScalarValue
cosS (Number y) = pure $ Number $ cos y
cosS _ = throwError $ DomainError "Cosine argument must be a number"

cosF :: Function
cosF = PrimitiveFunction (Just $ const $ scalarMonad cosS) Nothing "Cos" Nothing

arccosS :: MonadError Error m => ScalarValue -> m ScalarValue 
arccosS (Number y) = pure $ Number $ acos y
arccosS _ = throwError $ DomainError "Arccosine argument must be a number"

arccosF :: Function
arccosF = PrimitiveFunction (Just $ const $ scalarMonad arccosS) Nothing "Arccos" Nothing

tanS :: MonadError Error m => ScalarValue -> m ScalarValue  
tanS (Number y) = pure $ Number $ tan y
tanS _ = throwError $ DomainError "Tangent argument must be a number"

tanF :: Function
tanF = PrimitiveFunction (Just $ const $ scalarMonad tanS) Nothing "Tan" Nothing

arctanS :: MonadError Error m => ScalarValue -> m ScalarValue  
arctanS (Number y) = pure $ Number $ atan y
arctanS _ = throwError $ DomainError "Arctangent argument must be a number"

arctanF :: Function
arctanF = PrimitiveFunction (Just $ const $ scalarMonad arctanS) Nothing "Arctan" Nothing

sinhS :: MonadError Error m => ScalarValue -> m ScalarValue  
sinhS (Number y) = pure $ Number $ sinh y
sinhS _ = throwError $ DomainError "Hyperbolic sine argument must be a number"

sinhF :: Function
sinhF = PrimitiveFunction (Just $ const $ scalarMonad sinhS) Nothing "Sinh" Nothing

arsinhS :: MonadError Error m => ScalarValue -> m ScalarValue  
arsinhS (Number y) = pure $ Number $ asinh y
arsinhS _ = throwError $ DomainError "Hyperbolic arsine argument must be a number"

arsinhF :: Function
arsinhF = PrimitiveFunction (Just $ const $ scalarMonad arsinhS) Nothing "Arsinh" Nothing

coshS :: MonadError Error m => ScalarValue -> m ScalarValue  
coshS (Number y) = pure $ Number $ cosh y
coshS _ = throwError $ DomainError "Hyperbolic cosine argument must be a number"

coshF :: Function
coshF = PrimitiveFunction (Just $ const $ scalarMonad coshS) Nothing "Cosh" Nothing

arcoshS :: MonadError Error m => ScalarValue -> m ScalarValue  
arcoshS (Number y) = pure $ Number $ acosh y
arcoshS _ = throwError $ DomainError "Hyperbolic arcosine argument must be a number"

arcoshF :: Function
arcoshF = PrimitiveFunction (Just $ const $ scalarMonad arcoshS) Nothing "Arcosh" Nothing

tanhS :: MonadError Error m => ScalarValue -> m ScalarValue  
tanhS (Number y) = pure $ Number $ tanh y
tanhS _ = throwError $ DomainError "Hyperbolic tangent argument must be a number"

tanhF :: Function
tanhF = PrimitiveFunction (Just $ const $ scalarMonad tanhS) Nothing "Tanh" Nothing

artanhS :: MonadError Error m => ScalarValue -> m ScalarValue  
artanhS (Number y) = pure $ Number $ atanh y
artanhS _ = throwError $ DomainError "Hyperbolic artangent argument must be a number"

artanhF :: Function
artanhF = PrimitiveFunction (Just $ const $ scalarMonad artanhS) Nothing "Artanh" Nothing

math = Nilad (Just $ do
  scope <- createRef (Scope [("pi", (VariableConstant, piA))] ((\n -> (functionRepr n, (VariableConstant, n))) <$>
    [ complementaryF
    , sinF
    , arcsinF
    , cosF
    , arccosF
    , tanF
    , arctanF
    , sinhF
    , arsinhF
    , coshF
    , arcoshF
    , tanhF
    , artanhF
    ]) [] [] Nothing)
  ctx <- getContext
  pure $ scalar $ Struct $ ctx{ contextScope = scope } ) Nothing (G.quad : "math") Nothing