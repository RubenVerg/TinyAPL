{-# LANGUAGE FlexibleContexts #-}

module TinyAPL.CoreExtraArgs
  ( CoreExtraArgs(..)
  , coreExtraArgsToleranceKey
  , coreExtraArgsOriginKey
  , coreExtraArgsFillKey
  , coreExtraArgsBackwardKey
  , defaultCoreExtraArgs
  , parseCoreExtraArgs
  , reprCoreExtraArgs ) where

import TinyAPL.Complex
import TinyAPL.Error
import TinyAPL.Function
import TinyAPL.Noun
import TinyAPL.Tolerant

import Numeric.Natural
import Control.Monad.Except
import Data.Maybe
import Control.Monad

coreExtraArgsToleranceKey :: String
coreExtraArgsToleranceKey = "tolerance"

coreExtraArgsOriginKey :: String
coreExtraArgsOriginKey = "origin"

coreExtraArgsFillKey :: String
coreExtraArgsFillKey = "fill"

coreExtraArgsBackwardKey :: String
coreExtraArgsBackwardKey = "backward"

data CoreExtraArgs = CoreExtraArgs
  { coreExtraArgsTolerance :: Double
  , coreExtraArgsOrigin :: Natural
  , coreExtraArgsFill :: Maybe ScalarValue
  , coreExtraArgsBackward :: Bool }

defaultCoreExtraArgs :: CoreExtraArgs
defaultCoreExtraArgs = CoreExtraArgs
  { coreExtraArgsTolerance = comparisonTolerance
  , coreExtraArgsOrigin = 0
  , coreExtraArgsFill = Nothing
  , coreExtraArgsBackward = False }

parseCoreExtraArgs :: MonadError Error m => ExtraArgs -> m CoreExtraArgs
parseCoreExtraArgs ea = do
  let 
    lookupStr :: String -> Maybe ScalarValue
    lookupStr s = lookup (box $ vector $ Character <$> s) ea
  let toleranceErr = DomainError "Tolerance must be a scalar real number"
  tolerance <- fromMaybe (coreExtraArgsTolerance defaultCoreExtraArgs) <$> (mapM (asNumber toleranceErr >=> asReal toleranceErr) $ lookupStr coreExtraArgsToleranceKey)
  let originErr = DomainError "Origin must be a scalar integer"
  origin <- fromMaybe (coreExtraArgsOrigin defaultCoreExtraArgs) <$> (mapM (asNumber originErr >=> asInt originErr) $ lookupStr coreExtraArgsOriginKey)
  let fill = lookupStr coreExtraArgsFillKey
  let backwardErr = DomainError "Backward must be a scalar boolean"
  backward <- fromMaybe (coreExtraArgsBackward defaultCoreExtraArgs) <$> (mapM (asBool backwardErr) $ lookupStr coreExtraArgsBackwardKey)
  pure CoreExtraArgs
    { coreExtraArgsTolerance = tolerance
    , coreExtraArgsOrigin = origin
    , coreExtraArgsFill = fill
    , coreExtraArgsBackward = backward }

reprCoreExtraArgs :: CoreExtraArgs -> ExtraArgs
reprCoreExtraArgs CoreExtraArgs{ coreExtraArgsTolerance = tolerance, coreExtraArgsOrigin = origin, coreExtraArgsFill = fill, coreExtraArgsBackward = backward } =
  let key = box . vector . fmap Character
  in [ (key coreExtraArgsToleranceKey, Number $ tolerance :+ 0)
     , (key coreExtraArgsOriginKey, Number $ (fromIntegral origin) :+ 0)
     , (key coreExtraArgsBackwardKey, boolToScalar backward) ] ++ case fill of
        Nothing -> []
        Just f -> [(key coreExtraArgsFillKey, f)]