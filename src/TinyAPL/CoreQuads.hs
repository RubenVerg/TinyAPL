{-# LANGUAGE LambdaCase, CPP #-}
module TinyAPL.CoreQuads where

import TinyAPL.Noun
import TinyAPL.Function
import TinyAPL.Adverb
import TinyAPL.Context
import TinyAPL.Tolerant
import TinyAPL.Quads
import TinyAPL.Complex
import TinyAPL.CoreQuads.Inspect
import TinyAPL.CoreQuads.Math
import TinyAPL.CoreQuads.Regex
import TinyAPL.CoreQuads.Unicode
import TinyAPL.Error
import qualified TinyAPL.Glyphs as G
import TinyAPL.Interpreter
import TinyAPL.Random
import TinyAPL.StandardLibrary
import TinyAPL.Util

import Control.Monad.State
import Data.Time
import Data.Time.Clock.POSIX
import Control.Concurrent
import Data.List
import Data.List.Split (splitOn)

io = Nilad (Just $ pure $ scalar $ Number 0) Nothing (G.quad : "io") Nothing
ct = Nilad (Just $ pure $ scalar $ Number $ comparisonTolerance :+ 0) Nothing (G.quad : "ct") Nothing
u = Nilad (Just $ pure $ vector $ Character <$> ['A'..'Z']) Nothing (G.quad : "u") Nothing
l = Nilad (Just $ pure $ vector $ Character <$> ['a'..'z']) Nothing (G.quad : "l") Nothing
d = Nilad (Just $ pure $ vector $ Character <$> ['0'..'9']) Nothing (G.quad : "d") Nothing
seed = Nilad Nothing (Just $ \x -> do
  let e = DomainError "Seed must be a scalar integer"
  s <- asScalar e x >>= asNumber e >>= asInt e
  setSeed s) (G.quad : "seed") Nothing
unix = Nilad (Just $ scalar . Number . realToFrac <$> liftToSt getPOSIXTime) Nothing (G.quad : "unix") Nothing
ts = Nilad (Just $ do
  utc <- liftToSt $ posixSecondsToUTCTime <$> getPOSIXTime
  let (YearMonthDay y m d) = utctDay utc
  let (TimeOfDay h min s') = timeToTimeOfDay $ utctDayTime utc
  let s = floor $ fixedToFractional s'
  let ms = round $ fracPart (fixedToFractional s') * 1000
  pure $ vector [Number $ fromIntegral y, Number $ fromIntegral m, Number $ fromIntegral d, Number $ fromIntegral h, Number $ fromIntegral min, Number $ fromIntegral s, Number $ fromIntegral ms]) Nothing (G.quad : "ts") Nothing

exists = PrimitiveFunction (Just $ \_ y -> do
  var <- asString (DomainError "Exists argument must be a string") y
  v <- gets contextScope >>= readRef >>= scopeLookup True var
  case v of
    Just _ -> return $ scalar $ Number 1
    Nothing -> return $ scalar $ Number 0
  ) (Just $ \_ x y -> do
  var <- asString (DomainError "Exists right argument must be a string") y
  let err = DomainError "Exists left argument must be a scalar struct"
  ns <- asScalar err x >>= asStruct err >>= readRef . contextScope
  case scopeShallowLookup False var ns of
    Just _ -> pure $ scalar $ Number 1
    Nothing -> pure $ scalar $ Number 0) Nothing Nothing Nothing Nothing Nothing Nothing (G.quad : "Exists") Nothing
repr = PrimitiveFunction (Just $ \_ y -> vector . fmap Character <$> showM (Repr y)) Nothing Nothing Nothing Nothing Nothing Nothing Nothing (G.quad : "Repr") Nothing
delay = PrimitiveFunction (Just $ \_ y -> do
  let err = DomainError "Delay argument must be a nonnegative scalar number"
  n <- asScalar err y >>= asNumber err >>= asReal err
  if n < 0 then throwError err else do
    start <- realToFrac <$> liftToSt getPOSIXTime
    liftToSt $ threadDelay $ floor $ n * 1000 * 1000
    end <- realToFrac <$> liftToSt getPOSIXTime
    pure $ scalar $ Number $ (end - start) :+ 0
  ) Nothing Nothing Nothing Nothing Nothing Nothing Nothing (G.quad : "Delay") Nothing
type_ = PrimitiveFunction (Just $ \_ (Array sh cs) -> return $ Array sh $ (\case
  Number _ -> Number 0
  Character _ -> Number 1
  Box _ -> Number 2
  Wrap _ -> Number 3
  AdverbWrap _ -> Number 4
  ConjunctionWrap _ -> Number 5
  Struct _ -> Number 6) <$> cs) Nothing Nothing Nothing Nothing Nothing Nothing Nothing (G.quad : "Type") Nothing
print_ = PrimitiveFunction (Just $ \_ y -> do
  let err = DomainError "Print argument must be a string or vector of strings"
  ss <- asStrings err y
  out <- getsContext contextOut
  out $ intercalate "\n" ss ++ "\n"
  pure $ vector []
  ) Nothing Nothing Nothing Nothing Nothing Nothing Nothing (G.quad : "P") Nothing
errorPrint = PrimitiveFunction (Just $ \_ y -> do
  let err = DomainError "Print argument must be a string or vector of strings"
  ss <- asStrings err y
  err <- getsContext contextErr
  err $ intercalate "\n" ss ++ "\n"
  pure $ vector []
  ) Nothing Nothing Nothing Nothing Nothing Nothing Nothing (G.quad : "E") Nothing
measure = PrimitiveAdverb Nothing (Just $ \_ f -> pure $ DerivedFunctionFunction (Just $ \ea y -> do
  start <- realToFrac <$> liftToSt getPOSIXTime
  _ <- callMonad f ea y
  end <- realToFrac <$> liftToSt getPOSIXTime
  pure $ scalar $ Number $ (end - start) :+ 0) (Just $ \ea x y -> do
  start <- realToFrac <$> liftToSt getPOSIXTime
  _ <- callDyad f ea x y
  end <- realToFrac <$> liftToSt getPOSIXTime
  pure $ scalar $ Number $ (end - start) :+ 0) Nothing Nothing Nothing Nothing Nothing Nothing Nothing measure f) (G.quad : "_Measure") Nothing

core = quadsFromReprs [ io, ct, u, l, d, seed, unix, ts, math, regex, inspectNamespace ] [ exists, repr, delay, type_, unicode, print_, errorPrint, inspectF ] [ measure ] []

makeImport :: (FilePath -> St String) -> Maybe ([String] -> St String) -> Function
makeImport read readStd = PrimitiveFunction (Just $ \_ x -> do
  let err = DomainError "Import argument must be a character vector"
  path <- asVector err x >>= mapM (asCharacter err)
  ctx <- getContext
  scope <- createRef $ Scope [] [] [] [] Nothing -- The scope has intentionally no parent; imports run in an isolated context
  let ctx' = ctx{ contextScope = scope }
  source <-
    if isPrefixOf "std:" path
    then case readStd of
      Just fn -> fn <$> splitOn "/" $ drop (length "std:") path
      Nothing -> case lookup (splitOn "/" $ drop (length "std:") path) standardLibrary of
        Just source -> pure source
        Nothing -> throwError $ DomainError $ "Standard library module " ++ path ++ " not found"
    else read path
  runWithContext ctx' $ run' path source
  pure $ scalar $ Struct ctx') Nothing Nothing Nothing Nothing Nothing Nothing Nothing (G.quad : "Import") Nothing

bigEndian :: Bool
#ifdef ARCH_IS_BIG_ENDIAN
bigEndian = True
#else
bigEndian = False
#endif

makeSystemInfo :: String -> String -> Bool -> Bool -> Nilad
makeSystemInfo os arch js bigEndian = Nilad (Just $ do
  scope <- createRef $ Scope [ ("os", (VariableConstant, vector $ Character <$> os))
                             , ("arch", (VariableConstant, vector $ Character <$> arch))
                             , ("js", (VariableConstant, scalar $ boolToScalar js))
                             , ("bigEndian", (VariableConstant, scalar $ boolToScalar bigEndian)) ] [] [] [] Nothing
  ctx <- get
  pure $ scalar $ Struct $ ctx{ contextScope = scope } ) Nothing (G.quad : "systemInfo") Nothing
