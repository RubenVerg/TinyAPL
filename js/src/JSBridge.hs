{-# LANGUAGE FlexibleInstances #-}

module JSBridge (IsJS(..), IsJSSt(..), JSArray(..), jsToString, jsNil, (++#), jsHead, jsTail, jsLength, jsUndefined, valToObject, objectToVal) where

import TinyAPL.Noun
import TinyAPL.Function
import TinyAPL.Adverb
import TinyAPL.Conjunction
import TinyAPL.Context
import TinyAPL.Value
import TinyAPL.Quads
import TinyAPL.Error
import TinyAPL.Interpreter
import TinyAPL.Util

import GHC.Wasm.Prim
import Numeric.Natural
import TinyAPL.Complex
import Data.List
import Data.Bifunctor
import Data.Foldable
import Control.Monad (void)

class IsJS a where
  fromJSVal :: JSVal -> a
  toJSVal :: a -> JSVal

class IsJSSt a where
  fromJSValSt :: JSVal -> St a
  toJSValSt :: a -> St JSVal

instance IsJS JSVal where
  fromJSVal = id
  toJSVal = id

instance IsJSSt JSVal where
  fromJSValSt = pure
  toJSValSt = pure

instance IsJS () where
  fromJSVal = const ()
  toJSVal = const jsUndefined

foreign import javascript unsafe "return $1;" valToString :: JSVal -> JSString
foreign import javascript unsafe "return $1;" stringToVal :: JSString -> JSVal

instance IsJS JSString where
  fromJSVal = valToString
  toJSVal = stringToVal

instance IsJS Char where
  fromJSVal v = case fromJSString $ fromJSVal v of
    [c] -> c
    _ -> error "fromJSVal Char: not a character"
  toJSVal = toJSVal . toJSString . singleton

newtype JSArray = JSArray { unJSArray :: JSVal }

instance IsJS JSArray where
  fromJSVal = JSArray
  toJSVal = unJSArray

foreign import javascript unsafe "return [];" jsNil_ :: Bool -> JSArray
foreign import javascript unsafe "return [$1, ...$2];" jsCons :: JSVal -> JSArray -> JSArray
foreign import javascript unsafe "return $1[0];" jsHead :: JSArray -> JSVal
foreign import javascript unsafe "return $1.slice(1);" jsTail :: JSArray -> JSArray
foreign import javascript unsafe "return $1.length;" jsLength :: JSArray -> Int
foreign import javascript unsafe "return $1[$2];" jsAt :: JSArray -> Int -> JSVal
foreign import javascript unsafe "$1.push($2); return $1;" jsPush :: JSArray -> JSVal -> JSArray

foreign import javascript unsafe "return $1.toString();" jsToString_ :: JSVal -> JSString

jsToString :: IsJS a => a -> String
jsToString = fromJSString . jsToString_ . toJSVal

instance Show JSVal where
  show = jsToString

bamboozle :: [a] -> Bool
bamboozle [] = False
bamboozle _ = True
{-# NOINLINE bamboozle #-}

jsNil :: JSArray
jsNil = jsNil_ False

infixr 5 ++#
(++#) :: IsJS a => a -> JSArray -> JSArray
(++#) x = jsCons $ toJSVal x

listToArray :: IsJS a => [a] -> JSArray
listToArray xs = foldl' (\arr x -> jsPush arr $ toJSVal x) (jsNil_ $ bamboozle xs) xs

arrayToList :: IsJS a => JSArray -> [a]
arrayToList arr = fromJSVal . jsAt arr <$> [0..jsLength arr-1]

instance IsJS a => IsJS [a] where
  fromJSVal = arrayToList . fromJSVal
  toJSVal = toJSVal . listToArray

listToArraySt :: IsJSSt a => [a] -> St JSArray
listToArraySt xs = foldlM (\arr x -> jsPush arr <$> toJSValSt x) (jsNil_ $ bamboozle xs) xs

arrayToListSt :: IsJSSt a => JSArray -> St [a]
arrayToListSt arr = mapM (fromJSValSt . jsAt arr) [0..jsLength arr-1]

instance IsJSSt a => IsJSSt [a] where
  fromJSValSt = arrayToListSt . fromJSVal
  toJSValSt = fmap toJSVal . listToArraySt

foreign import javascript unsafe "return undefined;" jsUndefined :: JSVal
foreign import javascript unsafe "return $1 === undefined;" jsIsUndefined :: JSVal -> Bool

instance IsJS a => IsJS (Maybe a) where
  fromJSVal x = if jsIsUndefined x then Nothing else Just $ fromJSVal x
  toJSVal (Just x) = toJSVal x
  toJSVal Nothing = jsUndefined

foreign import javascript unsafe "return $1;" valToDouble :: JSVal -> Double
foreign import javascript unsafe "return $1;" doubleToVal :: Double -> JSVal

instance IsJS Double where
  fromJSVal = valToDouble
  toJSVal = doubleToVal

foreign import javascript unsafe "return $1;" valToInt :: JSVal -> Int
foreign import javascript unsafe "return $1;" intToVal :: Int -> JSVal

instance IsJS Int where
  fromJSVal = valToInt
  toJSVal = intToVal

instance IsJS Natural where
  fromJSVal = toEnum . fromJSVal
  toJSVal = toJSVal . fromEnum

foreign import javascript unsafe "return $1;" valToBool :: JSVal -> Bool
foreign import javascript unsafe "return $1;" boolToVal :: Bool -> JSVal

instance IsJS Bool where
  fromJSVal = valToBool
  toJSVal = boolToVal

instance IsJS (Complex Double) where
  fromJSVal xs = let [r, i] = arrayToList $ fromJSVal xs in r :+ i
  toJSVal (r :+ i) = toJSVal $ listToArray [r, i]

foreign import javascript unsafe "return [$1, $2];" jsPair :: JSVal -> JSVal -> JSVal
foreign import javascript unsafe "return $1[0];" jsFst :: JSVal -> JSVal
foreign import javascript unsafe "return $1[1];" jsSnd :: JSVal -> JSVal

instance (IsJS a, IsJS b) => IsJS (a, b) where
  fromJSVal xy = (fromJSVal $ jsFst xy, fromJSVal $ jsSnd xy)
  toJSVal (x, y) = jsPair (toJSVal x) (toJSVal y)

instance (IsJSSt a, IsJSSt b) => IsJSSt (a, b) where
  fromJSValSt xy = liftA2 (,) (fromJSValSt $ jsFst xy) (fromJSValSt $ jsSnd xy)
  toJSValSt (x, y) = do
    x' <- toJSValSt x
    y' <- toJSValSt y
    pure $ jsPair x' y'

foreign import javascript unsafe "return Object.entries($1);" jsEntries :: JSVal -> JSArray
foreign import javascript unsafe "return Object.fromEntries($1);" jsFromEntries :: JSArray -> JSVal

valToObject :: JSVal -> [(String, JSVal)]
valToObject = map (first fromJSString) . fromJSVal . toJSVal . jsEntries

objectToVal :: [(String, JSVal)] -> JSVal
objectToVal = jsFromEntries . fromJSVal . toJSVal . map (first toJSString)

foreign import javascript unsafe "return typeof $1;" jsTypeOf :: JSVal -> JSString
foreign import javascript unsafe "return Array.isArray($1);" jsIsArray :: JSVal -> Bool

instance IsJS VariableType where
  fromJSVal v = case fromJSString $ fromJSVal v of
    "normal" -> VariableNormal
    "constant" -> VariableConstant
    "private" -> VariablePrivate
    _ -> error "fromJSVal VariableType: wrong type"
  toJSVal VariableNormal = toJSVal $ toJSString "normal"
  toJSVal VariableConstant = toJSVal $ toJSString "constant"
  toJSVal VariablePrivate = toJSVal $ toJSString "private"

instance IsJSSt VariableType where
  fromJSValSt v = case fromJSString $ fromJSVal v of
    "normal" -> pure VariableNormal
    "constant" -> pure VariableConstant
    "private" -> pure VariablePrivate
    _ -> throwError $ DomainError "fromJSValSt VariableType: wrong type"
  toJSValSt VariableNormal = pure $ toJSVal $ toJSString "normal"
  toJSValSt VariableConstant = pure $ toJSVal $ toJSString "constant"
  toJSValSt VariablePrivate = pure $ toJSVal $ toJSString "private"

instance IsJS ScalarValue where
  fromJSVal v = case fromJSString $ jsTypeOf v of
    "number" -> Number $ fromJSVal v :+ 0
    "string" -> Character $ fromJSVal v
    "object" ->
      if jsIsArray v then Number $ fromJSVal v
      else if fromJSVal (jsLookup v $ toJSString "type") `elem` ["array", "dictionary"] then Box $ fromJSVal v
      else error "fromJSVal ScalarValue: wrong or unsupported type"
    _ -> error "fromJSVal ScalarValue: wrong type"
  toJSVal (Number x) = toJSVal x
  toJSVal (Character x) = toJSVal x
  toJSVal (Box xs) = toJSVal xs
  toJSVal _ = error "toJSVal ScalarValue: unsupported type"

instance IsJSSt ScalarValue where
  fromJSValSt v = case fromJSString $ jsTypeOf v of
    "number" -> pure $ Number $ fromJSVal v :+ 0
    "string" -> pure $ Character $ fromJSVal v
    "object" ->
      if jsIsArray v then pure $ Number $ fromJSVal v
      else if fromJSVal (jsLookup v $ toJSString "type") `elem` ["array", "dictionary"] then Box <$> fromJSValSt v
      else if fromJSVal (jsLookup v $ toJSString "type") == "function" then Wrap <$> fromJSValSt v
      else if fromJSVal (jsLookup v $ toJSString "type") == "adverb" then AdverbWrap <$> fromJSValSt v
      else if fromJSVal (jsLookup v $ toJSString "type") == "conjunction" then ConjunctionWrap <$> fromJSValSt v
      else if fromJSVal (jsLookup v $ toJSString "type") == "struct" then do
        ctx <- getContext
        scope <- foldrM (\(n, v) s -> fromJSValSt v >>= \(t, v') -> scopeUpdate True n t v' s) (Scope [] [] [] [] Nothing True) (valToObject $ jsLookup v $ toJSString "entries") >>= createRef
        pure $ Struct ctx{ contextScope = scope }
      else throwError $ DomainError "fromJSValSt ScalarValue: wrong type"
    _ -> throwError $ DomainError "fromJSValSt ScalarValue: wrong type"
  toJSValSt (Number x) = pure $ toJSVal x
  toJSValSt (Character x) = pure $ toJSVal x
  toJSValSt (Box xs) = toJSValSt xs
  toJSValSt (Wrap fn) = toJSValSt fn
  toJSValSt (AdverbWrap adv) = toJSValSt adv
  toJSValSt (ConjunctionWrap conj) = toJSValSt conj
  toJSValSt (Struct ctx) = do
    scope <- readRef $ contextScope ctx
    entries <- mapM (secondM toJSValSt) $ scopeEntries scope
    pure $ objectToVal [("type", toJSVal $ toJSString "struct"), ("entries", objectToVal entries)]

foreign import javascript unsafe "return $1[$2];" jsLookup :: JSVal -> JSString -> JSVal

instance IsJS Noun where
  fromJSVal v
    | fromJSVal (jsLookup v $ toJSString "type") == "array" = let
      shape = arrayToList $ fromJSVal $ jsLookup v $ toJSString "shape"
      contents = arrayToList $ fromJSVal $ jsLookup v $ toJSString "contents"
      in Array shape contents
    | fromJSVal (jsLookup v $ toJSString "type") == "dictionary" = let
      entries = arrayToList $ fromJSVal $ jsLookup v $ toJSString "entries"
      in dictionary entries
    | otherwise = error "fromJSVal Noun: not a noun"
  toJSVal (Array shape contents) = objectToVal [("type", toJSVal $ toJSString "array"), ("shape", toJSVal shape), ("contents", toJSVal contents)]
  toJSVal (Dictionary ks vs) = objectToVal [("type", toJSVal $ toJSString "dictionary"), ("entries", toJSVal $ listToArray $ zip ks vs)]

instance IsJSSt Noun where
  fromJSValSt v
    | fromJSVal (jsLookup v $ toJSString "type") == "array" = do
      let shape = arrayToList $ fromJSVal $ jsLookup v $ toJSString "shape"
      contents <- arrayToListSt $ fromJSVal $ jsLookup v $ toJSString "contents"
      pure $ Array shape contents
    | fromJSVal (jsLookup v $ toJSString "type") == "dictionary" = do
      entries <- arrayToListSt $ fromJSVal $ jsLookup v $ toJSString "entries"
      pure $ dictionary entries
    | otherwise = throwError $ DomainError "fromJSValSt Noun: not a noun"
  toJSValSt (Array shape contents) = do
    let shape' = toJSVal shape
    contents' <- toJSValSt contents
    pure $ objectToVal [("type", toJSVal $ toJSString "array"), ("shape", shape'), ("contents", contents')]
  toJSValSt (Dictionary ks vs) = do
    es <- listToArraySt $ zip ks vs
    pure $ objectToVal [("type", toJSVal $ toJSString "dictionary"), ("entries", toJSVal $ es)]

instance IsJS Error where
  fromJSVal v = let
    typ = fromJSVal $ jsLookup v $ toJSString "code"
    message = fromJSString $ fromJSVal $ jsLookup v $ toJSString "message"
    in fromErrorCode typ message
  toJSVal err = objectToVal [("code", toJSVal $ errorCode err), ("message", toJSVal $ toJSString $ errorMessage err)]

foreign import javascript unsafe "return $1 in $2;" jsIn :: JSString -> JSVal -> Bool

instance IsJS a => IsJS (Either Error a) where
  fromJSVal v = if jsIn (toJSString "code") v then Left $ fromJSVal v else Right $ fromJSVal v
  toJSVal (Left err) = toJSVal err
  toJSVal (Right x) = toJSVal x

instance IsJSSt a => IsJSSt (Either Error a) where
  fromJSValSt v = if jsIn (toJSString "code") v then pure $ Left $ fromJSVal v else Right <$> fromJSValSt v
  toJSValSt (Left err) = pure $ toJSVal err
  toJSValSt (Right x) = toJSValSt x

foreign import javascript safe "return await $1();" jsCall0 :: JSVal -> IO JSVal
foreign import javascript safe "return await $1($2);" jsCall1 :: JSVal -> JSVal -> IO JSVal
foreign import javascript safe "return await $1($2, $3);" jsCall2 :: JSVal -> JSVal -> JSVal -> IO JSVal
foreign import javascript safe "return await $1($2, $3, $4);" jsCall3 :: JSVal -> JSVal -> JSVal -> JSVal -> IO JSVal

foreign import javascript safe "wrapper" jsWrap0 :: IO JSVal -> IO JSVal
foreign import javascript safe "wrapper" jsWrap1 :: (JSVal -> IO JSVal) -> IO JSVal
foreign import javascript safe "wrapper" jsWrap2 :: (JSVal -> JSVal -> IO JSVal) -> IO JSVal
foreign import javascript safe "wrapper" jsWrap3 :: (JSVal -> JSVal -> JSVal -> IO JSVal) -> IO JSVal

instance IsJSSt Function where
  fromJSValSt v
    | fromJSVal (jsLookup v $ toJSString "type") == "function" = do
      let repr = fromJSString $ fromJSVal $ jsLookup v $ toJSString "repr"
      let monad = jsLookup v $ toJSString "monad"
      let dyad = jsLookup v $ toJSString "dyad"
      let un = jsLookup v $ toJSString "un"
      let anti = jsLookup v $ toJSString "anti"
      let contra = jsLookup v $ toJSString "contra"
      let dis = jsLookup v $ toJSString "dis"
      let bi = jsLookup v $ toJSString "bi"
      let ana = jsLookup v $ toJSString "ana"
      id <- assignId
      pure $ DefinedFunction {
        functionRepr = repr,
        functionContext = Nothing,
        functionMonad = if jsIsUndefined monad then Nothing else Just $ (\ea x -> do
          ea' <- toJSValSt ea
          x' <- toJSValSt x
          res <- liftToSt (jsCall2 monad ea' x') >>= fromJSValSt
          liftEither res),
        functionDyad = if jsIsUndefined dyad then Nothing else Just $ (\ea x y -> do
          ea' <- toJSValSt ea
          x' <- toJSValSt x
          y' <- toJSValSt y
          res <- liftToSt (jsCall3 dyad ea' x' y') >>= fromJSValSt
          liftEither res),
        functionUn = if jsIsUndefined un then Nothing else Just $ (\ea x -> do
          ea' <- toJSValSt ea
          x' <- toJSValSt x
          res <- liftToSt (jsCall2 un ea' x') >>= fromJSValSt
          liftEither res),
        functionAnti = if jsIsUndefined anti then Nothing else Just $ (\ea x y -> do
          ea' <- toJSValSt ea
          x' <- toJSValSt x
          y' <- toJSValSt y
          res <- liftToSt (jsCall3 anti ea' x' y') >>= fromJSValSt
          liftEither res),
        functionContra = if jsIsUndefined contra then Nothing else Just $ (\ea x y -> do
          ea' <- toJSValSt ea
          x' <- toJSValSt x
          y' <- toJSValSt y
          res <- liftToSt (jsCall3 contra ea' x' y') >>= fromJSValSt
          liftEither res),
        functionDis = if jsIsUndefined dis then Nothing else Just $ (\ea x -> do
          ea' <- toJSValSt ea
          x' <- toJSValSt x
          res <- liftToSt (jsCall2 dis ea' x') >>= fromJSValSt
          liftEither res),
        functionBi = if jsIsUndefined bi then Nothing else Just $ (\ea x -> do
          ea' <- toJSValSt ea
          x' <- toJSValSt x
          res <- liftToSt (jsCall2 bi ea' x') >>= fromJSValSt
          liftEither res),
        functionAna = if jsIsUndefined ana then Nothing else Just $ (\ea x y -> do
          ea' <- toJSValSt ea
          x' <- toJSValSt x
          y' <- toJSValSt y
          res <- liftToSt (jsCall3 ana ea' x' y') >>= fromJSValSt
          liftEither res),
        definedFunctionId = id }
    | otherwise = throwError $ DomainError "fromJSValSt Function: not a function"
  toJSValSt f = do
    ctx <- getContext
    fS <- showM f
    monad <- liftToSt $ jsWrap2 $ \ea x -> do
      ea' <- fromRight' . second fst <$> (runResult $ runSt (fromJSValSt ea) ctx)
      x' <- fromRight' . second fst <$> (runResult $ runSt (fromJSValSt x) ctx)
      r <- second fst <$> (runResult $ runSt (callMonad f ea' x') ctx)
      fromRight' . second fst <$> (runResult $ runSt (toJSValSt r) ctx)
    dyad <- liftToSt $ jsWrap3 $ \ea x y -> do
      ea' <- fromRight' . second fst <$> (runResult $ runSt (fromJSValSt ea) ctx)
      x' <- fromRight' . second fst <$> (runResult $ runSt (fromJSValSt x) ctx)
      y' <- fromRight' . second fst <$> (runResult $ runSt (fromJSValSt y) ctx)
      r <- second fst <$> (runResult $ runSt (callDyad f ea' x' y') ctx)
      fromRight' . second fst <$> (runResult $ runSt (toJSValSt r) ctx)
    un <- liftToSt $ jsWrap2 $ \ea x -> do
      ea' <- fromRight' . second fst <$> (runResult $ runSt (fromJSValSt ea) ctx)
      x' <- fromRight' . second fst <$> (runResult $ runSt (fromJSValSt x) ctx)
      r <- second fst <$> (runResult $ runSt (callUn f ea' x') ctx)
      fromRight' . second fst <$> (runResult $ runSt (toJSValSt r) ctx)
    anti <- liftToSt $ jsWrap3 $ \ea x y -> do
      ea' <- fromRight' . second fst <$> (runResult $ runSt (fromJSValSt ea) ctx)
      x' <- fromRight' . second fst <$> (runResult $ runSt (fromJSValSt x) ctx)
      y' <- fromRight' . second fst <$> (runResult $ runSt (fromJSValSt y) ctx)
      r <- second fst <$> (runResult $ runSt (callAnti f ea' x' y') ctx)
      fromRight' . second fst <$> (runResult $ runSt (toJSValSt r) ctx)
    contra <- liftToSt $ jsWrap3 $ \ea x y -> do
      ea' <- fromRight' . second fst <$> (runResult $ runSt (fromJSValSt ea) ctx)
      x' <- fromRight' . second fst <$> (runResult $ runSt (fromJSValSt x) ctx)
      y' <- fromRight' . second fst <$> (runResult $ runSt (fromJSValSt y) ctx)
      r <- second fst <$> (runResult $ runSt (callContra f ea' x' y') ctx)
      fromRight' . second fst <$> (runResult $ runSt (toJSValSt r) ctx)
    dis <- liftToSt $ jsWrap2 $ \ea x -> do
      ea' <- fromRight' . second fst <$> (runResult $ runSt (fromJSValSt ea) ctx)
      x' <- fromRight' . second fst <$> (runResult $ runSt (fromJSValSt x) ctx)
      r <- second fst <$> (runResult $ runSt (callDis f ea' x') ctx)
      fromRight' . second fst <$> (runResult $ runSt (toJSValSt r) ctx)
    bi <- liftToSt $ jsWrap2 $ \ea x -> do
      ea' <- fromRight' . second fst <$> (runResult $ runSt (fromJSValSt ea) ctx)
      x' <- fromRight' . second fst <$> (runResult $ runSt (fromJSValSt x) ctx)
      r <- second fst <$> (runResult $ runSt (callBi f ea' x') ctx)
      fromRight' . second fst <$> (runResult $ runSt (toJSValSt r) ctx)
    ana <- liftToSt $ jsWrap3 $ \ea x y -> do
      ea' <- fromRight' . second fst <$> (runResult $ runSt (fromJSValSt ea) ctx)
      x' <- fromRight' . second fst <$> (runResult $ runSt (fromJSValSt x) ctx)
      y' <- fromRight' . second fst <$> (runResult $ runSt (fromJSValSt y) ctx)
      r <- second fst <$> (runResult $ runSt (callAna f ea' x' y') ctx)
      fromRight' . second fst <$> (runResult $ runSt (toJSValSt r) ctx)
    pure $ objectToVal [("type", toJSVal $ toJSString "function"), ("repr", toJSVal $ toJSString fS), ("monad", monad), ("dyad", dyad), ("un", un), ("anti", anti), ("contra", contra), ("dis", dis), ("bi", bi), ("ana", ana)]

instance IsJSSt Adverb where
  fromJSValSt v
    | fromJSVal (jsLookup v $ toJSString "type") == "adverb" = do
      let repr = fromJSString $ fromJSVal $ jsLookup v $ toJSString "repr"
      let onArray = jsLookup v $ toJSString "array"
      let onFunction = jsLookup v $ toJSString "function"
      id <- assignId
      pure $ DefinedAdverb {
        adverbRepr = repr,
        adverbContext = Nothing,
        adverbOnNoun = if jsIsUndefined onArray then Nothing else Just $ (\ea x -> do
          ea' <- toJSValSt ea
          x' <- toJSValSt x
          res <- liftToSt (jsCall2 onArray ea' x') >>= fromJSValSt
          liftEither res),
        adverbOnFunction = if jsIsUndefined onFunction then Nothing else Just $ (\ea x -> do
          ea' <- toJSValSt ea
          x' <- toJSValSt x
          res <- liftToSt (jsCall2 onFunction ea' x') >>= fromJSValSt
          liftEither res),
        definedAdverbId = id }
    | otherwise = throwError $ DomainError "fromJSValSt Adverb: not an adverb"
  toJSValSt a = do
    ctx <- getContext
    aS <- showM a
    onArray <- liftToSt $ jsWrap2 $ \ea x -> do
      ea' <- fromRight' . second fst <$> (runResult $ runSt (fromJSValSt ea) ctx)
      x' <- fromRight' . second fst <$> (runResult $ runSt (fromJSValSt x) ctx)
      r <- second fst <$> (runResult $ runSt (callOnNoun a ea' x') ctx)
      fromRight' . second fst <$> (runResult $ runSt (toJSValSt r) ctx)
    onFunction <- liftToSt $ jsWrap2 $ \ea x -> do
      ea' <- fromRight' . second fst <$> (runResult $ runSt (fromJSValSt ea) ctx)
      x' <- fromRight' . second fst <$> (runResult $ runSt (fromJSValSt x) ctx)
      r <- second fst <$> (runResult $ runSt (callOnFunction a ea' x') ctx)
      fromRight' . second fst <$> (runResult $ runSt (toJSValSt r) ctx)
    pure $ objectToVal [("type", toJSVal $ toJSString "adverb"), ("repr", toJSVal $ toJSString aS), ("array", onArray), ("function", onFunction)]

instance IsJSSt Conjunction where
  fromJSValSt v
    | fromJSVal (jsLookup v $ toJSString "type") == "conjunction" = do
      let repr = fromJSString $ fromJSVal $ jsLookup v $ toJSString "repr"
      let onArrayArray = jsLookup v $ toJSString "arrayArray"
      let onArrayFunction = jsLookup v $ toJSString "arrayFunction"
      let onFunctionArray = jsLookup v $ toJSString "functionArray"
      let onFunctionFunction = jsLookup v $ toJSString "functionFunction"
      id <- assignId
      pure $ DefinedConjunction {
        conjRepr = repr,
        conjContext = Nothing,
        conjOnNounNoun = if jsIsUndefined onArrayArray then Nothing else Just $ (\ea x y -> do
          ea' <- toJSValSt ea
          x' <- toJSValSt x
          y' <- toJSValSt y
          res <- liftToSt (jsCall3 onArrayArray ea' x' y') >>= fromJSValSt
          liftEither res),
        conjOnNounFunction = if jsIsUndefined onArrayFunction then Nothing else Just $ (\ea x y -> do
          ea' <- toJSValSt ea
          x' <- toJSValSt x
          y' <- toJSValSt y
          res <- liftToSt (jsCall3 onArrayFunction ea' x' y') >>= fromJSValSt
          liftEither res),
        conjOnFunctionNoun = if jsIsUndefined onFunctionArray then Nothing else Just $ (\ea x y -> do
          ea' <- toJSValSt ea
          x' <- toJSValSt x
          y' <- toJSValSt y
          res <- liftToSt (jsCall3 onFunctionArray ea' x' y') >>= fromJSValSt
          liftEither res),
        conjOnFunctionFunction = if jsIsUndefined onFunctionFunction then Nothing else Just $ (\ea x y -> do
          ea' <- toJSValSt ea
          x' <- toJSValSt x
          y' <- toJSValSt y
          res <- liftToSt (jsCall3 onFunctionFunction ea' x' y') >>= fromJSValSt
          liftEither res),
        definedConjunctionId = id }
    | otherwise = throwError $ DomainError "fromJSValSt Conjunction: not a conjunction"
  toJSValSt c = do
    ctx <- getContext
    cS <- showM c
    onArrayArray <- liftToSt $ jsWrap3 $ \ea x y -> do
      ea' <- fromRight' . second fst <$> (runResult $ runSt (fromJSValSt ea) ctx)
      x' <- fromRight' . second fst <$> (runResult $ runSt (fromJSValSt x) ctx)
      y' <- fromRight' . second fst <$> (runResult $ runSt (fromJSValSt y) ctx)
      r <- second fst <$> (runResult $ runSt (callOnNounAndNoun c ea' x' y') ctx)
      fromRight' . second fst <$> (runResult $ runSt (toJSValSt r) ctx)
    onArrayFunction <- liftToSt $ jsWrap3 $ \ea x y -> do
      ea' <- fromRight' . second fst <$> (runResult $ runSt (fromJSValSt ea) ctx)
      x' <- fromRight' . second fst <$> (runResult $ runSt (fromJSValSt x) ctx)
      y' <- fromRight' . second fst <$> (runResult $ runSt (fromJSValSt y) ctx)
      r <- second fst <$> (runResult $ runSt (callOnNounAndFunction c ea' x' y') ctx)
      fromRight' . second fst <$> (runResult $ runSt (toJSValSt r) ctx)
    onFunctionArray <- liftToSt $ jsWrap3 $ \ea x y -> do
      ea' <- fromRight' . second fst <$> (runResult $ runSt (fromJSValSt ea) ctx)
      x' <- fromRight' . second fst <$> (runResult $ runSt (fromJSValSt x) ctx)
      y' <- fromRight' . second fst <$> (runResult $ runSt (fromJSValSt y) ctx)
      r <- second fst <$> (runResult $ runSt (callOnFunctionAndNoun c ea' x' y') ctx)
      fromRight' . second fst <$> (runResult $ runSt (toJSValSt r) ctx)
    onFunctionFunction <- liftToSt $ jsWrap3 $ \ea x y -> do
      ea' <- fromRight' . second fst <$> (runResult $ runSt (fromJSValSt ea) ctx)
      x' <- fromRight' . second fst <$> (runResult $ runSt (fromJSValSt x) ctx)
      y' <- fromRight' . second fst <$> (runResult $ runSt (fromJSValSt y) ctx)
      r <- second fst <$> (runResult $ runSt (callOnFunctionAndFunction c ea' x' y') ctx)
      fromRight' . second fst <$> (runResult $ runSt (toJSValSt r) ctx)
    pure $ objectToVal [("type", toJSVal $ toJSString "conjunction"), ("repr", toJSVal $ toJSString cS), ("arrayArray", onArrayArray), ("arrayFunction", onArrayFunction), ("functionArray", onFunctionArray), ("functionFunction", onFunctionFunction)]

instance IsJSSt Value where
  fromJSValSt v
    | fromJSVal (jsLookup v $ toJSString "type") `elem` ["array", "dictionary"] = VNoun <$> fromJSValSt v
    | fromJSVal (jsLookup v $ toJSString "type") == "function" = VFunction <$> fromJSValSt v
    | fromJSVal (jsLookup v $ toJSString "type") == "adverb" = VAdverb <$> fromJSValSt v
    | fromJSVal (jsLookup v $ toJSString "type") == "conjunction" = VConjunction <$> fromJSValSt v
    | otherwise = throwError $ DomainError "fromJSValSt Value: unknown type"
  toJSValSt (VNoun arr) = toJSValSt arr
  toJSValSt (VFunction f) = toJSValSt f
  toJSValSt (VAdverb adv) = toJSValSt adv
  toJSValSt (VConjunction conj) = toJSValSt conj

instance IsJSSt Nilad where
  fromJSValSt v
    | fromJSVal (jsLookup v $ toJSString "type") == "nilad" = do
      let repr = fromJSString $ fromJSVal $ jsLookup v $ toJSString "repr"
      let get = jsLookup v $ toJSString "get"
      let set = jsLookup v $ toJSString "set"
      sc <- createRef $ Scope [] [] [] [] Nothing True
      ctx <- getContext
      pure $ Nilad {
        niladRepr = repr,
        niladContext = Just $ ctx{ contextScope = sc },
        niladGet = if jsIsUndefined get then Nothing else Just $ (liftToSt (jsCall0 get) >>= fromJSValSt),
        niladSet = if jsIsUndefined set then Nothing else Just $ (\x -> void $ toJSValSt x >>= liftToSt . jsCall1 set) }
    | otherwise = throwError $ DomainError "fromJSValSt Nilad: not a nilad"
  toJSValSt n = do
    ctx <- getContext
    get <- liftToSt $ jsWrap0 $ do
      r <- second fst <$> (runResult $ runSt (getNilad n) ctx)
      fromRight' . second fst <$> (runResult $ runSt (toJSValSt r) ctx)
    set <- liftToSt $ jsWrap1 $ \x -> do
      x' <- fromRight' . second fst <$> (runResult $ runSt (fromJSValSt x) ctx)
      r <- second (const jsUndefined) <$> (runResult $ runSt (setNilad n x') ctx)
      fromRight' . second fst <$> (runResult $ runSt (toJSValSt r) ctx)
    pure $ objectToVal [("type", toJSVal $ toJSString "nilad"), ("repr", toJSVal $ toJSString $ niladRepr n), ("get", get), ("set", set)]
