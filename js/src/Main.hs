{-# LANGUAGE LambdaCase #-}

module Main
  ( main
  , newContext
  , runCodeJS
  , getGlobalsJS
  , getGlobal
  , setGlobal
  , glyphsSyntaxJS
  , glyphsIdentifiersJS
  , glyphsArraysJS
  , glyphsFunctionsJS
  , glyphsAdverbsJS
  , glyphsConjunctionsJS
  , jsHighlight
  , splitString
  , joinString
  , hlOther
  , hlSyntax
  , hlNumber
  , hlString
  , hlStringEscape
  , hlArrayName
  , hlPrimArray
  , hlFunctionName
  , hlPrimFunction
  , hlAdverbName
  , hlPrimAdverb
  , hlConjunctionName
  , hlPrimConjunction
  , hlComment
  , errUser
  , errDomain
  , errLength
  , errRank
  , errNYI
  , errSyntax
  , errAssertion
  , showJS
  , reprJS
  , varArrowJS ) where  

import JSBridge

import TinyAPL.Noun
import TinyAPL.Function
import TinyAPL.Adverb
import TinyAPL.Conjunction
import TinyAPL.Context
import TinyAPL.Value
import TinyAPL.Quads
import TinyAPL.CoreQuads
import TinyAPL.Error
import TinyAPL.Glyphs (syntax, identifiers, arrays, functions, adverbs, conjunctions, quad, assign, assignConstant, assignPrivate)
import TinyAPL.Highlighter
import TinyAPL.Interpreter
import TinyAPL.Parser
import TinyAPL.Util
import TinyAPL.Primitives (primitives)

import Data.IORef
import GHC.Wasm.Prim
import System.IO.Unsafe
import Data.Bifunctor
import Control.Exception
import System.Info

foreign export javascript "hs_start" main :: IO ()

main :: IO ()
main = return ()

foreign import javascript safe "return await fetch($1).then(x => x.text());" fetchStr :: JSString -> IO JSString

readImportUrl :: String -> St String
readImportUrl = liftToSt . fmap fromJSString . fetchStr . toJSString

{-# NOINLINE contexts #-}
contexts :: IORef [Context]
contexts = unsafePerformIO $ newIORef []

{-# NOINLINE lasts #-}
lasts :: IORef [Maybe Value]
lasts = unsafePerformIO $ newIORef []

noLast :: Error
noLast = DomainError "Last not found or has wrong type"

foreign import javascript safe "return await $1();" callInput :: JSVal -> IO JSString
foreign import javascript safe "await $1($2);" callOutput :: JSVal -> JSString -> IO ()

foreign export javascript "tinyapl_newContext" newContext :: JSVal -> JSVal -> JSVal -> JSVal -> JSString -> IO Int

lastQuads :: Int -> Quads
lastQuads l = let readLast = (!! l) <$> (liftToSt $ readIORef lasts) in
  quadsFromReprs [Nilad (Just $ do
    l <- readLast
    case l of
      Just (VNoun arr) -> return arr
      _ -> throwError noLast
  ) Nothing (quad : "last") Nothing] [PrimitiveFunction (Just $ \ea y -> do
    l <- readLast
    case l of
      Just (VFunction f) -> callMonad f ea y
      _ -> throwError noLast
  ) (Just $ \ea x y -> do
    l <- readLast
    case l of
      Just (VFunction f) -> callDyad f ea x y
      _ -> throwError noLast
  ) Nothing Nothing Nothing Nothing Nothing Nothing (quad : "Last") Nothing] [PrimitiveAdverb (Just $ \ea u -> do
    l <- readLast
    case l of
      Just (VAdverb adv) -> callOnNoun adv ea u
      _ -> throwError noLast
  ) (Just $ \ea f -> do
    l <- readLast
    case l of
      Just (VAdverb adv) -> callOnFunction adv ea f
      _ -> throwError noLast
  ) (quad : "_Last") Nothing] [PrimitiveConjunction (Just $ \ea u v -> do
    l <- readLast
    case l of
      Just (VConjunction conj) -> callOnNounAndNoun conj ea u v
      _ -> throwError noLast
  ) (Just $ \ea u g -> do
    l <- readLast
    case l of
      Just (VConjunction conj) -> callOnNounAndFunction conj ea u g
      _ -> throwError noLast
  ) (Just $ \ea f v -> do
    l <- readLast
    case l of
      Just (VConjunction conj) -> callOnFunctionAndNoun conj ea f v
      _ -> throwError noLast
  ) (Just $ \ea f g -> do
    l <- readLast
    case l of
      Just (VConjunction conj) -> callOnFunctionAndFunction conj ea f g
      _ -> throwError noLast
  ) (quad : "_Last_") Nothing]

newContext :: JSVal -> JSVal -> JSVal -> JSVal -> JSString -> IO Int
newContext input output error quads cwd = do
  l <- length <$> readIORef contexts
  emptyScope <- newIORef $ Scope [] [] [] [] Nothing True
  let cwd' = fromJSString cwd
  let input' = liftToSt $ fromJSString <$> callInput input
  let output' = liftToSt . callOutput output . toJSString
  let error' = liftToSt . callOutput error . toJSString
  id <- newIORef 0
  let qpc = Context emptyScope core input' output' error' id cwd' primitives
  qs <- fromRight' . second fst <$> (runResult $ runSt (mapM (secondM fromJSValSt) $ valToObject quads) qpc )
  nilads <- secondM (\x -> fromRight' . second fst <$> (runResult $ runSt (fromJSValSt x) qpc)) `mapM` filter (isArrayName . fst) qs
  functions <- secondM (\x -> fromRight' . second fst <$> (runResult $ runSt (fromJSValSt x) qpc)) `mapM` filter (isFunctionName . fst) qs
  adverbs <- secondM (\x -> fromRight' . second fst <$> (runResult $ runSt (fromJSValSt x) qpc)) `mapM` filter (isAdverbName . fst) qs
  conjunctions <- secondM (\x -> fromRight' . second fst <$> (runResult $ runSt (fromJSValSt x) qpc)) `mapM` filter (isConjunctionName . fst) qs
  modifyIORef contexts (++ [Context
    { contextScope = emptyScope
    , contextQuads = core <> lastQuads l <> Quads {
      quadArrays = first (quad :) <$> nilads,
      quadFunctions = first (quad :) <$> functions,
      quadAdverbs = first (quad :) <$> adverbs,
      quadConjunctions = first (quad :) <$> conjunctions } <> quadsFromReprs [ makeSystemInfo os arch True bigEndian ] [ makeImport readImportUrl Nothing ] [] []
    , contextIn = input'
    , contextOut = output'
    , contextErr = error'
    , contextIncrementalId = id
    , contextDirectory = cwd'
    , contextPrimitives = primitives }])
  modifyIORef lasts (++ [Nothing])
  return l

runCode :: Int -> String -> IO (Either Error Value)
runCode contextId code = do
  context <- (!! contextId) <$> readIORef contexts
  let file = "<tinyapl js>"
  (result, context') <- fmap fromRight' $ runResult $ flip runSt context $ runAndCatch $ run' file code
  modifyIORef contexts (setAt contextId context')
  case result of
    (Panicked ex) -> return $ Left $ HaskellError $ displayException ex
    (Thrown err) -> return $ Left err
    (Succeeded res) -> do
      modifyIORef lasts (setAt contextId $ Just res)
      return $ Right res

foreign export javascript "tinyapl_runCode" runCodeJS :: Int -> JSString -> IO JSVal

runCodeJS :: Int -> JSString -> IO JSVal
runCodeJS contextId code = do
  context <- (!! contextId) <$> readIORef contexts
  res <- runCode contextId $ fromJSString code
  fst . fromRight' <$> (runResult $ runSt (toJSValSt res) context)

getGlobals :: Int -> IO [String]
getGlobals contextId = do
  context <- (!! contextId) <$> readIORef contexts
  scope <- readIORef $ contextScope context
  return $ (fst <$> scopeNouns scope) ++ (fst <$> scopeFunctions scope) ++ (fst <$> scopeAdverbs scope) ++ (fst <$> scopeConjunctions scope)

foreign export javascript "tinyapl_getGlobals" getGlobalsJS :: Int -> IO JSArray

getGlobalsJS :: Int -> IO JSArray
getGlobalsJS contextId = do
  globals <- getGlobals contextId
  return $ fromJSVal $ toJSVal $ map toJSString globals

foreign export javascript "tinyapl_getGlobal" getGlobal :: Int -> JSString -> IO JSVal

getGlobal :: Int -> JSString -> IO JSVal
getGlobal contextId name = do
  context <- (!! contextId) <$> readIORef contexts
  toJSVal . fmap fst <$> (runResult $ runSt (readRef (contextScope context) >>= scopeLookup True (fromJSString name) >>= (\case
    Just x -> toJSValSt x
    Nothing -> throwError $ DomainError $ "Global " ++ fromJSString name ++ " not found")) context)

foreign export javascript "tinyapl_setGlobal" setGlobal :: Int -> JSString -> JSVal -> IO JSVal

setGlobal :: Int -> JSString -> JSVal -> IO JSVal
setGlobal contextId name val = do
  context <- (!! contextId) <$> readIORef contexts
  toJSVal . fmap fst <$> (runResult $ runSt (do
    sc <- readRef (contextScope context)
    v <- fromJSValSt val
    sc' <- scopeUpdate True (fromJSString name) VariableNormal v sc
    writeRef (contextScope context) sc') context)
  
foreign export javascript "tinyapl_glyphsSyntax" glyphsSyntaxJS :: JSArray
foreign export javascript "tinyapl_glyphsIdentifiers" glyphsIdentifiersJS :: JSArray
foreign export javascript "tinyapl_glyphsArrays" glyphsArraysJS :: JSArray
foreign export javascript "tinyapl_glyphsFunctions" glyphsFunctionsJS :: JSArray
foreign export javascript "tinyapl_glyphsAdverbs" glyphsAdverbsJS :: JSArray
foreign export javascript "tinyapl_glyphsConjunctions" glyphsConjunctionsJS :: JSArray

glyphsSyntaxJS = fromJSVal $ toJSVal $ toJSVal <$> syntax :: JSArray
glyphsIdentifiersJS = fromJSVal $ toJSVal $ toJSVal <$> identifiers :: JSArray
glyphsArraysJS = fromJSVal $ toJSVal $ toJSVal <$> arrays :: JSArray 
glyphsFunctionsJS = fromJSVal $ toJSVal $ toJSVal <$> functions :: JSArray
glyphsAdverbsJS = fromJSVal $ toJSVal $ toJSVal <$> adverbs :: JSArray
glyphsConjunctionsJS = fromJSVal $ toJSVal $ toJSVal <$> conjunctions :: JSArray

foreign export javascript "tinyapl_highlight" jsHighlight :: JSString -> JSArray

jsHighlight :: JSString -> JSArray
jsHighlight = fromJSVal . toJSVal . map (toJSVal . fromEnum). highlight . fromJSString

foreign export javascript "tinyapl_splitString" splitString :: JSString -> JSArray

splitString :: JSString -> JSArray
splitString = fromJSVal . toJSVal . fromJSString

foreign export javascript "tinyapl_joinString" joinString :: JSArray -> JSString

joinString :: JSArray -> JSString
joinString = toJSString . fromJSVal . toJSVal

foreign export javascript "tinyapl_hlOther" hlOther :: Int
foreign export javascript "tinyapl_hlSyntax" hlSyntax :: Int
foreign export javascript "tinyapl_hlNumber" hlNumber :: Int
foreign export javascript "tinyapl_hlString" hlString :: Int
foreign export javascript "tinyapl_hlStringEscape" hlStringEscape :: Int
foreign export javascript "tinyapl_hlArrayName" hlArrayName :: Int
foreign export javascript "tinyapl_hlPrimArray" hlPrimArray :: Int
foreign export javascript "tinyapl_hlFunctionName" hlFunctionName :: Int
foreign export javascript "tinyapl_hlPrimFunction" hlPrimFunction :: Int
foreign export javascript "tinyapl_hlAdverbName" hlAdverbName :: Int
foreign export javascript "tinyapl_hlPrimAdverb" hlPrimAdverb :: Int
foreign export javascript "tinyapl_hlConjunctionName" hlConjunctionName :: Int
foreign export javascript "tinyapl_hlPrimConjunction" hlPrimConjunction :: Int
foreign export javascript "tinyapl_hlComment" hlComment :: Int

hlOther = fromEnum COther
hlSyntax = fromEnum CSyntax
hlNumber = fromEnum CNumber
hlString = fromEnum CString
hlStringEscape = fromEnum CStringEscape
hlArrayName = fromEnum CArrayName
hlPrimArray = fromEnum CPrimArray
hlFunctionName = fromEnum CFunctionName
hlPrimFunction = fromEnum CPrimFunction
hlAdverbName = fromEnum CAdverbName
hlPrimAdverb = fromEnum CPrimAdverb
hlConjunctionName = fromEnum CConjunctionName
hlPrimConjunction = fromEnum CPrimConjunction
hlComment = fromEnum CComment

foreign export javascript "tinyapl_errUser" errUser :: Int
foreign export javascript "tinyapl_errDomain" errDomain :: Int
foreign export javascript "tinyapl_errLength" errLength :: Int
foreign export javascript "tinyapl_errRank" errRank :: Int
foreign export javascript "tinyapl_errNYI" errNYI :: Int
foreign export javascript "tinyapl_errSyntax" errSyntax :: Int
foreign export javascript "tinyapl_errAssertion" errAssertion :: Int

errUser = errorCode $ UserError ""
errDomain = errorCode $ DomainError ""
errLength = errorCode $ LengthError ""
errRank = errorCode $ RankError ""
errNYI = errorCode $ NYIError ""
errSyntax = errorCode $ SyntaxError ""
errAssertion = errorCode $ AssertionError ""

foreign export javascript "tinyapl_show" showJS :: JSVal -> IO JSString

showJS :: JSVal -> IO JSString
showJS val = do
  scope <- newIORef $ Scope [] [] [] [] Nothing True
  id <- newIORef 0
  r <- fromRight' . second fst <$> (runResult $ runSt ((fromJSValSt val :: St (Either Error Value)) >>= secondME showM) (Context scope mempty undefined undefined undefined id "" primitives)) :: IO (Either Error String)
  pure $ toJSString $ case r of
    Left err -> show err
    Right val -> val

foreign export javascript "tinyapl_repr" reprJS :: JSVal -> IO JSString

reprJS :: JSVal -> IO JSString
reprJS val = do
  scope <- newIORef $ Scope [] [] [] [] Nothing True
  id <- newIORef 0
  r <- fromRight' . second fst <$> (runResult $ runSt (fromJSValSt val) (Context scope mempty undefined undefined undefined id "" primitives))
  toJSString <$> case r of
    VNoun arr -> fromRight' . second fst <$> (runResult $ runSt (showM $ Repr arr) (Context scope mempty undefined undefined undefined id "" primitives))
    o -> fromRight' . second fst <$> (runResult $ runSt (showM o) (Context scope mempty undefined undefined undefined id "" primitives))

varArrow :: VariableType -> Char
varArrow VariableNormal = assign
varArrow VariableConstant = assignConstant
varArrow VariablePrivate = assignPrivate

foreign export javascript "tinyapl_varArrow" varArrowJS :: JSVal -> JSVal

varArrowJS :: JSVal -> JSVal
varArrowJS = toJSVal . varArrow . fromJSVal
