{-# LANGUAGE CPP, FlexibleContexts, RankNTypes, TypeApplications, ScopedTypeVariables, AllowAmbiguousTypes, TupleSections, LambdaCase #-}

module TinyAPL.Quads.FFI where

#ifndef wasm32_HOST_ARCH

import TinyAPL.ArrayFunctionOperator
import TinyAPL.Error
import qualified TinyAPL.Glyphs as G

import Foreign
import Foreign.C
import qualified Foreign.LibFFI as FFI
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class
import Control.Monad
import TinyAPL.Complex
import Control.Monad.Catch
import Data.List
import Data.Maybe
import Numeric (showHex)
import Data.Functor hiding (unzip)
#if defined(MIN_VERSION_Win32)
import Foreign.Ptr (castPtrToFunPtr)
import System.Win32.DLL
import System.Win32.Types (HMODULE)
#else
import System.Posix.DynamicLinker
#endif

data FFIType
  = FFIVoid
  | FFIUInt8
  | FFIUInt16
  | FFIUInt32
  | FFIUInt64
  | FFIInt8
  | FFIInt16
  | FFIInt32
  | FFIInt64
  | FFIUInt
  | FFIInt
  | FFIULong
  | FFILong
  | FFIUSize
  | FFIFloat
  | FFIDouble
  | FFIChar8
  | FFIChar16
  | FFIChar32
  | FFIUChar
  | FFIChar
  | FFIWChar
  | FFIArrayOf FFIType
  | FFIString
  | FFIPointerTo FFIType

nounToIntegral :: forall a m. MonadError Error m => (Integral a, Bounded a) => String -> Noun -> m a
nounToIntegral name arr = do
  let err = DomainError $ "FFI " ++ name ++ " not a scalar integer or not in range"
  num <- asScalar err arr >>= asNumber err >>= asInt err
  when (num < toInteger (minBound @a)) $ throwError err
  when (num > toInteger (maxBound @a)) $ throwError err
  pure $ fromInteger num

nounToFloating :: forall a m. MonadError Error m => Fractional a => String -> Noun -> m a
nounToFloating name arr = do
  let err = DomainError $ "FFI " ++ name ++ " not a scalar real"
  num <- asScalar err arr >>= asNumber err >>= asReal err
  pure $ realToFrac num

nounToChar :: MonadError Error m => String -> Noun -> m Char
nounToChar name arr = do
  let err = DomainError $ "FFI " ++ name ++ " not a scalar character"
  asScalar err arr >>= asCharacter err

ffiArgIntegral :: forall a m. MonadError Error m => (Integral a, Bounded a) => String -> (a -> FFI.Arg) -> Noun -> m (FFI.Arg, m ())
ffiArgIntegral name makeArg arr = do
  num <- nounToIntegral name arr
  pure (makeArg num, pure ())

ffiArgFloating :: forall a m. MonadError Error m => Fractional a => String -> (a -> FFI.Arg) -> Noun -> m (FFI.Arg, m ())
ffiArgFloating name makeArg arr = do
  num <- nounToFloating name arr
  pure (makeArg num, pure ())

ffiArgChar :: forall a m. MonadError Error m => (Integral a, Bounded a) => String -> (a -> FFI.Arg) -> Noun -> m (FFI.Arg, m ())
ffiArgChar name makeArg arr = do
  num <- nounToChar name arr
  ffiArgIntegral @a name makeArg $ scalar $ Number $ (:+ 0) $ fromIntegral $ fromEnum num

ffiArg :: FFIType -> Noun -> St (FFI.Arg, St ())
ffiArg FFIVoid _ = throwError $ DomainError "FFI void argument"
ffiArg FFIUInt8 arr = ffiArgIntegral "uint8" FFI.argWord8 arr
ffiArg FFIUInt16 arr = ffiArgIntegral "uint16" FFI.argWord16 arr
ffiArg FFIUInt32 arr = ffiArgIntegral "uint32" FFI.argWord32 arr
ffiArg FFIUInt64 arr = ffiArgIntegral "uint64" FFI.argWord64 arr
ffiArg FFIInt8 arr = ffiArgIntegral "int8" FFI.argInt8 arr
ffiArg FFIInt16 arr = ffiArgIntegral "int16" FFI.argInt16 arr
ffiArg FFIInt32 arr = ffiArgIntegral "int32" FFI.argInt32 arr
ffiArg FFIInt64 arr = ffiArgIntegral "int64" FFI.argInt64 arr
ffiArg FFIUInt arr = ffiArgIntegral "uint" FFI.argCUInt arr
ffiArg FFIInt arr = ffiArgIntegral "int" FFI.argCInt arr
ffiArg FFIULong arr = ffiArgIntegral "long" FFI.argCULong arr
ffiArg FFILong arr = ffiArgIntegral "ulong" FFI.argCLong arr
ffiArg FFIUSize arr = ffiArgIntegral "usize" FFI.argCSize arr
ffiArg FFIFloat arr = ffiArgFloating "float" FFI.argCFloat arr
ffiArg FFIDouble arr = ffiArgFloating "double" FFI.argCDouble arr
ffiArg FFIChar8 arr = ffiArgChar "char8" FFI.argWord8 arr
ffiArg FFIChar16 arr = ffiArgChar "char16" FFI.argWord16 arr
ffiArg FFIChar32 arr = ffiArgChar "char32" FFI.argWord32 arr
ffiArg FFIUChar arr = ffiArgChar "uchar" FFI.argCUChar arr
ffiArg FFIChar arr = ffiArgChar "char" FFI.argCChar arr
ffiArg FFIWChar arr = ffiArgChar "wchar" FFI.argCWchar arr
ffiArg (FFIArrayOf typ) arr = do
  (ptr, cleanup) <- ffiNewArray typ arr
  pure (FFI.argPtr ptr, cleanup)
ffiArg FFIString arr = do
  str <- asString (DomainError "FFI string not a string") arr
  pure (FFI.argString str, pure ())
ffiArg (FFIPointerTo _) arr = do
  struct <- asScalar (DomainError "FFI pointer not a pointer object") arr
  ptr <- pointerGet struct
  pure (FFI.argPtr ptr, pure ())

ffiPeekIntegral :: forall a m p. (MonadError Error m, MonadIO m) => (Integral a, Storable a) => Ptr p -> m Noun
ffiPeekIntegral ptr = do
  value <- liftIO $ peek $ castPtr @p @a ptr
  pure $ scalar $ Number $ (:+ 0) $ fromIntegral value

ffiPeekFloating :: forall a m p. (MonadError Error m, MonadIO m) => (Real a, Storable a) => Ptr p -> m Noun
ffiPeekFloating ptr = do
  value <- liftIO $ peek $ castPtr @p @a ptr
  pure $ scalar $ Number $ (:+ 0) $ realToFrac value

ffiPeekChar :: forall a m p. (MonadError Error m, MonadIO m) => (Integral a, Storable a) => Ptr p -> m Noun
ffiPeekChar ptr = do
  value <- liftIO $ peek $ castPtr @p @a ptr
  pure $ scalar $ Character $ toEnum $ fromIntegral value

ffiPeek :: FFIType -> Ptr p -> St Noun
ffiPeek FFIVoid _ = throwError $ DomainError "FFI peek void"
ffiPeek FFIUInt8 ptr = ffiPeekIntegral @Word8 ptr
ffiPeek FFIUInt16 ptr = ffiPeekIntegral @Word16 ptr
ffiPeek FFIUInt32 ptr = ffiPeekIntegral @Word32 ptr
ffiPeek FFIUInt64 ptr = ffiPeekIntegral @Word64 ptr
ffiPeek FFIInt8 ptr = ffiPeekIntegral @Int8 ptr
ffiPeek FFIInt16 ptr = ffiPeekIntegral @Int16 ptr
ffiPeek FFIInt32 ptr = ffiPeekIntegral @Int32 ptr
ffiPeek FFIInt64 ptr = ffiPeekIntegral @Int64 ptr
ffiPeek FFIUInt ptr = ffiPeekIntegral @CUInt ptr
ffiPeek FFIInt ptr = ffiPeekIntegral @CInt ptr
ffiPeek FFIULong ptr = ffiPeekIntegral @CULong ptr
ffiPeek FFILong ptr = ffiPeekIntegral @CLong ptr
ffiPeek FFIUSize ptr = ffiPeekIntegral @CSize ptr
ffiPeek FFIFloat ptr = ffiPeekFloating @CFloat ptr
ffiPeek FFIDouble ptr = ffiPeekFloating @CDouble ptr
ffiPeek FFIChar8 ptr = ffiPeekChar @Word8 ptr
ffiPeek FFIChar16 ptr = ffiPeekChar @Word16 ptr
ffiPeek FFIChar32 ptr = ffiPeekChar @Word32 ptr
ffiPeek FFIUChar ptr = ffiPeekChar @CUChar ptr
ffiPeek FFIChar ptr = ffiPeekChar @CChar ptr
ffiPeek FFIWChar ptr = ffiPeekChar @CWchar ptr
ffiPeek (FFIArrayOf _) _ = throwError $ DomainError "FFI peek array"
ffiPeek FFIString ptr = do
  str <- liftIO $ peekCString $ castPtr ptr
  pure $ vector $ Character <$> str
ffiPeek (FFIPointerTo typ) ptr = do
  ptrPtr <- liftIO $ peek $ castPtr ptr
  pointerObj <- pointer typ ptrPtr
  pure $ scalar pointerObj

ffiPokeIntegral :: forall a m p. (MonadError Error m, MonadIO m) => (Integral a, Bounded a, Storable a) => String -> Ptr p -> Noun -> m ()
ffiPokeIntegral name ptr arr = do
  num <- nounToIntegral @a name arr
  liftIO $ poke (castPtr ptr) num

ffiPokeFloating :: forall a m p. (MonadError Error m, MonadIO m) => (Floating a, Storable a) => String -> Ptr p -> Noun -> m ()
ffiPokeFloating name ptr arr = do
  num <- nounToFloating @a name arr
  liftIO $ poke (castPtr ptr) num

ffiPokeChar :: forall a m p. (MonadError Error m, MonadIO m) => (Integral a, Bounded a, Storable a) => String -> Ptr p -> Noun -> m ()
ffiPokeChar name ptr arr = do
  ch <- nounToChar name arr
  ffiPokeIntegral @a name ptr $ scalar $ Number $ (:+ 0) $ fromIntegral $ fromEnum ch

ffiPoke :: FFIType -> Ptr p -> Noun -> St ()
ffiPoke FFIVoid _ _ = throwError $ DomainError "FFI peek void"
ffiPoke FFIUInt8 ptr arr = ffiPokeIntegral @Word8 "uint8" ptr arr
ffiPoke FFIUInt16 ptr arr = ffiPokeIntegral @Word16 "uint16" ptr arr
ffiPoke FFIUInt32 ptr arr = ffiPokeIntegral @Word32 "uint32" ptr arr
ffiPoke FFIUInt64 ptr arr = ffiPokeIntegral @Word64 "uint64" ptr arr
ffiPoke FFIInt8 ptr arr = ffiPokeIntegral @Int8 "int8" ptr arr
ffiPoke FFIInt16 ptr arr = ffiPokeIntegral @Int16 "int16" ptr arr
ffiPoke FFIInt32 ptr arr = ffiPokeIntegral @Int32 "int32" ptr arr
ffiPoke FFIInt64 ptr arr = ffiPokeIntegral @Int64 "int64" ptr arr
ffiPoke FFIUInt ptr arr = ffiPokeIntegral @CUInt "uint" ptr arr
ffiPoke FFIInt ptr arr = ffiPokeIntegral @CInt "int" ptr arr
ffiPoke FFIULong ptr arr = ffiPokeIntegral @CULong "ulong" ptr arr
ffiPoke FFILong ptr arr = ffiPokeIntegral @CLong "long" ptr arr
ffiPoke FFIUSize ptr arr = ffiPokeIntegral @CSize "usize" ptr arr
ffiPoke FFIFloat ptr arr = ffiPokeFloating @CFloat "float" ptr arr
ffiPoke FFIDouble ptr arr = ffiPokeFloating @CDouble "double" ptr arr
ffiPoke FFIChar8 ptr arr = ffiPokeChar @Word8 "char8" ptr arr
ffiPoke FFIChar16 ptr arr = ffiPokeChar @Word16 "char16" ptr arr
ffiPoke FFIChar32 ptr arr = ffiPokeChar @Word32 "char32" ptr arr
ffiPoke FFIUChar ptr arr = ffiPokeChar @CUChar "uchar" ptr arr
ffiPoke FFIChar ptr arr = ffiPokeChar @CChar "char" ptr arr
ffiPoke FFIWChar ptr arr = ffiPokeChar @CWchar "wchar" ptr arr
ffiPoke (FFIArrayOf _) _ _ = throwError $ DomainError "FFI peek array"
ffiPoke FFIString ptr arr = do
  str <- asString (DomainError "Poke string not a string") arr
  liftIO $ pokeArray0 (0 :: CChar) (castPtr ptr) $ toEnum . fromEnum <$> str
ffiPoke (FFIPointerTo _) ptr arr = do
  pointerObj <- asScalar (DomainError "Poke pointer not a pointer object") arr
  ptrPtr <- pointerGet pointerObj
  liftIO $ poke (castPtr ptr) ptrPtr

ffiNewArrayIntegral :: forall a m p. (MonadError Error m, MonadIO m) => (Integral a, Bounded a, Storable a) => String -> Noun -> m (Ptr p, m ())
ffiNewArrayIntegral name arr = do
  let err = DomainError "FFI array must be vector"
  vec <- fmap fromScalar <$> asVector err arr
  nums <- mapM (nounToIntegral @a name) vec
  ptr <- liftIO $ castPtr <$> newArray nums
  pure (ptr, liftIO (free ptr))

ffiNewArrayFloating :: forall a m p. (MonadError Error m, MonadIO m) => (Floating a, Storable a) => String -> Noun -> m (Ptr p, m ())
ffiNewArrayFloating name arr = do
  let err = DomainError "FFI array must be vector"
  vec <- fmap fromScalar <$> asVector err arr
  nums <- mapM (nounToFloating @a name) vec
  ptr <- liftIO $ castPtr <$> newArray nums
  pure (ptr, liftIO (free ptr))

ffiNewArrayChar :: forall a m p. (MonadError Error m, MonadIO m) => (Integral a, Bounded a, Storable a) => String -> Noun -> m (Ptr p, m ())
ffiNewArrayChar name arr = do
  let err = DomainError "FFI char array must be string"
  str <- asString err arr
  ffiNewArrayIntegral @a name $ vector $ (Number . (:+ 0) . fromIntegral . fromEnum) <$> str

ffiNewArray :: FFIType -> Noun -> St (Ptr p, St ())
ffiNewArray FFIVoid _ = throwError $ DomainError "FFI void array"
ffiNewArray FFIUInt8 arr = ffiNewArrayIntegral @Word8 "uint8" arr
ffiNewArray FFIUInt16 arr = ffiNewArrayIntegral @Word16 "uint16" arr
ffiNewArray FFIUInt32 arr = ffiNewArrayIntegral @Word32 "uint32" arr
ffiNewArray FFIUInt64 arr = ffiNewArrayIntegral @Word64 "uint64" arr
ffiNewArray FFIInt8 arr = ffiNewArrayIntegral @Int8 "int8" arr
ffiNewArray FFIInt16 arr = ffiNewArrayIntegral @Int16 "int16" arr
ffiNewArray FFIInt32 arr = ffiNewArrayIntegral @Int32 "int32" arr
ffiNewArray FFIInt64 arr = ffiNewArrayIntegral @Int64 "int64" arr
ffiNewArray FFIUInt arr = ffiNewArrayIntegral @CUInt "uint" arr
ffiNewArray FFIInt arr = ffiNewArrayIntegral @CInt "int" arr
ffiNewArray FFIULong arr = ffiNewArrayIntegral @CULong "ulong" arr
ffiNewArray FFILong arr = ffiNewArrayIntegral @CLong "long" arr
ffiNewArray FFIUSize arr = ffiNewArrayIntegral @CSize "usize" arr
ffiNewArray FFIFloat arr = ffiNewArrayFloating @CFloat "float" arr
ffiNewArray FFIDouble arr = ffiNewArrayFloating @CDouble "double" arr
ffiNewArray FFIChar8 arr = ffiNewArrayChar @Word8 "char8" arr
ffiNewArray FFIChar16 arr = ffiNewArrayChar @Word16 "char16" arr
ffiNewArray FFIChar32 arr = ffiNewArrayChar @Word32 "char32" arr
ffiNewArray FFIUChar arr = ffiNewArrayChar @CUChar "uchar" arr
ffiNewArray FFIChar arr = ffiNewArrayChar @CChar "char" arr
ffiNewArray FFIWChar arr = ffiNewArrayChar @CWchar "wchar" arr
ffiNewArray (FFIArrayOf typ) arr = do
  let err = DomainError "FFI array must be vector"
  vec <- fmap fromScalar <$> asVector err arr
  (ptrs, cleanups) <- unzip <$> mapM (ffiNewArray typ) vec
  ptr <- liftIO $ newArray ptrs
  pure (castPtr ptr, sequence_ cleanups >> liftIO (free ptr))
ffiNewArray FFIString arr = do
  let err = DomainError "FFI string array must be vector of strings"
  strs <- asStrings err arr
  ptrs <- forM strs $ \str -> liftIO $ newCString str
  ptr <- liftIO $ newArray ptrs
  pure (castPtr ptr, mapM_ (liftIO . free) ptrs >> liftIO (free ptr))
ffiNewArray (FFIPointerTo _) arr = do
  let err = DomainError "FFI pointer array must be vector of pointer objects"
  ptrs <- asVector err arr >>= mapM pointerGet
  ptr <- liftIO $ newArray ptrs
  pure (castPtr ptr, liftIO $ free ptr)

ffiSizeStorable :: forall a. Storable a => Int
ffiSizeStorable = sizeOf (undefined :: a)

ffiSize :: FFIType -> Int
ffiSize FFIVoid = ffiSizeStorable @()
ffiSize FFIUInt8 = ffiSizeStorable @Word8
ffiSize FFIUInt16 = ffiSizeStorable @Word16
ffiSize FFIUInt32 = ffiSizeStorable @Word32
ffiSize FFIUInt64 = ffiSizeStorable @Word64
ffiSize FFIInt8 = ffiSizeStorable @Int8
ffiSize FFIInt16 = ffiSizeStorable @Int16
ffiSize FFIInt32 = ffiSizeStorable @Int32
ffiSize FFIInt64 = ffiSizeStorable @Int64
ffiSize FFIUInt = ffiSizeStorable @CUInt
ffiSize FFIInt = ffiSizeStorable @CInt
ffiSize FFIULong = ffiSizeStorable @CULong
ffiSize FFILong = ffiSizeStorable @CLong
ffiSize FFIUSize = ffiSizeStorable @CSize
ffiSize FFIFloat = ffiSizeStorable @CFloat
ffiSize FFIDouble = ffiSizeStorable @CDouble
ffiSize FFIChar8 = ffiSizeStorable @Word8
ffiSize FFIChar16 = ffiSizeStorable @Word16
ffiSize FFIChar32 = ffiSizeStorable @Word32
ffiSize FFIUChar = ffiSizeStorable @CUChar
ffiSize FFIChar = ffiSizeStorable @CChar
ffiSize FFIWChar = ffiSizeStorable @CWchar
ffiSize (FFIArrayOf _) = ffiSizeStorable @(Ptr ())
ffiSize FFIString = ffiSizeStorable @(Ptr ())
ffiSize (FFIPointerTo _) = ffiSizeStorable @(Ptr ())

ffiReturnInteger :: MonadError Error m => Integral a => FFI.RetType a -> FFI.RetType (m Noun)
ffiReturnInteger ret = (pure . scalar . Number . (:+ 0) . fromIntegral) <$> ret

ffiReturnFloating :: MonadError Error m => Real a => FFI.RetType a -> FFI.RetType (m Noun)
ffiReturnFloating ret = (pure . scalar . Number . (:+ 0) . realToFrac) <$> ret

ffiReturnChar :: MonadError Error m => Integral a => FFI.RetType a -> FFI.RetType (m Noun)
ffiReturnChar ret = (pure . scalar . Character . toEnum . fromIntegral) <$> ret

ffiReturn :: FFIType -> FFI.RetType (St Noun)
ffiReturn FFIVoid = pure (vector []) <$ FFI.retVoid
ffiReturn FFIUInt8 = ffiReturnInteger FFI.retWord8
ffiReturn FFIUInt16 = ffiReturnInteger FFI.retWord16
ffiReturn FFIUInt32 = ffiReturnInteger FFI.retWord32
ffiReturn FFIUInt64 = ffiReturnInteger FFI.retWord64
ffiReturn FFIInt8 = ffiReturnInteger FFI.retInt8
ffiReturn FFIInt16 = ffiReturnInteger FFI.retInt16
ffiReturn FFIInt32 = ffiReturnInteger FFI.retInt32
ffiReturn FFIInt64 = ffiReturnInteger FFI.retInt64
ffiReturn FFIUInt = ffiReturnInteger FFI.retCUInt
ffiReturn FFIInt = ffiReturnInteger FFI.retCInt
ffiReturn FFIULong = ffiReturnInteger FFI.retCULong
ffiReturn FFILong = ffiReturnInteger FFI.retCLong
ffiReturn FFIUSize = ffiReturnInteger FFI.retCSize
ffiReturn FFIFloat = ffiReturnFloating FFI.retCFloat
ffiReturn FFIDouble = ffiReturnFloating FFI.retCDouble
ffiReturn FFIChar8 = ffiReturnChar FFI.retWord8
ffiReturn FFIChar16 = ffiReturnChar FFI.retWord16
ffiReturn FFIChar32 = ffiReturnChar FFI.retWord32
ffiReturn FFIUChar = ffiReturnChar FFI.retCUChar
ffiReturn FFIChar = ffiReturnChar FFI.retCChar
ffiReturn FFIWChar = ffiReturnChar FFI.retCWchar
ffiReturn (FFIArrayOf _) = (const $ throwError $ DomainError "FFI return array") <$> FFI.retPtr FFI.retVoid
ffiReturn FFIString = (pure . vector . fmap Character) <$> FFI.retString
ffiReturn (FFIPointerTo typ) = (\ptr -> scalar <$> pointer typ ptr) <$> FFI.retPtr FFI.retVoid

ffiFunc :: FunPtr a -> [FFIType] -> FFIType -> [Noun] -> St Noun
ffiFunc fun params ret args = do
  unless (length params == length args) $ throwError $ LengthError "Argument length must be the same as parameter length"
  (params', cleanup') <- unzip <$> zipWithM ffiArg params args
  let ret' = ffiReturn ret
  res <- liftIO $ FFI.callFFI fun ret' params'
  sequence_ cleanup'
  res

crtPath :: FilePath
#if defined(mingw32_HOST_OS)
crtPath = "msvcrt.dll"
#elif defined(darwin_HOST_OS)
crtPath = "libc.dylib"
#else
crtPath = ""
#endif

#if defined(mingw32_HOST_OS)
type DynLib = HMODULE
#else
type DynLib = DL
#endif

openDynLib :: (MonadIO m, MonadMask m) => FilePath -> m DynLib
#if defined(MIN_VERSION_Win32)
openDynLib = liftIO . loadLibrary
#else
openDynLib file = liftIO $ dlopen file [RTLD_NOW]
#endif

dynLibSym :: MonadIO m => DynLib -> String -> m (FunPtr a)
#if defined(MIN_VERSION_Win32)
dynLibSym lib sym = liftIO $ castPtrToFunPtr <$> getProcAddress lib sym
#else
dynLibSym lib sym = liftIO $ dlsym lib sym
#endif

parseFFI :: String -> Maybe FFIType
parseFFI str | "[]" `isPrefixOf` str = FFIArrayOf <$> parseFFI (drop 2 str)
parseFFI str | "*" `isPrefixOf` str = FFIPointerTo <$> parseFFI (drop 1 str)
parseFFI "void" = pure FFIVoid
parseFFI "u8" = pure FFIUInt8
parseFFI "u16" = pure FFIUInt16
parseFFI "u32" = pure FFIUInt32
parseFFI "u64" = pure FFIUInt64
parseFFI "i8" = pure FFIInt8
parseFFI "i16" = pure FFIInt16
parseFFI "i32" = pure FFIInt32
parseFFI "i64" = pure FFIInt64
parseFFI "uint" = pure FFIUInt
parseFFI "int" = pure FFIInt
parseFFI "ulong" = pure FFIULong
parseFFI "long" = pure FFILong
parseFFI "usize" = pure FFIUSize
parseFFI "f32" = pure FFIFloat
parseFFI "f64" = pure FFIDouble
parseFFI "c8" = pure FFIChar8
parseFFI "c16" = pure FFIChar16
parseFFI "c32" = pure FFIChar32
parseFFI "uchar" = pure FFIUChar
parseFFI "char" = pure FFIChar
parseFFI "wchar" = pure FFIWChar
parseFFI "str" = pure FFIString
parseFFI _ = Nothing

doFFI :: Maybe String -> Noun -> St Noun
doFFI libName args' = do
  lib <- openDynLib $ fromMaybe crtPath libName
  args <- asStrings (DomainError "FFI right argument must be a vector of strings") args'
  when (length args < 2) $ throwError $ LengthError "FFI right argument must be at least length 2"
  let (ret : name : params) = args
  sym <- dynLibSym lib name
  ret' <- case parseFFI ret of
    Just typ -> pure typ
    Nothing -> throwError $ DomainError "Invalid return type"
  params' <- forM params $ \param -> case parseFFI param of
    Just typ -> pure typ
    Nothing -> throwError $ DomainError "Invalid parameter type"
  pure $ scalar $ Wrap $ PrimitiveFunction (Just $ const $ \y -> do
    args <- fmap fromScalar <$> asVector (DomainError "FFI arguments must be a vector") y
    ffiFunc sym params' ret' args) Nothing (case libName of
    Just path -> "<" ++ ret ++ " " ++ path ++ ":" ++ name ++ "(" ++ intercalate ", " params ++ ")>"
    Nothing -> "<" ++ ret ++ " " ++ name ++ "(" ++ intercalate ", " params ++ ")>") Nothing

ffi :: Function
ffi = PrimitiveFunction (Just $ const $ doFFI Nothing) (Just $ const $ \x y -> do
  name <- asString (DomainError "FFI left argument must be a string") x
  doFFI (Just name) y) (G.quad : "FFI") Nothing

pointer :: FFIType -> Ptr a -> St ScalarValue
pointer typ ptr = do
  let peek = ffiPeek typ
  let poke = ffiPoke typ
  scope <- createRef $ Scope
    [ ("address", (VariableConstant, scalar $ Number $ (:+ 0) $ fromIntegral $ ptrToWordPtr ptr))
    , ("size", (VariableConstant, scalar $ Number $ (:+ 0) $ fromIntegral $ ffiSize typ))
    , (G.delta : "show", (VariableConstant, vector $ Character <$> ("<ptr 0x" ++ showHex (ptrToWordPtr ptr) "" ++ ">"))) ]
    [ ("Peek", (VariableConstant, PrimitiveFunction (Just $ const $ \y -> do
      let err = DomainError "Pointer peek offset must be an integer"
      offset <- asScalar err y >>= asNumber err >>= asInt err
      peek $ plusPtr ptr offset) Nothing "Peek" Nothing))
    , ("Poke", (VariableConstant, PrimitiveFunction (Just $ const $ \y -> poke ptr y $> vector []) (Just $ const $ \x y -> do
      let err = DomainError "Pointer poke offset must be an integer"
      offset <- asScalar err x >>= asNumber err >>= asInt err
      poke (plusPtr ptr offset) y $> vector []) "Poke" Nothing))
    , ("Cast", (VariableConstant, PrimitiveFunction (Just $ const $ \y -> do
      typ <- asString (DomainError "Pointer cast argument must be a string") y
      case parseFFI typ of
        Nothing -> throwError $ DomainError "Invalid cast target type"
        Just ffiTyp -> scalar <$> pointer ffiTyp ptr) Nothing "Cast" Nothing))
    , (G.deltaBar : "Add", (VariableConstant, PrimitiveFunction Nothing (Just $ const $ \cases
      (Array [] [pointerObj@(Struct _)]) (Array [] [Number off]) -> do
        offset <- asInt (DomainError "Pointer add offset must be an integer") off
        p <- pointerGet pointerObj
        scalar <$> pointer typ (plusPtr p offset)
      (Array [] [Number off]) (Array [] [pointerObj@(Struct _)]) -> do
        offset <- asInt (DomainError "Pointer add offset must be an integer") off
        p <- pointerGet pointerObj
        scalar <$> pointer typ (plusPtr p offset)
      _ _ -> throwError $ DomainError "Invalid arguments to pointer add") (G.deltaBar : "Add") Nothing)) 
    , (G.deltaBar : "Subtract", (VariableConstant, PrimitiveFunction Nothing (Just $ const $ \cases
      (Array [] [pointerObj@(Struct _)]) (Array [] [Number off]) -> do
        offset <- asInt (DomainError "Pointer subtract offset must be an integer") off
        p <- pointerGet pointerObj
        scalar <$> pointer typ (plusPtr p $ negate offset)
      (Array [] [leftObj@(Struct _)]) (Array [] [rightObj@(Struct _)]) -> do
        left <- pointerGet leftObj
        right <- pointerGet rightObj
        pure $ scalar $ Number $ (:+ 0) $ fromIntegral $ right `minusPtr` left
      _ _ -> throwError $ DomainError "Invalid arguments to pointer subtract") (G.deltaBar : "Subtract") Nothing))
    , (G.deltaBar : "Increment", (VariableConstant, PrimitiveFunction (Just $ const $ \y -> do
      sc <- asScalar undefined y
      p <- pointerGet sc
      scalar <$> pointer typ (plusPtr p 1)) Nothing (G.deltaBar : "Increment") Nothing))
    , (G.deltaBar : "Decrement", (VariableConstant, PrimitiveFunction (Just $ const $ \y -> do
      sc <- asScalar undefined y
      p <- pointerGet sc
      scalar <$> pointer typ (plusPtr p $ -1)) Nothing (G.deltaBar : "Decrement") Nothing)) ]
    [] [] Nothing
  ctx <- getContext
  pure $ Struct $ ctx{ contextScope = scope }

pointerGet :: ScalarValue -> St (Ptr a)
pointerGet (Struct ctx) = do
  scope <- readRef $ contextScope ctx
  address <- scopeLookupNoun False "address" scope
  case address of
    Just (Array [] [Number (addr :+ 0)]) -> pure $ wordPtrToPtr $ fromInteger $ floor addr
    _ -> throwError $ DomainError "address of non-pointer"
pointerGet _ = throwError $ DomainError "address of non-pointer"

nullPointer :: Nilad
nullPointer = Nilad (Just $ scalar <$> pointer FFIUInt8 nullPtr) Nothing (G.quad : "nullPtr") Nothing
#endif
