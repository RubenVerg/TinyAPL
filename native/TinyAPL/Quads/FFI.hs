{-# LANGUAGE CPP, FlexibleContexts, RankNTypes, TypeApplications, ScopedTypeVariables, AllowAmbiguousTypes, TupleSections, LambdaCase #-}

module TinyAPL.Quads.FFI where

#ifndef wasm32_HOST_ARCH

import TinyAPL.Noun
import TinyAPL.Function
import TinyAPL.Context
import TinyAPL.Quads
import TinyAPL.Error
import qualified TinyAPL.Glyphs as G

import Foreign
import Foreign.C
import qualified Foreign.LibFFI as FFI
import qualified Foreign.LibFFI.Base as FFI
import qualified Foreign.LibFFI.Internal as FFI
import qualified Foreign.LibFFI.FFITypes as FFI
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class
import Control.Monad
import TinyAPL.Complex
import Control.Monad.Catch
import Data.List
import Data.Maybe
import Data.Bifunctor
import qualified Data.Complex as Cx
import Numeric (showHex)
import Data.Functor hiding (unzip)
import System.IO.Unsafe
#if defined(MIN_VERSION_Win32)
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
  | FFIComplexFloat
  | FFIComplexDouble
  | FFIChar8
  | FFIChar16
  | FFIChar32
  | FFIUChar
  | FFIChar
  | FFIWChar
  | FFIArrayOf FFIType
  | FFIString
  | FFIPointerTo FFIType
  | FFIStructOf [FFIType] (Ptr FFI.CType) (St ()) (Int, Int) [Int] Noun

ffiCType :: FFIType -> Ptr FFI.CType
ffiCType FFIVoid = FFI.ffi_type_void
ffiCType FFIUInt8 = FFI.ffi_type_uint8
ffiCType FFIUInt16 = FFI.ffi_type_uint16
ffiCType FFIUInt32 = FFI.ffi_type_uint32
ffiCType FFIUInt64 = FFI.ffi_type_uint64
ffiCType FFIInt8 = FFI.ffi_type_sint8
ffiCType FFIInt16 = FFI.ffi_type_sint16
ffiCType FFIInt32 = FFI.ffi_type_sint32
ffiCType FFIInt64 = FFI.ffi_type_sint64
ffiCType FFIUInt = FFI.ffi_type_uint
ffiCType FFIInt = FFI.ffi_type_sint
ffiCType FFIULong = FFI.ffi_type_ulong
ffiCType FFILong = FFI.ffi_type_slong
ffiCType FFIUSize = FFI.ffi_type_size
ffiCType FFIFloat = FFI.ffi_type_float
ffiCType FFIDouble = FFI.ffi_type_double
ffiCType FFIComplexFloat = FFI.ffi_type_complex_float
ffiCType FFIComplexDouble = FFI.ffi_type_complex_double
ffiCType FFIChar8 = FFI.ffi_type_uint8
ffiCType FFIChar16 = FFI.ffi_type_uint16
ffiCType FFIChar32 = FFI.ffi_type_uint32
ffiCType FFIUChar = FFI.ffi_type_uchar
ffiCType FFIChar = FFI.ffi_type_schar
ffiCType FFIWChar = FFI.ffi_type_wchar
ffiCType (FFIArrayOf _) = FFI.ffi_type_pointer
ffiCType FFIString = FFI.ffi_type_pointer
ffiCType (FFIPointerTo _) = FFI.ffi_type_pointer
ffiCType (FFIStructOf _ t _ _ _ _) = t

structType :: [FFIType] -> St (Ptr FFI.CType, St (), (Int, Int), [Int], Noun)
structType types = do
  (t, cleanup) <- liftIO $ FFI.newStructCType $ ffiCType <$> types
  (size, alignment) <- liftIO $ FFI.sizeAndAlignmentOfCType t
  offsets <- liftIO $ FFI.structOffsetsOfCType (length types) t
  zero <- bracket (liftIO $ callocBytes size) (liftIO . free) (ffiPeek $ FFIStructOf types t (liftIO cleanup) (size, alignment) offsets undefined)
  pure (t, liftIO cleanup, (size, alignment), offsets, zero)

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

nounToComplex :: forall a m. MonadError Error m => Fractional a => String -> Noun -> m (Cx.Complex a)
nounToComplex name arr = do
  let err = DomainError $ "FFI " ++ name ++ " not a scalar number"
  num <- asScalar err arr >>= asNumber err
  pure $ realToFrac <$> toStd num

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

ffiArgComplex :: forall a m. MonadError Error m => Fractional a => String -> (Cx.Complex a -> FFI.Arg) -> Noun -> m (FFI.Arg, m ())
ffiArgComplex name makeArg arr = do
  num <- nounToComplex name arr
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
ffiArg FFIComplexFloat arr = ffiArgComplex "complex float" FFI.argComplexCFloat arr
ffiArg FFIComplexDouble arr = ffiArgComplex "complex double" FFI.argComplexCDouble arr
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
ffiArg (FFIStructOf typs cTyp _ (size, alignment) offsets _) arr = do
  let len = length typs
  let err = DomainError "FFI struct not a vector of the correct length"
  vec <- map fromScalar <$> asVector err arr
  when (length vec /= len) $ throwError err
  (temps, cleanup) <- unzip <$> zipWithM ffiNew typs vec
  pure (FFI.Arg $ \withArg -> do  
    allocaBytesAligned size alignment $ \ptr -> do
      mapM_ (\(off, temp, typ) -> copyBytes (ptr `plusPtr` off) temp (ffiSize typ)) $ zip3 offsets temps typs
      withArg cTyp $ castPtr ptr
    undefined, sequence_ cleanup)

ffiPeekIntegral :: forall a m p. (MonadError Error m, MonadIO m) => (Integral a, Storable a) => Ptr p -> m Noun
ffiPeekIntegral ptr = do
  value <- liftIO $ peek $ castPtr @p @a ptr
  pure $ scalar $ Number $ (:+ 0) $ fromIntegral value

ffiPeekFloating :: forall a m p. (MonadError Error m, MonadIO m) => (Real a, Storable a) => Ptr p -> m Noun
ffiPeekFloating ptr = do
  value <- liftIO $ peek $ castPtr @p @a ptr
  pure $ scalar $ Number $ (:+ 0) $ realToFrac value

ffiPeekComplex :: forall a m p. (MonadError Error m, MonadIO m) => (Real a, Storable a) => Ptr p -> m Noun
ffiPeekComplex ptr = do
  value <- liftIO $ peek $ castPtr @p @(Cx.Complex a) ptr
  pure $ scalar $ Number $ realToFrac <$> fromStd value

ffiPeekChar :: forall a m p. (MonadError Error m, MonadIO m) => (Integral a, Storable a) => Ptr p -> m Noun
ffiPeekChar ptr = do
  value <- liftIO $ peek $ castPtr @p @a ptr
  pure $ scalar $ Character $ toEnum $ fromIntegral value

peekCStringSafe :: CString -> IO String
peekCStringSafe ptr = if ptr == nullPtr then pure "" else peekCString ptr

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
ffiPeek FFIComplexFloat ptr = ffiPeekComplex @CFloat ptr
ffiPeek FFIComplexDouble ptr = ffiPeekComplex @CDouble ptr
ffiPeek FFIChar8 ptr = ffiPeekChar @Word8 ptr
ffiPeek FFIChar16 ptr = ffiPeekChar @Word16 ptr
ffiPeek FFIChar32 ptr = ffiPeekChar @Word32 ptr
ffiPeek FFIUChar ptr = ffiPeekChar @CUChar ptr
ffiPeek FFIChar ptr = ffiPeekChar @CChar ptr
ffiPeek FFIWChar ptr = ffiPeekChar @CWchar ptr
ffiPeek (FFIArrayOf _) _ = throwError $ DomainError "FFI peek array"
ffiPeek FFIString ptr = do
  ptrPtr <- liftIO $ peek $ castPtr ptr
  str <- liftIO $ peekCStringSafe ptrPtr
  pure $ vector $ Character <$> str
ffiPeek (FFIPointerTo typ) ptr = do
  ptrPtr <- liftIO $ peek $ castPtr ptr
  pointerObj <- pointer typ ptrPtr
  pure $ scalar pointerObj
ffiPeek (FFIStructOf typs _ _ _ offsets _) ptr = vector . map box <$> zipWithM (\off typ -> ffiPeek typ $ ptr `plusPtr` off) offsets typs

ffiPeekArrayLenIntegral :: forall a m p. (MonadError Error m, MonadIO m) => (Integral a, Storable a) => Ptr p -> Int -> m Noun
ffiPeekArrayLenIntegral ptr len = do
  values <- liftIO $ peekArray len $ castPtr @p @a ptr
  pure $ vector $ Number . (:+ 0) . fromIntegral <$> values

ffiPeekArrayLenFloating :: forall a m p. (MonadError Error m, MonadIO m) => (Real a, Storable a) => Ptr p -> Int -> m Noun
ffiPeekArrayLenFloating ptr len = do
  values <- liftIO $ peekArray len $ castPtr @p @a ptr
  pure $ vector $ Number . (:+ 0) . realToFrac <$> values

ffiPeekArrayLenComplex :: forall a m p. (MonadError Error m, MonadIO m) => (Real a, Storable a) => Ptr p -> Int -> m Noun
ffiPeekArrayLenComplex ptr len = do
  values <- liftIO $ peekArray len $ castPtr @p @(Cx.Complex a) ptr
  pure $ vector $ Number . fmap realToFrac . fromStd <$> values

ffiPeekArrayLenChar :: forall a m p. (MonadError Error m, MonadIO m) => (Integral a, Storable a) => Ptr p -> Int -> m Noun
ffiPeekArrayLenChar ptr len = do
  values <- liftIO $ peekArray len $ castPtr @p @a ptr
  pure $ vector $ Character . toEnum . fromIntegral <$> values

ffiPeekArrayLen :: FFIType -> Ptr p -> Int -> St Noun
ffiPeekArrayLen FFIVoid _ _ = throwError $ DomainError "FFI peek void"
ffiPeekArrayLen FFIUInt8 ptr len = ffiPeekArrayLenIntegral @Word8 ptr len
ffiPeekArrayLen FFIUInt16 ptr len = ffiPeekArrayLenIntegral @Word16 ptr len
ffiPeekArrayLen FFIUInt32 ptr len = ffiPeekArrayLenIntegral @Word32 ptr len
ffiPeekArrayLen FFIUInt64 ptr len = ffiPeekArrayLenIntegral @Word64 ptr len
ffiPeekArrayLen FFIInt8 ptr len = ffiPeekArrayLenIntegral @Int8 ptr len
ffiPeekArrayLen FFIInt16 ptr len = ffiPeekArrayLenIntegral @Int16 ptr len
ffiPeekArrayLen FFIInt32 ptr len = ffiPeekArrayLenIntegral @Int32 ptr len
ffiPeekArrayLen FFIInt64 ptr len = ffiPeekArrayLenIntegral @Int64 ptr len
ffiPeekArrayLen FFIUInt ptr len = ffiPeekArrayLenIntegral @CUInt ptr len
ffiPeekArrayLen FFIInt ptr len = ffiPeekArrayLenIntegral @CInt ptr len
ffiPeekArrayLen FFIULong ptr len = ffiPeekArrayLenIntegral @CULong ptr len
ffiPeekArrayLen FFILong ptr len = ffiPeekArrayLenIntegral @CLong ptr len
ffiPeekArrayLen FFIUSize ptr len = ffiPeekArrayLenIntegral @CSize ptr len
ffiPeekArrayLen FFIFloat ptr len = ffiPeekArrayLenFloating @CFloat ptr len
ffiPeekArrayLen FFIDouble ptr len = ffiPeekArrayLenFloating @CDouble ptr len
ffiPeekArrayLen FFIComplexFloat ptr len = ffiPeekArrayLenComplex @CFloat ptr len
ffiPeekArrayLen FFIComplexDouble ptr len = ffiPeekArrayLenComplex @CDouble ptr len
ffiPeekArrayLen FFIChar8 ptr len = ffiPeekArrayLenChar @Word8 ptr len
ffiPeekArrayLen FFIChar16 ptr len = ffiPeekArrayLenChar @Word16 ptr len
ffiPeekArrayLen FFIChar32 ptr len = ffiPeekArrayLenChar @Word32 ptr len
ffiPeekArrayLen FFIUChar ptr len = ffiPeekArrayLenChar @CUChar ptr len
ffiPeekArrayLen FFIChar ptr len = ffiPeekArrayLenChar @CChar ptr len
ffiPeekArrayLen FFIWChar ptr len = ffiPeekArrayLenChar @CWchar ptr len
ffiPeekArrayLen (FFIArrayOf _) _ _ = throwError $ DomainError "FFI peek array"
ffiPeekArrayLen FFIString ptr len = do
  ptrPtrs <- liftIO $ peekArray len $ castPtr ptr
  strs <- mapM (liftIO . peekCStringSafe) ptrPtrs
  pure $ vector $ box . vector . fmap Character <$> strs
ffiPeekArrayLen (FFIPointerTo typ) ptr len = do
  ptrPtrs <- liftIO $ peekArray len $ castPtr ptr
  pointerObjs <- mapM (pointer typ) ptrPtrs
  pure $ vector pointerObjs
ffiPeekArrayLen typ@(FFIStructOf _ _ _ (size, _) _ _) ptr len = fmap vector $ forM [0..(len - 1)] $ \idx -> fmap box $ ffiPeek typ $ ptr `plusPtr` (size * idx)

ffiPeekArrayEndIntegral :: forall a m p. (MonadError Error m, MonadIO m) => (Integral a, Bounded a, Storable a) => String -> Ptr p -> Noun -> m Noun
ffiPeekArrayEndIntegral name ptr end = do
  eNum <- nounToIntegral @a name end
  values <- liftIO $ peekArray0 eNum $ castPtr @p @a ptr
  pure $ vector $ Number . (:+ 0) . fromIntegral <$> values

ffiPeekArrayEndFloating :: forall a m p. (MonadError Error m, MonadIO m) => (Fractional a, Real a, Storable a) => String -> Ptr p -> Noun -> m Noun
ffiPeekArrayEndFloating name ptr end = do
  eNum <- nounToFloating @a name end
  values <- liftIO $ peekArray0 eNum $ castPtr @p @a ptr
  pure $ vector $ Number . (:+ 0) . realToFrac <$> values

ffiPeekArrayEndComplex :: forall a m p. (MonadError Error m, MonadIO m) => (Fractional a, Real a, Storable a) => String -> Ptr p -> Noun -> m Noun
ffiPeekArrayEndComplex name ptr end = do
  eNum <- nounToComplex @a name end
  values <- liftIO $ peekArray0 eNum $ castPtr @p @(Cx.Complex a) ptr
  pure $ vector $ Number . fmap realToFrac . fromStd <$> values

ffiPeekArrayEndChar :: forall a m p. (MonadError Error m, MonadIO m) => (Integral a, Storable a) => String -> Ptr p -> Noun -> m Noun
ffiPeekArrayEndChar name ptr end = do
  eChar <- nounToChar name end
  let eNum = fromIntegral $ fromEnum eChar
  values <- liftIO $ peekArray0 eNum $ castPtr @p @a ptr
  pure $ vector $ Character . toEnum . fromIntegral <$> values

ffiPeekArrayEnd :: FFIType -> Ptr p -> Noun -> St Noun
ffiPeekArrayEnd FFIVoid _ _ = throwError $ DomainError "FFI peek void"
ffiPeekArrayEnd FFIUInt8 ptr end = ffiPeekArrayEndIntegral @Word8 "uint8" ptr end
ffiPeekArrayEnd FFIUInt16 ptr end = ffiPeekArrayEndIntegral @Word16 "uint16" ptr end
ffiPeekArrayEnd FFIUInt32 ptr end = ffiPeekArrayEndIntegral @Word32 "uint32" ptr end
ffiPeekArrayEnd FFIUInt64 ptr end = ffiPeekArrayEndIntegral @Word64 "uint64" ptr end
ffiPeekArrayEnd FFIInt8 ptr end = ffiPeekArrayEndIntegral @Int8 "int8" ptr end
ffiPeekArrayEnd FFIInt16 ptr end = ffiPeekArrayEndIntegral @Int16 "int16" ptr end
ffiPeekArrayEnd FFIInt32 ptr end = ffiPeekArrayEndIntegral @Int32 "int32" ptr end
ffiPeekArrayEnd FFIInt64 ptr end = ffiPeekArrayEndIntegral @Int64 "int64" ptr end
ffiPeekArrayEnd FFIUInt ptr end = ffiPeekArrayEndIntegral @CUInt "uint" ptr end
ffiPeekArrayEnd FFIInt ptr end = ffiPeekArrayEndIntegral @CInt "int" ptr end
ffiPeekArrayEnd FFIULong ptr end = ffiPeekArrayEndIntegral @CULong "ulong" ptr end
ffiPeekArrayEnd FFILong ptr end = ffiPeekArrayEndIntegral @CLong "long" ptr end
ffiPeekArrayEnd FFIUSize ptr end = ffiPeekArrayEndIntegral @CSize "usize" ptr end
ffiPeekArrayEnd FFIFloat ptr end = ffiPeekArrayEndFloating @CFloat "float" ptr end
ffiPeekArrayEnd FFIDouble ptr end = ffiPeekArrayEndFloating @CDouble "double" ptr end
ffiPeekArrayEnd FFIComplexFloat ptr end = ffiPeekArrayEndComplex @CFloat "complex float" ptr end
ffiPeekArrayEnd FFIComplexDouble ptr end = ffiPeekArrayEndComplex @CDouble "complex double" ptr end
ffiPeekArrayEnd FFIChar8 ptr end = ffiPeekArrayEndChar @Word8 "char8" ptr end
ffiPeekArrayEnd FFIChar16 ptr end = ffiPeekArrayEndChar @Word16 "char16" ptr end
ffiPeekArrayEnd FFIChar32 ptr end = ffiPeekArrayEndChar @Word32 "char32" ptr end
ffiPeekArrayEnd FFIUChar ptr end = ffiPeekArrayEndChar @CUChar "uchar" ptr end
ffiPeekArrayEnd FFIChar ptr end = ffiPeekArrayEndChar @CChar "char" ptr end
ffiPeekArrayEnd FFIWChar ptr end = ffiPeekArrayEndChar @CWchar "wchar" ptr end
ffiPeekArrayEnd (FFIArrayOf _) _ _ = throwError $ DomainError "FFI peek array"
ffiPeekArrayEnd FFIString ptr end = do
  ePtr <- asScalar (DomainError "FFI peek array string end end must be pointer object (manually allocate!)") end >>= pointerGet
  ptrPtrs <- liftIO $ peekArray0 ePtr $ castPtr ptr
  strs <- mapM (liftIO . peekCStringSafe) ptrPtrs
  pure $ vector $ box . vector . fmap Character <$> strs
ffiPeekArrayEnd (FFIPointerTo typ) ptr end = do
  ePtr <- asScalar (DomainError "FFI peek array pointer end end must be pointer object") end >>= pointerGet
  ptrPtrs <- liftIO $ peekArray0 ePtr $ castPtr ptr
  pointerObjs <- mapM (pointer typ) ptrPtrs
  pure $ vector pointerObjs
ffiPeekArrayEnd typ@(FFIStructOf _ _ _ (size, _) _ _) ptr end = do
  let go acc idx = do {
    r <- ffiPeek typ $ ptr `plusPtr` (size * idx);
    if r == end then pure (reverse acc) else go (r : acc) (idx + 1) }
  vector . map box <$> go [] 0

ffiPeekArray0Integral :: forall a m p. (MonadError Error m, MonadIO m) => (Integral a, Bounded a, Storable a) => Ptr p -> m Noun
ffiPeekArray0Integral ptr = do
  values <- liftIO $ peekArray0 (storableZero @a) $ castPtr @p @a ptr
  pure $ vector $ Number . (:+ 0) . fromIntegral <$> values

ffiPeekArray0Floating :: forall a m p. (MonadError Error m, MonadIO m) => (Fractional a, Real a, Storable a) => Ptr p -> m Noun
ffiPeekArray0Floating ptr = do
  values <- liftIO $ peekArray0 (storableZero @a) $ castPtr @p @a ptr
  pure $ vector $ Number . (:+ 0) . realToFrac <$> values

ffiPeekArray0Complex :: forall a m p. (MonadError Error m, MonadIO m) => (Fractional a, Real a, Storable a) => Ptr p -> m Noun
ffiPeekArray0Complex ptr = do
  values <- liftIO $ peekArray0 (storableZero @(Cx.Complex a)) $ castPtr @p @(Cx.Complex a) ptr
  pure $ vector $ Number . fmap realToFrac . fromStd <$> values

ffiPeekArray0Char :: forall a m p. (MonadError Error m, MonadIO m) => (Integral a, Storable a) => Ptr p -> m Noun
ffiPeekArray0Char ptr = do
  values <- liftIO $ peekArray0 (storableZero @a) $ castPtr @p @a ptr
  pure $ vector $ Character . toEnum . fromIntegral <$> values

ffiPeekArray0 :: FFIType -> Ptr p -> St Noun
ffiPeekArray0 FFIVoid _ = throwError $ DomainError "FFI peek void"
ffiPeekArray0 FFIUInt8 ptr = ffiPeekArray0Integral @Word8 ptr
ffiPeekArray0 FFIUInt16 ptr = ffiPeekArray0Integral @Word16 ptr
ffiPeekArray0 FFIUInt32 ptr = ffiPeekArray0Integral @Word32 ptr
ffiPeekArray0 FFIUInt64 ptr = ffiPeekArray0Integral @Word64 ptr
ffiPeekArray0 FFIInt8 ptr = ffiPeekArray0Integral @Int8 ptr
ffiPeekArray0 FFIInt16 ptr = ffiPeekArray0Integral @Int16 ptr
ffiPeekArray0 FFIInt32 ptr = ffiPeekArray0Integral @Int32 ptr
ffiPeekArray0 FFIInt64 ptr = ffiPeekArray0Integral @Int64 ptr
ffiPeekArray0 FFIUInt ptr = ffiPeekArray0Integral @CUInt ptr
ffiPeekArray0 FFIInt ptr = ffiPeekArray0Integral @CInt ptr
ffiPeekArray0 FFIULong ptr = ffiPeekArray0Integral @CULong ptr
ffiPeekArray0 FFILong ptr = ffiPeekArray0Integral @CLong ptr
ffiPeekArray0 FFIUSize ptr = ffiPeekArray0Integral @CSize ptr
ffiPeekArray0 FFIFloat ptr = ffiPeekArray0Floating @CFloat ptr
ffiPeekArray0 FFIDouble ptr = ffiPeekArray0Floating @CDouble ptr
ffiPeekArray0 FFIComplexFloat ptr = ffiPeekArray0Complex @CFloat ptr
ffiPeekArray0 FFIComplexDouble ptr = ffiPeekArray0Complex @CDouble ptr
ffiPeekArray0 FFIChar8 ptr = ffiPeekArray0Char @Word8 ptr
ffiPeekArray0 FFIChar16 ptr = ffiPeekArray0Char @Word16 ptr
ffiPeekArray0 FFIChar32 ptr = ffiPeekArray0Char @Word32 ptr
ffiPeekArray0 FFIUChar ptr = ffiPeekArray0Char @CUChar ptr
ffiPeekArray0 FFIChar ptr = ffiPeekArray0Char @CChar ptr
ffiPeekArray0 FFIWChar ptr = ffiPeekArray0Char @CWchar ptr
ffiPeekArray0 (FFIArrayOf _) _ = throwError $ DomainError "FFI peek array"
ffiPeekArray0 FFIString ptr = do
  ptrPtrs <- liftIO $ peekArray0 storableZero $ castPtr ptr
  strs <- mapM (liftIO . peekCStringSafe) ptrPtrs
  pure $ vector $ box . vector . fmap Character <$> strs
ffiPeekArray0 (FFIPointerTo typ) ptr = do
  ptrPtrs <- liftIO $ peekArray0 storableZero $ castPtr ptr
  pointerObjs <- mapM (pointer typ) ptrPtrs
  pure $ vector pointerObjs
ffiPeekArray0 typ@(FFIStructOf _ _ _ (size, _) _ zero) ptr = do
  let go acc idx = do {
    r <- ffiPeek typ $ ptr `plusPtr` (size * idx);
    if r == zero then pure (reverse acc) else go (r : acc) (idx + 1) }
  vector . map box <$> go [] 0

ffiPokeIntegral :: forall a m p. (MonadError Error m, MonadIO m) => (Integral a, Bounded a, Storable a) => String -> Ptr p -> Noun -> m ()
ffiPokeIntegral name ptr arr = do
  num <- nounToIntegral @a name arr
  liftIO $ poke (castPtr ptr) num

ffiPokeFloating :: forall a m p. (MonadError Error m, MonadIO m) => (Floating a, Storable a) => String -> Ptr p -> Noun -> m ()
ffiPokeFloating name ptr arr = do
  num <- nounToFloating @a name arr
  liftIO $ poke (castPtr ptr) num

ffiPokeComplex :: forall a m p. (MonadError Error m, MonadIO m) => (Floating a, Storable a) => String -> Ptr p -> Noun -> m ()
ffiPokeComplex name ptr arr = do
  num <- nounToComplex @a name arr
  liftIO $ poke (castPtr ptr) num

ffiPokeChar :: forall a m p. (MonadError Error m, MonadIO m) => (Integral a, Bounded a, Storable a) => String -> Ptr p -> Noun -> m ()
ffiPokeChar name ptr arr = do
  ch <- nounToChar name arr
  ffiPokeIntegral @a name ptr $ scalar $ Number $ (:+ 0) $ fromIntegral $ fromEnum ch

ffiPoke :: FFIType -> Ptr p -> Noun -> St ()
ffiPoke FFIVoid _ _ = throwError $ DomainError "FFI poke void"
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
ffiPoke FFIComplexFloat ptr arr = ffiPokeComplex @CFloat "complex float" ptr arr
ffiPoke FFIComplexDouble ptr arr = ffiPokeComplex @CDouble "complex double" ptr arr
ffiPoke FFIChar8 ptr arr = ffiPokeChar @Word8 "char8" ptr arr
ffiPoke FFIChar16 ptr arr = ffiPokeChar @Word16 "char16" ptr arr
ffiPoke FFIChar32 ptr arr = ffiPokeChar @Word32 "char32" ptr arr
ffiPoke FFIUChar ptr arr = ffiPokeChar @CUChar "uchar" ptr arr
ffiPoke FFIChar ptr arr = ffiPokeChar @CChar "char" ptr arr
ffiPoke FFIWChar ptr arr = ffiPokeChar @CWchar "wchar" ptr arr
ffiPoke (FFIArrayOf _) _ _ = throwError $ DomainError "FFI poke array"
ffiPoke FFIString ptr arr = do
  pointerObj <- asScalar (DomainError "Poke string not a pointer object (manually allocate!)") arr
  ptrPtr <- pointerGet pointerObj
  liftIO $ poke (castPtr ptr) ptrPtr
ffiPoke (FFIPointerTo _) ptr arr = do
  pointerObj <- asScalar (DomainError "Poke pointer not a pointer object") arr
  ptrPtr <- pointerGet pointerObj
  liftIO $ poke (castPtr ptr) ptrPtr
ffiPoke (FFIStructOf typs _ _ _ offsets _) ptr arr = do
  let err = DomainError "Poke struct not a vector of the correct length"
  vec <- map fromScalar <$> asVector err arr
  when (length vec /= length typs) $ throwError err
  mapM_ (\(off, typ, elem) -> ffiPoke typ (ptr `plusPtr` off) elem) $ zip3 offsets typs vec

ffiPokeArrayIntegral :: forall a m p. (MonadError Error m, MonadIO m) => (Integral a, Bounded a, Storable a) => String -> Ptr p -> Noun -> m ()
ffiPokeArrayIntegral name ptr arr = do
  nums <- asVector (DomainError "FFI poke array must be vector") arr >>= mapM (nounToIntegral @a name . fromScalar)
  liftIO $ pokeArray (castPtr ptr) nums

ffiPokeArrayFloating :: forall a m p. (MonadError Error m, MonadIO m) => (Floating a, Storable a) => String -> Ptr p -> Noun -> m ()
ffiPokeArrayFloating name ptr arr = do
  nums <- asVector (DomainError "FFI poke array must be vector") arr >>= mapM (nounToFloating @a name . fromScalar)
  liftIO $ pokeArray (castPtr ptr) nums

ffiPokeArrayComplex :: forall a m p. (MonadError Error m, MonadIO m) => (Floating a, Storable a) => String -> Ptr p -> Noun -> m ()
ffiPokeArrayComplex name ptr arr = do
  nums <- asVector (DomainError "FFI poke array must be vector") arr >>= mapM (nounToComplex @a name . fromScalar)
  liftIO $ pokeArray (castPtr ptr) nums

ffiPokeArrayChar :: forall a m p. (MonadError Error m, MonadIO m) => (Integral a, Bounded a, Storable a) => String -> Ptr p -> Noun -> m ()
ffiPokeArrayChar name ptr arr = do
  chars <- asVector (DomainError "FFI poke array must be vector") arr >>= mapM (nounToChar name . fromScalar)
  ffiPokeArrayIntegral @a name ptr $ vector $ Number . (:+ 0) . fromIntegral . fromEnum <$> chars

ffiPokeArray :: FFIType -> Ptr p -> Noun -> St ()
ffiPokeArray FFIVoid _ _ = throwError $ DomainError "FFI poke array void"
ffiPokeArray FFIUInt8 ptr arr = ffiPokeArrayIntegral @Word8 "uint8" ptr arr
ffiPokeArray FFIUInt16 ptr arr = ffiPokeArrayIntegral @Word16 "uint16" ptr arr
ffiPokeArray FFIUInt32 ptr arr = ffiPokeArrayIntegral @Word32 "uint32" ptr arr
ffiPokeArray FFIUInt64 ptr arr = ffiPokeArrayIntegral @Word64 "uint64" ptr arr
ffiPokeArray FFIInt8 ptr arr = ffiPokeArrayIntegral @Int8 "int8" ptr arr
ffiPokeArray FFIInt16 ptr arr = ffiPokeArrayIntegral @Int16 "int16" ptr arr
ffiPokeArray FFIInt32 ptr arr = ffiPokeArrayIntegral @Int32 "int32" ptr arr
ffiPokeArray FFIInt64 ptr arr = ffiPokeArrayIntegral @Int64 "int64" ptr arr
ffiPokeArray FFIUInt ptr arr = ffiPokeArrayIntegral @CUInt "uint" ptr arr
ffiPokeArray FFIInt ptr arr = ffiPokeArrayIntegral @CInt "int" ptr arr
ffiPokeArray FFIULong ptr arr = ffiPokeArrayIntegral @CULong "ulong" ptr arr
ffiPokeArray FFILong ptr arr = ffiPokeArrayIntegral @CLong "long" ptr arr
ffiPokeArray FFIUSize ptr arr = ffiPokeArrayIntegral @CSize "usize" ptr arr
ffiPokeArray FFIFloat ptr arr = ffiPokeArrayFloating @CFloat "float" ptr arr
ffiPokeArray FFIDouble ptr arr = ffiPokeArrayFloating @CDouble "double" ptr arr
ffiPokeArray FFIComplexFloat ptr arr = ffiPokeArrayComplex @CFloat "complex float" ptr arr
ffiPokeArray FFIComplexDouble ptr arr = ffiPokeArrayComplex @CDouble "complex double" ptr arr
ffiPokeArray FFIChar8 ptr arr = ffiPokeArrayChar @Word8 "char8" ptr arr
ffiPokeArray FFIChar16 ptr arr = ffiPokeArrayChar @Word16 "char16" ptr arr
ffiPokeArray FFIChar32 ptr arr = ffiPokeArrayChar @Word32 "char32" ptr arr
ffiPokeArray FFIUChar ptr arr = ffiPokeArrayChar @CUChar "uchar" ptr arr
ffiPokeArray FFIChar ptr arr = ffiPokeArrayChar @CChar "char" ptr arr
ffiPokeArray FFIWChar ptr arr = ffiPokeArrayChar @CWchar "wchar" ptr arr
ffiPokeArray (FFIArrayOf _) _ _ = throwError $ DomainError "FFI poke array array"
ffiPokeArray FFIString ptr arr = do
  ptrs <- asVector (DomainError "FFI poke array must be vector") arr >>= mapM pointerGet
  liftIO $ pokeArray (castPtr ptr) ptrs
ffiPokeArray (FFIPointerTo _) ptr arr = do
  ptrs <- asVector (DomainError "FFI poke array must be vector") arr >>= mapM pointerGet
  liftIO $ pokeArray (castPtr ptr) ptrs
ffiPokeArray typ@(FFIStructOf typs _ _ (size, _) _ _) ptr arr = do
  let err = DomainError "FFI poke struct must be a vector of the correct length"
  vec <- map fromScalar <$> asVector err arr
  when (length vec /= length typs) $ throwError err
  zipWithM_ (\idx elem -> ffiPoke typ (ptr `plusPtr` (size * idx)) elem) [0..(length vec - 1)] vec

ffiPokeArrayEndIntegral :: forall a m p. (MonadError Error m, MonadIO m) => (Integral a, Bounded a, Storable a) => String -> Ptr p -> Noun -> Noun -> m ()
ffiPokeArrayEndIntegral name ptr end arr = do
  eNum <- nounToIntegral @a name end
  nums <- asVector (DomainError "FFI poke array must be vector") arr >>= mapM (nounToIntegral @a name . fromScalar)
  liftIO $ pokeArray0 eNum (castPtr ptr) nums

ffiPokeArrayEndFloating :: forall a m p. (MonadError Error m, MonadIO m) => (Floating a, Storable a) => String -> Ptr p -> Noun -> Noun -> m ()
ffiPokeArrayEndFloating name ptr end arr = do
  eNum <- nounToFloating @a name end
  nums <- asVector (DomainError "FFI poke array must be vector") arr >>= mapM (nounToFloating @a name . fromScalar)
  liftIO $ pokeArray0 eNum (castPtr ptr) nums

ffiPokeArrayEndComplex :: forall a m p. (MonadError Error m, MonadIO m) => (Floating a, Storable a) => String -> Ptr p -> Noun -> Noun -> m ()
ffiPokeArrayEndComplex name ptr end arr = do
  eNum <- nounToComplex @a name end
  nums <- asVector (DomainError "FFI poke array must be vector") arr >>= mapM (nounToComplex @a name . fromScalar)
  liftIO $ pokeArray0 eNum (castPtr ptr) nums

ffiPokeArrayEndChar :: forall a m p. (MonadError Error m, MonadIO m) => (Integral a, Bounded a, Storable a) => String -> Ptr p -> Noun -> Noun -> m ()
ffiPokeArrayEndChar name ptr end arr = do
  eChar <- nounToChar name end
  chars <- asVector (DomainError "FFI poke array must be vector") arr >>= mapM (nounToChar name . fromScalar)
  ffiPokeArrayEndIntegral @a name ptr (scalar $ Number $ (:+ 0) $ fromIntegral $ fromEnum eChar) (vector $ Number . (:+ 0) . fromIntegral . fromEnum <$> chars)

ffiPokeArrayEnd :: FFIType -> Ptr p -> Noun -> Noun -> St ()
ffiPokeArrayEnd FFIVoid _ _ _ = throwError $ DomainError "FFI poke array zero void"
ffiPokeArrayEnd FFIUInt8 ptr end arr = ffiPokeArrayEndIntegral @Word8 "uint8" ptr end arr
ffiPokeArrayEnd FFIUInt16 ptr end arr = ffiPokeArrayEndIntegral @Word16 "uint16" ptr end arr
ffiPokeArrayEnd FFIUInt32 ptr end arr = ffiPokeArrayEndIntegral @Word32 "uint32" ptr end arr
ffiPokeArrayEnd FFIUInt64 ptr end arr = ffiPokeArrayEndIntegral @Word64 "uint64" ptr end arr
ffiPokeArrayEnd FFIInt8 ptr end arr = ffiPokeArrayEndIntegral @Int8 "int8" ptr end arr
ffiPokeArrayEnd FFIInt16 ptr end arr = ffiPokeArrayEndIntegral @Int16 "int16" ptr end arr
ffiPokeArrayEnd FFIInt32 ptr end arr = ffiPokeArrayEndIntegral @Int32 "int32" ptr end arr
ffiPokeArrayEnd FFIInt64 ptr end arr = ffiPokeArrayEndIntegral @Int64 "int64" ptr end arr
ffiPokeArrayEnd FFIUInt ptr end arr = ffiPokeArrayEndIntegral @CUInt "uint" ptr end arr
ffiPokeArrayEnd FFIInt ptr end arr = ffiPokeArrayEndIntegral @CInt "int" ptr end arr
ffiPokeArrayEnd FFIULong ptr end arr = ffiPokeArrayEndIntegral @CULong "ulong" ptr end arr
ffiPokeArrayEnd FFILong ptr end arr = ffiPokeArrayEndIntegral @CLong "long" ptr end arr
ffiPokeArrayEnd FFIUSize ptr end arr = ffiPokeArrayEndIntegral @CSize "usize" ptr end arr
ffiPokeArrayEnd FFIFloat ptr end arr = ffiPokeArrayEndFloating @CFloat "float" ptr end arr
ffiPokeArrayEnd FFIDouble ptr end arr = ffiPokeArrayEndFloating @CDouble "double" ptr end arr
ffiPokeArrayEnd FFIComplexFloat ptr end arr = ffiPokeArrayEndComplex @CFloat "complex float" ptr end arr
ffiPokeArrayEnd FFIComplexDouble ptr end arr = ffiPokeArrayEndComplex @CDouble "complex double" ptr end arr
ffiPokeArrayEnd FFIChar8 ptr end arr = ffiPokeArrayEndChar @Word8 "char8" ptr end arr
ffiPokeArrayEnd FFIChar16 ptr end arr = ffiPokeArrayEndChar @Word16 "char16" ptr end arr
ffiPokeArrayEnd FFIChar32 ptr end arr = ffiPokeArrayEndChar @Word32 "char32" ptr end arr
ffiPokeArrayEnd FFIUChar ptr end arr = ffiPokeArrayEndChar @CUChar "uchar" ptr end arr
ffiPokeArrayEnd FFIChar ptr end arr = ffiPokeArrayEndChar @CChar "char" ptr end arr
ffiPokeArrayEnd FFIWChar ptr end arr = ffiPokeArrayEndChar @CWchar "wchar" ptr end arr
ffiPokeArrayEnd (FFIArrayOf _) _ _ _ = throwError $ DomainError "FFI poke array zero array"
ffiPokeArrayEnd FFIString ptr end arr = do
  ePtr <- asScalar (DomainError "FFI poke array zero zero must be pointer object") end >>= pointerGet
  ptrs <- asVector (DomainError "FFI poke array zero must be vector") arr >>= mapM pointerGet
  liftIO $ pokeArray0 ePtr (castPtr ptr) ptrs
ffiPokeArrayEnd (FFIPointerTo _) ptr end arr = do
  ePtr <- asScalar (DomainError "FFI poke array string zero must be pointer object (manually allocate!)") end >>= pointerGet
  ptrs <- asVector (DomainError "FFI poke array must be vector") arr >>= mapM pointerGet
  liftIO $ pokeArray0 ePtr (castPtr ptr) ptrs
ffiPokeArrayEnd typ@(FFIStructOf typs _ _ (size, _) _ _) ptr end arr = do
  let err = DomainError "FFI poke struct must be a vector of the correct length"
  vec <- map fromScalar <$> asVector err arr
  when (length vec /= length typs) $ throwError err
  zipWithM_ (\idx elem -> ffiPoke typ (ptr `plusPtr` (size * idx)) elem) [0..(length vec - 1)] vec
  ffiPoke typ (ptr `plusPtr` (size * length vec)) end

storableZero :: forall a. Storable a => a
storableZero = unsafePerformIO $ bracket calloc free peek

ffiPokeArray0Integral :: forall a m p. (MonadError Error m, MonadIO m) => (Integral a, Bounded a, Storable a) => String -> Ptr p -> Noun -> m ()
ffiPokeArray0Integral name ptr arr = do
  nums <- asVector (DomainError "FFI poke array must be vector") arr >>= mapM (nounToIntegral @a name . fromScalar)
  liftIO $ pokeArray0 (storableZero @a) (castPtr ptr) nums

ffiPokeArray0Floating :: forall a m p. (MonadError Error m, MonadIO m) => (Floating a, Storable a) => String -> Ptr p -> Noun -> m ()
ffiPokeArray0Floating name ptr arr = do
  nums <- asVector (DomainError "FFI poke array must be vector") arr >>= mapM (nounToFloating @a name . fromScalar)
  liftIO $ pokeArray0 (storableZero @a) (castPtr ptr) nums

ffiPokeArray0Complex :: forall a m p. (MonadError Error m, MonadIO m) => (Floating a, Storable a) => String -> Ptr p -> Noun -> m ()
ffiPokeArray0Complex name ptr arr = do
  nums <- asVector (DomainError "FFI poke array must be vector") arr >>= mapM (nounToComplex @a name . fromScalar)
  liftIO $ pokeArray0 (storableZero @(Cx.Complex a)) (castPtr ptr) nums

ffiPokeArray0Char :: forall a m p. (MonadError Error m, MonadIO m) => (Integral a, Bounded a, Storable a) => String -> Ptr p -> Noun -> m ()
ffiPokeArray0Char name ptr arr = do
  chars <- asVector (DomainError "FFI poke array must be vector") arr >>= mapM (nounToChar name . fromScalar)
  ffiPokeArray0Integral @a name ptr $ vector $ Number . (:+ 0) . fromIntegral . fromEnum <$> chars

ffiPokeArray0 :: FFIType -> Ptr p -> Noun -> St ()
ffiPokeArray0 FFIVoid _ _ = throwError $ DomainError "FFI poke array void"
ffiPokeArray0 FFIUInt8 ptr arr = ffiPokeArray0Integral @Word8 "uint8" ptr arr
ffiPokeArray0 FFIUInt16 ptr arr = ffiPokeArray0Integral @Word16 "uint16" ptr arr
ffiPokeArray0 FFIUInt32 ptr arr = ffiPokeArray0Integral @Word32 "uint32" ptr arr
ffiPokeArray0 FFIUInt64 ptr arr = ffiPokeArray0Integral @Word64 "uint64" ptr arr
ffiPokeArray0 FFIInt8 ptr arr = ffiPokeArray0Integral @Int8 "int8" ptr arr
ffiPokeArray0 FFIInt16 ptr arr = ffiPokeArray0Integral @Int16 "int16" ptr arr
ffiPokeArray0 FFIInt32 ptr arr = ffiPokeArray0Integral @Int32 "int32" ptr arr
ffiPokeArray0 FFIInt64 ptr arr = ffiPokeArray0Integral @Int64 "int64" ptr arr
ffiPokeArray0 FFIUInt ptr arr = ffiPokeArray0Integral @CUInt "uint" ptr arr
ffiPokeArray0 FFIInt ptr arr = ffiPokeArray0Integral @CInt "int" ptr arr
ffiPokeArray0 FFIULong ptr arr = ffiPokeArray0Integral @CULong "ulong" ptr arr
ffiPokeArray0 FFILong ptr arr = ffiPokeArray0Integral @CLong "long" ptr arr
ffiPokeArray0 FFIUSize ptr arr = ffiPokeArray0Integral @CSize "usize" ptr arr
ffiPokeArray0 FFIFloat ptr arr = ffiPokeArray0Floating @CFloat "float" ptr arr
ffiPokeArray0 FFIDouble ptr arr = ffiPokeArray0Floating @CDouble "double" ptr arr
ffiPokeArray0 FFIComplexFloat ptr arr = ffiPokeArray0Complex @CFloat "complex float" ptr arr
ffiPokeArray0 FFIComplexDouble ptr arr = ffiPokeArray0Complex @CDouble "complex double" ptr arr
ffiPokeArray0 FFIChar8 ptr arr = ffiPokeArray0Char @Word8 "char8" ptr arr
ffiPokeArray0 FFIChar16 ptr arr = ffiPokeArray0Char @Word16 "char16" ptr arr
ffiPokeArray0 FFIChar32 ptr arr = ffiPokeArray0Char @Word32 "char32" ptr arr
ffiPokeArray0 FFIUChar ptr arr = ffiPokeArray0Char @CUChar "uchar" ptr arr
ffiPokeArray0 FFIChar ptr arr = ffiPokeArray0Char @CChar "char" ptr arr
ffiPokeArray0 FFIWChar ptr arr = ffiPokeArray0Char @CWchar "wchar" ptr arr
ffiPokeArray0 (FFIArrayOf _) _ _ = throwError $ DomainError "FFI poke array array"
ffiPokeArray0 FFIString ptr arr = do
  ptrs <- asVector (DomainError "FFI poke array must be vector") arr >>= mapM pointerGet
  liftIO $ pokeArray0 storableZero (castPtr ptr) ptrs
ffiPokeArray0 (FFIPointerTo _) ptr arr = do
  ptrs <- asVector (DomainError "FFI poke array must be vector") arr >>= mapM pointerGet
  liftIO $ pokeArray0 storableZero (castPtr ptr) ptrs
ffiPokeArray0 typ@(FFIStructOf typs _ _ (size, _) _ zero) ptr arr = do
  let err = DomainError "FFI poke struct must be a vector of the correct length"
  vec <- map fromScalar <$> asVector err arr
  when (length vec /= length typs) $ throwError err
  zipWithM_ (\idx elem -> ffiPoke typ (ptr `plusPtr` (size * idx)) elem) [0..(length vec - 1)] vec
  ffiPoke typ (ptr `plusPtr` (size * length vec)) zero

ffiNewIntegral :: forall a m p. (MonadError Error m, MonadIO m) => (Integral a, Bounded a, Storable a) => String -> Noun -> m (Ptr p, m ())
ffiNewIntegral name arr = do
  num <- nounToIntegral @a name arr
  ptr <- liftIO $ castPtr <$> new num
  pure (ptr, liftIO (free ptr))

ffiNewFloating :: forall a m p. (MonadError Error m, MonadIO m) => (Floating a, Storable a) => String -> Noun -> m (Ptr p, m ())
ffiNewFloating name arr = do
  num <- nounToFloating @a name arr
  ptr <- liftIO $ castPtr <$> new num
  pure (ptr, liftIO (free ptr))

ffiNewComplex :: forall a m p. (MonadError Error m, MonadIO m) => (Floating a, Storable a) => String -> Noun -> m (Ptr p, m ())
ffiNewComplex name arr = do
  num <- nounToComplex @a name arr
  ptr <- liftIO $ castPtr <$> new num
  pure (ptr, liftIO (free ptr))

ffiNewChar :: forall a m p. (MonadError Error m, MonadIO m) => (Integral a, Bounded a, Storable a) => String -> Noun -> m (Ptr p, m ())
ffiNewChar name arr = do
  char <- nounToChar name arr
  ffiNewIntegral @a name $ scalar $ Number $ (:+ 0) $ fromIntegral $ fromEnum char 

ffiNew :: FFIType -> Noun -> St (Ptr p, St ())
ffiNew FFIVoid _ = throwError $ DomainError "FFI void array"
ffiNew FFIUInt8 arr = ffiNewIntegral @Word8 "uint8" arr
ffiNew FFIUInt16 arr = ffiNewIntegral @Word16 "uint16" arr
ffiNew FFIUInt32 arr = ffiNewIntegral @Word32 "uint32" arr
ffiNew FFIUInt64 arr = ffiNewIntegral @Word64 "uint64" arr
ffiNew FFIInt8 arr = ffiNewIntegral @Int8 "int8" arr
ffiNew FFIInt16 arr = ffiNewIntegral @Int16 "int16" arr
ffiNew FFIInt32 arr = ffiNewIntegral @Int32 "int32" arr
ffiNew FFIInt64 arr = ffiNewIntegral @Int64 "int64" arr
ffiNew FFIUInt arr = ffiNewIntegral @CUInt "uint" arr
ffiNew FFIInt arr = ffiNewIntegral @CInt "int" arr
ffiNew FFIULong arr = ffiNewIntegral @CULong "ulong" arr
ffiNew FFILong arr = ffiNewIntegral @CLong "long" arr
ffiNew FFIUSize arr = ffiNewIntegral @CSize "usize" arr
ffiNew FFIFloat arr = ffiNewFloating @CFloat "float" arr
ffiNew FFIDouble arr = ffiNewFloating @CDouble "double" arr
ffiNew FFIComplexFloat arr = ffiNewComplex @CFloat "complex float" arr
ffiNew FFIComplexDouble arr = ffiNewComplex @CDouble "complex double" arr
ffiNew FFIChar8 arr = ffiNewChar @Word8 "char8" arr
ffiNew FFIChar16 arr = ffiNewChar @Word16 "char16" arr
ffiNew FFIChar32 arr = ffiNewChar @Word32 "char32" arr
ffiNew FFIUChar arr = ffiNewChar @CUChar "uchar" arr
ffiNew FFIChar arr = ffiNewChar @CChar "char" arr
ffiNew FFIWChar arr = ffiNewChar @CWchar "wchar" arr
ffiNew (FFIArrayOf typ) arr = ffiNewArray typ arr
ffiNew FFIString arr = do
  let err = DomainError "FFI string must be string"
  str <- asString err arr
  ptr <- liftIO $ castPtr <$> newCString str
  pure (ptr, liftIO (free ptr))
ffiNew (FFIPointerTo _) arr = do
  let err = DomainError "FFI pointer must be pointer object"
  ptrPtr <- asScalar err arr >>= pointerGet
  ptr <- liftIO $ castPtr <$> new ptrPtr
  pure (ptr, liftIO (free ptr))
ffiNew typ@(FFIStructOf typs _ _ (size, _) _ _) arr = do
  let err = DomainError "FFI new struct must be a vector of the correct length"
  vec <- map fromScalar <$> asVector err arr
  when (length vec /= length typs) $ throwError err
  ptr <- liftIO $ mallocBytes size
  ffiPoke typ ptr arr
  pure (ptr, liftIO (free ptr))

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

ffiNewArrayComplex :: forall a m p. (MonadError Error m, MonadIO m) => (Floating a, Storable a) => String -> Noun -> m (Ptr p, m ())
ffiNewArrayComplex name arr = do
  let err = DomainError "FFI array must be vector"
  vec <- fmap fromScalar <$> asVector err arr
  nums <- mapM (nounToComplex @a name) vec
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
ffiNewArray FFIComplexFloat arr = ffiNewArrayComplex @CFloat "complex float" arr
ffiNewArray FFIComplexDouble arr = ffiNewArrayComplex @CDouble "complex double" arr
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
ffiNewArray typ@(FFIStructOf _ _ _ (size, _) _ _) arr = do
  let err = DomainError "FFI struct arraym must be vector"
  vec <- map fromScalar <$> asVector err arr
  ptr <- liftIO $ mallocBytes $ size * length vec
  ffiPokeArray typ ptr arr
  pure (ptr, liftIO (free ptr))

{-
ffiNewArrayEndIntegral :: forall a m p. (MonadError Error m, MonadIO m) => (Integral a, Bounded a, Storable a) => String -> Noun -> Noun -> m (Ptr p, m ())
ffiNewArrayEndIntegral name end arr = do
  eNum <- nounToIntegral @a name end
  let err = DomainError "FFI array must be vector"
  vec <- fmap fromScalar <$> asVector err arr
  nums <- mapM (nounToIntegral @a name) vec
  ptr <- liftIO $ castPtr <$> newArray0 eNum nums
  pure (ptr, liftIO (free ptr))

ffiNewArrayEndFloating :: forall a m p. (MonadError Error m, MonadIO m) => (Floating a, Storable a) => String -> Noun -> Noun -> m (Ptr p, m ())
ffiNewArrayEndFloating name end arr = do
  eNum <- nounToFloating @a name end
  let err = DomainError "FFI array must be vector"
  vec <- fmap fromScalar <$> asVector err arr
  nums <- mapM (nounToFloating @a name) vec
  ptr <- liftIO $ castPtr <$> newArray0 eNum nums
  pure (ptr, liftIO (free ptr))

ffiNewArrayEndChar :: forall a m p. (MonadError Error m, MonadIO m) => (Integral a, Bounded a, Storable a) => String -> Noun -> Noun -> m (Ptr p, m ())
ffiNewArrayEndChar name end arr = do
  eChar <- nounToChar name end
  let err = DomainError "FFI char array must be string"
  str <- asString err arr
  ffiNewArrayEndIntegral @a name (scalar $ Number $ (:+ 0) $ fromIntegral $ fromEnum eChar) (vector $ (Number . (:+ 0) . fromIntegral . fromEnum) <$> str)

ffiNewArrayEnd :: FFIType -> Noun -> Noun -> St (Ptr p, St ())
ffiNewArrayEnd FFIVoid _ _ = throwError $ DomainError "FFI void array"
ffiNewArrayEnd FFIUInt8 end arr = ffiNewArrayEndIntegral @Word8 "uint8" end arr
ffiNewArrayEnd FFIUInt16 end arr = ffiNewArrayEndIntegral @Word16 "uint16" end arr
ffiNewArrayEnd FFIUInt32 end arr = ffiNewArrayEndIntegral @Word32 "uint32" end arr
ffiNewArrayEnd FFIUInt64 end arr = ffiNewArrayEndIntegral @Word64 "uint64" end arr
ffiNewArrayEnd FFIInt8 end arr = ffiNewArrayEndIntegral @Int8 "int8" end arr
ffiNewArrayEnd FFIInt16 end arr = ffiNewArrayEndIntegral @Int16 "int16" end arr
ffiNewArrayEnd FFIInt32 end arr = ffiNewArrayEndIntegral @Int32 "int32" end arr
ffiNewArrayEnd FFIInt64 end arr = ffiNewArrayEndIntegral @Int64 "int64" end arr
ffiNewArrayEnd FFIUInt end arr = ffiNewArrayEndIntegral @CUInt "uint" end arr
ffiNewArrayEnd FFIInt end arr = ffiNewArrayEndIntegral @CInt "int" end arr
ffiNewArrayEnd FFIULong end arr = ffiNewArrayEndIntegral @CULong "ulong" end arr
ffiNewArrayEnd FFILong end arr = ffiNewArrayEndIntegral @CLong "long" end arr
ffiNewArrayEnd FFIUSize end arr = ffiNewArrayEndIntegral @CSize "usize" end arr
ffiNewArrayEnd FFIFloat end arr = ffiNewArrayEndFloating @CFloat "float" end arr
ffiNewArrayEnd FFIDouble end arr = ffiNewArrayEndFloating @CDouble "double" end arr
ffiNewArrayEnd FFIChar8 end arr = ffiNewArrayEndChar @Word8 "char8" end arr
ffiNewArrayEnd FFIChar16 end arr = ffiNewArrayEndChar @Word16 "char16" end arr
ffiNewArrayEnd FFIChar32 end arr = ffiNewArrayEndChar @Word32 "char32" end arr
ffiNewArrayEnd FFIUChar end arr = ffiNewArrayEndChar @CUChar "uchar" end arr
ffiNewArrayEnd FFIChar end arr = ffiNewArrayEndChar @CChar "char" end arr
ffiNewArrayEnd FFIWChar end arr = ffiNewArrayEndChar @CWchar "wchar" end arr
ffiNewArrayEnd (FFIArrayOf typ) end arr = do
  let err = DomainError "FFI array must be vector"
  vec <- fmap fromScalar <$> asVector err arr
  (ptrs, cleanups) <- unzip <$> mapM (ffiNewArray typ) vec
  ptr <- liftIO $ newArray ptrs
  pure (castPtr ptr, sequence_ cleanups >> liftIO (free ptr))
ffiNewArrayEnd FFIString end arr = do
  eStr <- asString (DomainError "FFI string array end must be string") end
  ePtr <- liftIO $ newCString eStr
  let err = DomainError "FFI string array must be vector of strings"
  strs <- asStrings err arr
  ptrs <- forM strs $ \str -> liftIO $ newCString str
  ptr <- liftIO $ newArray0 ePtr ptrs
  pure (castPtr ptr, liftIO (free ePtr) >> mapM_ (liftIO . free) ptrs >> liftIO (free ptr))
ffiNewArrayEnd (FFIPointerTo _) end arr = do
  ePtr <- asScalar (DomainError "FFI pointer array end must be pointer object") end >>= pointerGet
  let err = DomainError "FFI pointer array must be vector of pointer objects"
  ptrs <- asVector err arr >>= mapM pointerGet
  ptr <- liftIO $ newArray0 ePtr ptrs
  pure (castPtr ptr, liftIO $ free ptr)
-}

ffiNewArray0Integral :: forall a m p. (MonadError Error m, MonadIO m) => (Integral a, Bounded a, Storable a) => String -> Noun -> m (Ptr p, m ())
ffiNewArray0Integral name arr = do
  let err = DomainError "FFI array must be vector"
  vec <- fmap fromScalar <$> asVector err arr
  nums <- mapM (nounToIntegral @a name) vec
  ptr <- liftIO $ castPtr <$> newArray0 (storableZero @a) nums
  pure (ptr, liftIO (free ptr))

ffiNewArray0Floating :: forall a m p. (MonadError Error m, MonadIO m) => (Floating a, Storable a) => String -> Noun -> m (Ptr p, m ())
ffiNewArray0Floating name arr = do
  let err = DomainError "FFI array must be vector"
  vec <- fmap fromScalar <$> asVector err arr
  nums <- mapM (nounToFloating @a name) vec
  ptr <- liftIO $ castPtr <$> newArray0 (storableZero @a) nums
  pure (ptr, liftIO (free ptr))

ffiNewArray0Complex :: forall a m p. (MonadError Error m, MonadIO m) => (Floating a, Storable a) => String -> Noun -> m (Ptr p, m ())
ffiNewArray0Complex name arr = do
  let err = DomainError "FFI array must be vector"
  vec <- fmap fromScalar <$> asVector err arr
  nums <- mapM (nounToComplex @a name) vec
  ptr <- liftIO $ castPtr <$> newArray0 (storableZero @(Cx.Complex a)) nums
  pure (ptr, liftIO (free ptr))

ffiNewArray0Char :: forall a m p. (MonadError Error m, MonadIO m) => (Integral a, Bounded a, Storable a) => String -> Noun -> m (Ptr p, m ())
ffiNewArray0Char name arr = do
  let err = DomainError "FFI char array must be string"
  str <- asString err arr
  ffiNewArray0Integral @a name $ vector $ (Number . (:+ 0) . fromIntegral . fromEnum) <$> str

ffiNewArray0 :: FFIType -> Noun -> St (Ptr p, St ())
ffiNewArray0 FFIVoid _ = throwError $ DomainError "FFI void array"
ffiNewArray0 FFIUInt8 arr = ffiNewArray0Integral @Word8 "uint8" arr
ffiNewArray0 FFIUInt16 arr = ffiNewArray0Integral @Word16 "uint16" arr
ffiNewArray0 FFIUInt32 arr = ffiNewArray0Integral @Word32 "uint32" arr
ffiNewArray0 FFIUInt64 arr = ffiNewArray0Integral @Word64 "uint64" arr
ffiNewArray0 FFIInt8 arr = ffiNewArray0Integral @Int8 "int8" arr
ffiNewArray0 FFIInt16 arr = ffiNewArray0Integral @Int16 "int16" arr
ffiNewArray0 FFIInt32 arr = ffiNewArray0Integral @Int32 "int32" arr
ffiNewArray0 FFIInt64 arr = ffiNewArray0Integral @Int64 "int64" arr
ffiNewArray0 FFIUInt arr = ffiNewArray0Integral @CUInt "uint" arr
ffiNewArray0 FFIInt arr = ffiNewArray0Integral @CInt "int" arr
ffiNewArray0 FFIULong arr = ffiNewArray0Integral @CULong "ulong" arr
ffiNewArray0 FFILong arr = ffiNewArray0Integral @CLong "long" arr
ffiNewArray0 FFIUSize arr = ffiNewArray0Integral @CSize "usize" arr
ffiNewArray0 FFIFloat arr = ffiNewArray0Floating @CFloat "float" arr
ffiNewArray0 FFIDouble arr = ffiNewArray0Floating @CDouble "double" arr
ffiNewArray0 FFIComplexFloat arr = ffiNewArray0Complex @CFloat "complex float" arr
ffiNewArray0 FFIComplexDouble arr = ffiNewArray0Complex @CDouble "complex double" arr
ffiNewArray0 FFIChar8 arr = ffiNewArray0Char @Word8 "char8" arr
ffiNewArray0 FFIChar16 arr = ffiNewArray0Char @Word16 "char16" arr
ffiNewArray0 FFIChar32 arr = ffiNewArray0Char @Word32 "char32" arr
ffiNewArray0 FFIUChar arr = ffiNewArray0Char @CUChar "uchar" arr
ffiNewArray0 FFIChar arr = ffiNewArray0Char @CChar "char" arr
ffiNewArray0 FFIWChar arr = ffiNewArray0Char @CWchar "wchar" arr
ffiNewArray0 (FFIArrayOf typ) arr = do
  let err = DomainError "FFI array must be vector"
  vec <- fmap fromScalar <$> asVector err arr
  (ptrs, cleanups) <- unzip <$> mapM (ffiNewArray typ) vec
  ptr <- liftIO $ newArray0 storableZero ptrs
  pure (castPtr ptr, sequence_ cleanups >> liftIO (free ptr))
ffiNewArray0 FFIString arr = do
  let err = DomainError "FFI string array must be vector of strings"
  strs <- asStrings err arr
  ptrs <- forM strs $ \str -> liftIO $ newCString str
  ptr <- liftIO $ newArray0 storableZero ptrs
  pure (castPtr ptr, mapM_ (liftIO . free) ptrs >> liftIO (free ptr))
ffiNewArray0 (FFIPointerTo _) arr = do
  let err = DomainError "FFI pointer array must be vector of pointer objects"
  ptrs <- asVector err arr >>= mapM pointerGet
  ptr <- liftIO $ newArray0 storableZero ptrs
  pure (castPtr ptr, liftIO $ free ptr)
ffiNewArray0 typ@(FFIStructOf _ _ _ (size, _) _ _) arr = do
  let err = DomainError "FFI struct arraym must be vector"
  vec <- map fromScalar <$> asVector err arr
  ptr <- liftIO $ mallocBytes $ size * (length vec + 1)
  ffiPokeArray0 typ ptr arr
  pure (ptr, liftIO (free ptr))

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
ffiSize FFIComplexFloat = ffiSizeStorable @(Cx.Complex CFloat)
ffiSize FFIComplexDouble = ffiSizeStorable @(Cx.Complex CDouble)
ffiSize FFIChar8 = ffiSizeStorable @Word8
ffiSize FFIChar16 = ffiSizeStorable @Word16
ffiSize FFIChar32 = ffiSizeStorable @Word32
ffiSize FFIUChar = ffiSizeStorable @CUChar
ffiSize FFIChar = ffiSizeStorable @CChar
ffiSize FFIWChar = ffiSizeStorable @CWchar
ffiSize (FFIArrayOf _) = ffiSizeStorable @(Ptr ())
ffiSize FFIString = ffiSizeStorable @(Ptr ())
ffiSize (FFIPointerTo _) = ffiSizeStorable @(Ptr ())
ffiSize (FFIStructOf _ _ _ (size, _) _ _) = size

ffiReturnInteger :: MonadError Error m => Integral a => FFI.RetType a -> FFI.RetType (m Noun)
ffiReturnInteger ret = pure . scalar . Number . (:+ 0) . fromIntegral <$> ret

ffiReturnFloating :: MonadError Error m => Real a => FFI.RetType a -> FFI.RetType (m Noun)
ffiReturnFloating ret = pure . scalar . Number . (:+ 0) . realToFrac <$> ret

ffiReturnComplex :: MonadError Error m => Real a => FFI.RetType (Cx.Complex a) -> FFI.RetType (m Noun)
ffiReturnComplex ret = pure . scalar . Number . fmap realToFrac . fromStd <$> ret

ffiReturnChar :: MonadError Error m => Integral a => FFI.RetType a -> FFI.RetType (m Noun)
ffiReturnChar ret = pure . scalar . Character . toEnum . fromIntegral <$> ret

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
ffiReturn FFIComplexFloat = ffiReturnComplex FFI.retComplexCFloat
ffiReturn FFIComplexDouble = ffiReturnComplex FFI.retComplexCDouble
ffiReturn FFIChar8 = ffiReturnChar FFI.retWord8
ffiReturn FFIChar16 = ffiReturnChar FFI.retWord16
ffiReturn FFIChar32 = ffiReturnChar FFI.retWord32
ffiReturn FFIUChar = ffiReturnChar FFI.retCUChar
ffiReturn FFIChar = ffiReturnChar FFI.retCChar
ffiReturn FFIWChar = ffiReturnChar FFI.retCWchar
ffiReturn (FFIArrayOf _) = (const $ throwError $ DomainError "FFI return array") <$> FFI.retPtr FFI.retVoid
ffiReturn FFIString = (pure . vector . fmap Character) <$> FFI.retString
ffiReturn (FFIPointerTo typ) = (\ptr -> scalar <$> pointer typ ptr) <$> FFI.retPtr FFI.retVoid
ffiReturn (FFIStructOf typs cTyp _ (size, _) offsets _) = (\ptr -> bracket (pure ptr) (liftIO . free) $ const $ do
  vector . fmap box <$> zipWithM (\off typ -> ffiPeek typ $ ptr `plusPtr` off) offsets typs) <$> (FFI.RetType $ \write -> do
  p <- callocBytes size
  write cTyp p
  pure p)

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

parseFFI :: String -> St (Maybe FFIType)
parseFFI str = case p str of
  Just (ty, "") -> Just <$> ty
  _ -> pure Nothing
  where
  p :: String -> Maybe (St FFIType, String)
  p str | "[]" `isPrefixOf` str = first (fmap FFIArrayOf) <$> p (drop (length "[]") str)
  p str | "*" `isPrefixOf` str = first (fmap FFIPointerTo) <$> p (drop (length "*") str)
  p str | "{" `isPrefixOf` str = do
    let pTys acc s = case p s of {
      Nothing -> pure (acc, s) ;
      Just (ty, rest) -> if "," `isPrefixOf` rest then pTys (ty : acc) (drop (length ",") rest) else pure (ty : acc, rest)}
    (tys, rest) <- first (sequence . reverse) <$> pTys [] (drop (length "{") str)
    if "}" `isPrefixOf` rest then pure
      ( do
        tys' <- tys
        (cType, free, sizeAlignment, offsets, zero) <- structType tys'
        pure $ FFIStructOf tys' cType free sizeAlignment offsets zero
      , drop (length "}") rest
      )
    else Nothing
  p str | "void" `isPrefixOf` str = pure (pure FFIVoid, drop (length "void") str)
  p str | "u8" `isPrefixOf` str = pure (pure FFIUInt8, drop (length "u8") str)
  p str | "u16" `isPrefixOf` str = pure (pure FFIUInt16, drop (length "u16") str)
  p str | "u32" `isPrefixOf` str = pure (pure FFIUInt32, drop (length "u32") str)
  p str | "u64" `isPrefixOf` str = pure (pure FFIUInt64, drop (length "u64") str)
  p str | "i8" `isPrefixOf` str = pure (pure FFIInt8, drop (length "i8") str)
  p str | "i16" `isPrefixOf` str = pure (pure FFIInt16, drop (length "i16") str)
  p str | "i32" `isPrefixOf` str = pure (pure FFIInt32, drop (length "i32") str)
  p str | "i64" `isPrefixOf` str = pure (pure FFIInt64, drop (length "i64") str)
  p str | "uint" `isPrefixOf` str = pure (pure FFIUInt, drop (length "uint") str)
  p str | "int" `isPrefixOf` str = pure (pure FFIInt, drop (length "int") str)
  p str | "ulong" `isPrefixOf` str = pure (pure FFIULong, drop (length "ulong") str)
  p str | "long" `isPrefixOf` str = pure (pure FFILong, drop (length "long") str)
  p str | "usize" `isPrefixOf` str = pure (pure FFIUSize, drop (length "usize") str)
  p str | "f32" `isPrefixOf` str = pure (pure FFIFloat, drop (length "f32") str)
  p str | "f64" `isPrefixOf` str = pure (pure FFIDouble, drop (length "f64") str)
  p str | "complex32" `isPrefixOf` str = pure (pure FFIComplexFloat, drop (length "complex32") str)
  p str | "complex64" `isPrefixOf` str = pure (pure FFIComplexDouble, drop (length "complex64") str)
  p str | "c8" `isPrefixOf` str = pure (pure FFIChar8, drop (length "c8") str)
  p str | "c16" `isPrefixOf` str = pure (pure FFIChar16, drop (length "c16") str)
  p str | "c32" `isPrefixOf` str = pure (pure FFIChar32, drop (length "c32") str)
  p str | "uchar" `isPrefixOf` str = pure (pure FFIUChar, drop (length "uchar") str)
  p str | "char" `isPrefixOf` str = pure (pure FFIChar, drop (length "char") str)
  p str | "wchar" `isPrefixOf` str = pure (pure FFIWChar, drop (length "wchar") str)
  p str | "str" `isPrefixOf` str = pure (pure FFIString, drop (length "str") str)
  p _ = Nothing

doFFI :: Maybe String -> Noun -> St Noun
doFFI libName args' = do
  lib <- openDynLib $ fromMaybe crtPath libName
  args <- asStrings (DomainError "FFI right argument must be a vector of strings") args'
  when (length args < 2) $ throwError $ LengthError "FFI right argument must be at least length 2"
  let (ret : name : params) = args
  sym <- dynLibSym lib name
  ret' <- parseFFI ret >>= \case
    Just typ -> pure typ
    Nothing -> throwError $ DomainError "Invalid return type"
  params' <- forM params $ parseFFI >=> \case
    Just typ -> pure typ
    Nothing -> throwError $ DomainError "Invalid parameter type"
  pure $ scalar $ Wrap $ PrimitiveFunction (FunctionCalls (Just $ const $ \y -> do
    args <- fmap fromScalar <$> asVector (DomainError "FFI arguments must be a vector") y
    ffiFunc sym params' ret' args) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) (case libName of
    Just path -> "<" ++ ret ++ " " ++ path ++ ":" ++ name ++ "(" ++ intercalate ", " params ++ ")>"
    Nothing -> "<" ++ ret ++ " " ++ name ++ "(" ++ intercalate ", " params ++ ")>") Nothing

ffi :: Function
ffi = PrimitiveFunction (FunctionCalls (Just $ const $ doFFI Nothing) (Just $ const $ \x y -> do
  name <- asString (DomainError "FFI left argument must be a string") x
  doFFI (Just name) y) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) (G.quad : "FFI") Nothing

pointer :: FFIType -> Ptr a -> St ScalarValue
pointer typ ptr = do
  let peek = ffiPeek typ
  let poke = ffiPoke typ
  let size = ffiSize typ
  scope <- createRef $ Scope
    [ ("address", (VariableConstant, scalar $ Number $ (:+ 0) $ fromIntegral $ ptrToWordPtr ptr))
    , ("size", (VariableConstant, scalar $ Number $ (:+ 0) $ fromIntegral $ size))
    , (G.delta : "show", (VariableConstant, vector $ Character <$> ("<ptr 0x" ++ showHex (ptrToWordPtr ptr) "" ++ ">"))) ]
    [ ("Peek", (VariableConstant, PrimitiveFunction (FunctionCalls (Just $ const $ scalarMonad $ \y -> do
      let err = DomainError "Pointer peek offset must be an integer"
      offset <- asNumber err y >>= asInt err
      fmap toScalar $ peek $ plusPtr ptr $ offset * size) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) "Peek" Nothing))
    , ("PeekB", (VariableConstant, PrimitiveFunction (FunctionCalls (Just $ const $ scalarMonad $ \y -> do
      let err = DomainError "Pointer peek offset must be an integer"
      offset <- asNumber err y >>= asInt err
      fmap toScalar $ peek $ plusPtr ptr offset) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) "PeekB" Nothing))
    , ("PeekL", (VariableConstant, PrimitiveFunction (FunctionCalls (Just $ const $ \y -> do
      let err = DomainError "Pointer peek with length length must be a natural"
      len <- asScalar err y >>= asNumber err >>= asNat err
      ffiPeekArrayLen typ ptr $ fromIntegral len) (Just $ const $ \x y -> do
      let lenErr = DomainError "Pointer peek with length length must be a natural"
      let offsetErr = DomainError "Pointer peek with length offset must be an integer"
      offset <- asScalar offsetErr x >>= asNumber offsetErr >>= asInt offsetErr
      len <- asScalar lenErr y >>= asNumber lenErr >>= asNat lenErr
      ffiPeekArrayLen typ (plusPtr ptr $ offset * size) $ fromIntegral len) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) "PeekL" Nothing))
    , ("PeekE", (VariableConstant, PrimitiveFunction (FunctionCalls (Just $ const $ \y -> do
      let err = DomainError "Pointer peek to zero offset must be an integer"
      offset <- asScalar err y >>= asNumber err >>= asInt err
      ffiPeekArray0 typ $ plusPtr ptr $ offset * size) (Just $ const $ \x y -> do
      let err = DomainError "Pointer peek to end offset must be an integer"
      offset <- asScalar err x >>= asNumber err >>= asInt err
      ffiPeekArrayEnd typ (plusPtr ptr $ offset * size) y) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) "PeekE" Nothing))
    , ("Poke", (VariableConstant, PrimitiveFunction (FunctionCalls (Just $ const $ \y -> poke ptr y $> vector []) (Just $ const $ \x y -> do
      let err = DomainError "Pointer poke offset must be an integer"
      offset <- asScalar err x >>= asNumber err >>= asInt err
      poke (plusPtr ptr $ offset * size) y $> vector []) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) "Poke" Nothing))
    , ("PokeB", (VariableConstant, PrimitiveFunction (FunctionCalls Nothing (Just $ const $ \x y -> do
      let err = DomainError "Pointer poke offset must be an integer"
      offset <- asScalar err x >>= asNumber err >>= asInt err
      poke (plusPtr ptr offset) y $> vector []) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) "PokeB" Nothing))
    , ("PokeA", (VariableConstant, PrimitiveFunction (FunctionCalls (Just $ const $ \y -> ffiPokeArray typ ptr y $> vector []) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) "PokeA" Nothing))
    , ("PokeE", (VariableConstant, PrimitiveFunction (FunctionCalls (Just $ const $ \y -> ffiPokeArray0 typ ptr y $> vector []) (Just $ const $ \x y -> ffiPokeArrayEnd typ ptr x y $> vector []) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) "PokeE" Nothing))
    , ("AddB", (VariableConstant, PrimitiveFunction (FunctionCalls (Just $ const $ scalarMonad $ \y -> do
      let err = DomainError "Pointer add byte offset must be an integer"
      offset <- asNumber err y >>= asInt err
      pointer typ $ plusPtr ptr offset) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) "AddB" Nothing))
    , ("SubB", (VariableConstant, PrimitiveFunction (FunctionCalls (Just $ const $ scalarMonad $ \case
      (Number off) -> do
        offset <- asInt (DomainError "Pointer subtract byte offset must be an integer") off
        pointer typ $ plusPtr ptr $ negate offset
      pointerObj@(Struct _) -> do
        right <- pointerGet pointerObj
        pure $ Number $ (:+ 0) $ fromIntegral $ ptr `minusPtr` right
      _ -> throwError $ DomainError "Invalid arguments to pointer subtract byte") Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) "SubB" Nothing))
    , ("Cast", (VariableConstant, PrimitiveFunction (FunctionCalls (Just $ const $ \y -> do
      typ <- asString (DomainError "Pointer cast argument must be a string") y >>= parseFFI
      case typ of
        Nothing -> throwError $ DomainError "Invalid cast target type"
        Just ffiTyp -> scalar <$> pointer ffiTyp ptr) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) "Cast" Nothing))
    , (G.deltaBar : "Add", (VariableConstant, PrimitiveFunction (FunctionCalls Nothing (Just $ const $ \cases
      (Array [] [pointerObj@(Struct _)]) (Array [] [Number off]) -> do
        offset <- asInt (DomainError "Pointer add offset must be an integer") off
        p <- pointerGet pointerObj
        scalar <$> pointer typ (plusPtr p $ offset * size)
      (Array [] [Number off]) (Array [] [pointerObj@(Struct _)]) -> do
        offset <- asInt (DomainError "Pointer add offset must be an integer") off
        p <- pointerGet pointerObj
        scalar <$> pointer typ (plusPtr p $ offset * size)
      _ _ -> throwError $ DomainError "Invalid arguments to pointer add") Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) (G.deltaBar : "Add") Nothing)) 
    , (G.deltaBar : "Subtract", (VariableConstant, PrimitiveFunction (FunctionCalls Nothing (Just $ const $ \cases
      (Array [] [pointerObj@(Struct _)]) (Array [] [Number off]) -> do
        offset <- asInt (DomainError "Pointer subtract offset must be an integer") off
        p <- pointerGet pointerObj
        scalar <$> pointer typ (plusPtr p $ negate $ offset * size)
      (Array [] [leftObj@(Struct _)]) (Array [] [rightObj@(Struct _)]) -> do
        left <- pointerGet leftObj
        right <- pointerGet rightObj
        pure $ scalar $ Number $ (:+ 0) $ fromIntegral $ (`div` size) $ left `minusPtr` right
      _ _ -> throwError $ DomainError "Invalid arguments to pointer subtract") Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) (G.deltaBar : "Subtract") Nothing))
    , (G.deltaBar : "Increment", (VariableConstant, PrimitiveFunction (FunctionCalls (Just $ const $ \y -> do
      sc <- asScalar undefined y
      p <- pointerGet sc
      scalar <$> pointer typ (plusPtr p 1)) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) (G.deltaBar : "Increment") Nothing))
    , (G.deltaBar : "Decrement", (VariableConstant, PrimitiveFunction (FunctionCalls (Just $ const $ \y -> do
      sc <- asScalar undefined y
      p <- pointerGet sc
      scalar <$> pointer typ (plusPtr p $ -1)) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) (G.deltaBar : "Decrement") Nothing))
    , (G.deltaBar : "Sign", (VariableConstant, PrimitiveFunction (FunctionCalls (Just $ const $ \y -> do
      sc <- asScalar undefined y
      p <- pointerGet sc
      pure $ scalar $ boolToScalar $ castPtr p == nullPtr) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) (G.deltaBar : "Sign") Nothing)) ]
    [] [] Nothing True
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

doAlloc :: Int -> Noun -> St Noun
doAlloc count y = do
  typ <- asString (DomainError "Alloc right argument must be a string") y >>= parseFFI
  case typ of
    Nothing -> throwError $ DomainError "Invalid Alloc type"
    Just typ' -> do
      let size = ffiSize typ'
      ptr <- liftIO $ callocBytes $ count * size
      scalar <$> pointer typ' ptr

ffiStruct :: Nilad
ffiStruct = Nilad (Just $ do
  nullPointer <- pointer FFIUInt8 nullPtr
  scope <- createRef $ Scope
    [ ("null", (VariableConstant, scalar nullPointer)) ]
    [ ("New", (VariableConstant, PrimitiveFunction (FunctionCalls Nothing (Just $ const $ \x y -> do
      typ <- asString (DomainError "FFI New left argument must be a string") x >>= parseFFI
      case typ of
        Nothing -> throwError $ DomainError "Invalid New type"
        Just typ' -> ffiNew typ' y >>= fmap scalar . pointer typ' . fst) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) "New" Nothing))
    , ("NewA", (VariableConstant, PrimitiveFunction (FunctionCalls Nothing (Just $ const $ \x y -> do
      typ <- asString (DomainError "FFI NewA left argument must be a string") x >>= parseFFI
      case typ of
        Nothing -> throwError $ DomainError "Invalid NewA type"
        Just typ' -> ffiNewArray typ' y >>= fmap scalar . pointer typ' . fst) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) "NewA" Nothing))
    , ("NewZ", (VariableConstant, PrimitiveFunction (FunctionCalls Nothing (Just $ const $ \x y -> do
      typ <- asString (DomainError "FFI NewZ left argument must be a string") x >>= parseFFI
      case typ of
        Nothing -> throwError $ DomainError "Invalid NewZ type"
        Just typ' -> ffiNewArray0 typ' y >>= fmap scalar . pointer typ' . fst) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) "NewZ" Nothing))
    , ("Alloc", (VariableConstant, PrimitiveFunction (FunctionCalls (Just $ const $ doAlloc 1) (Just $ const $ \x y -> do
      let err = DomainError "FFI Alloc left argument must be a natural"
      count <- asScalar err x >>= asNumber err >>= asNat err
      doAlloc (fromIntegral count) y) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) "Alloc" Nothing))
    , ("Free", (VariableConstant, PrimitiveFunction (FunctionCalls (Just $ const $ \y -> do
      ptr <- asScalar (DomainError "FFI Free argument must be a pointer object") y >>= pointerGet
      liftIO $ free ptr
      pure $ vector []) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) "Free" Nothing))
    , ("SizeOf", (VariableConstant, PrimitiveFunction (FunctionCalls (Just $ const $ \y -> do
      (sh, typs) <- asArrayOfStrings (DomainError "FFI SizeOf argument must be a string or array of strings") y
      mapM parseFFI typs <&> sequence >>= \case
        Nothing -> throwError$ DomainError "Invalid SizeOf type"
        Just typs' -> pure $ Array sh $ Number . (:+ 0) . fromIntegral . ffiSize <$> typs') Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) "SizeOf" Nothing)) ]
    [] [] Nothing True
  ctx <- getContext
  pure $ scalar $ Struct $ ctx{ contextScope = scope }) Nothing (G.quad : "ffi") Nothing
#endif
