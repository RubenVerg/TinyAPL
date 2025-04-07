{-# LANGUAGE FlexibleContexts, TypeApplications #-}
module TinyAPL.CoreQuads.Unicode where

import TinyAPL.ArrayFunctionOperator
import TinyAPL.Error
import qualified TinyAPL.Glyphs as G
import TinyAPL.Complex

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Serialize as S
import Data.Word
import Control.Monad.Catch
import Control.Monad.Except (MonadError)
import Control.Exception (evaluate)
import Numeric.Natural
import Control.Monad
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Char (ord, chr)

getAllOf :: S.Get a -> S.Get [a]
getAllOf g = do
  e <- S.isEmpty
  if e then pure []
  else liftA2 (:) g (getAllOf g)

toUtf8 :: MonadError Error m => String -> m [Word8]
toUtf8 str = do
  let text = T.pack str
  let bs = T.encodeUtf8 text
  case S.runGet (getAllOf S.getWord8) bs of
    Right codes -> pure codes
    Left err -> throwError $ DomainError $ "Error in encoding: " ++ err

toUtf16 :: MonadError Error m => String -> m [Word16]
toUtf16 str = do
  let text = T.pack str
  let bs = T.encodeUtf16BE text
  case S.runGet (getAllOf S.getWord16be) bs of
    Right codes -> pure codes
    Left err -> throwError $ DomainError $ "Error in encoding: " ++ err

toUtf32 :: MonadError Error m => String -> m [Word32]
toUtf32 str = do
  let text = T.pack str
  let bs = T.encodeUtf32BE text
  case S.runGet (getAllOf S.getWord32be) bs of
    Right codes -> pure codes
    Left err -> throwError $ DomainError $ "Error in encoding: " ++ err

putAllOf :: S.Putter a -> S.Putter [a]
putAllOf _ [] = pure ()
putAllOf p (x:xs) = p x *> putAllOf p xs

fromUtf8 :: (MonadError Error m, MonadIO m, MonadCatch m) => [Word8] -> m String
fromUtf8 codes = do
  let bs = S.runPut $ putAllOf S.putWord8 codes
  text <- handle (\(T.DecodeError e _) -> throwError $ DomainError $ "Error in decoding: " ++ e) $ liftIO $ evaluate (T.decodeUtf8With T.strictDecode bs)
  pure $ T.unpack text

fromUtf16 :: (MonadError Error m, MonadIO m, MonadCatch m) => [Word16] -> m String
fromUtf16 codes = do
  let bs = S.runPut $ putAllOf S.putWord16be codes
  text <- handle (\(T.DecodeError e _) -> throwError $ DomainError $ "Error in decoding: " ++ e) $ liftIO $ evaluate (T.decodeUtf16BEWith T.strictDecode bs)
  pure $ T.unpack text

fromUtf32 :: (MonadError Error m, MonadIO m, MonadCatch m) => [Word32] -> m String
fromUtf32 codes = do
  let bs = S.runPut $ putAllOf S.putWord32be codes
  text <- handle (\(T.DecodeError e _) -> throwError $ DomainError $ "Error in decoding: " ++ e) $ liftIO $ evaluate (T.decodeUtf32BEWith T.strictDecode bs)
  pure $ T.unpack text

unicodeS :: MonadError Error m => ScalarValue -> m ScalarValue
unicodeS (Number y) = do
  let err = DomainError $ G.quad : "Unicode argument must be a natural or character"
  n <- asNat err y
  pure $ Character $ chr $ fromIntegral n
unicodeS (Character y) = pure $ Number $ (fromIntegral $ ord y) :+ 0
unicodeS _ = throwError $ DomainError $ G.quad : "Unicode argument must be a natural or character"

data EncodeDecodeType
  = Utf8
  | Utf16
  | Utf32
  deriving (Eq, Ord, Enum, Bounded)

encode :: MonadError Error m => EncodeDecodeType -> String -> m [Natural]
encode Utf8 str = fmap (fromInteger . toInteger) <$> toUtf8 str
encode Utf16 str = fmap (fromInteger . toInteger) <$> toUtf16 str
encode Utf32 str = fmap (fromInteger . toInteger) <$> toUtf32 str

decode :: (MonadError Error m, MonadIO m, MonadCatch m) => EncodeDecodeType -> [Natural] -> m String
decode Utf8 codes = fromUtf8 $ fromInteger . toInteger <$> codes
decode Utf16 codes = fromUtf16 $ fromInteger . toInteger <$> codes
decode Utf32 codes = fromUtf32 $ fromInteger . toInteger <$> codes

encodeDecode :: (MonadError Error m, MonadIO m, MonadCatch m) => Noun -> Noun -> m Noun
encodeDecode t arr = do
  let terr = DomainError $ G.quad : "Unicode left argument must be one of 8, 16 or 32"
  t' <- asScalar terr t >>= asNumber terr >>= asInt terr
  ed <- case t' of
    8 -> pure Utf8
    16 -> pure Utf16
    32 -> pure Utf32
    _ -> throwError terr
  let derr = DomainError $ G.quad : "Unicode right argument must be a string or natural vector"
  dat <- asVector derr arr
  if all isNumber dat then do
    codes <- mapM (asNumber derr >=> asNat derr) dat
    vector . fmap Character <$> decode ed codes
  else if all isCharacter dat then do
    chars <- mapM (asCharacter derr) dat
    vector . fmap (Number . (:+ 0) . fromInteger . toInteger) <$> encode ed chars
  else throwError derr

unicode = PrimitiveFunction (Just $ const $ scalarMonad unicodeS) (Just $ const encodeDecode) Nothing Nothing Nothing Nothing Nothing Nothing (G.quad : "Unicode") Nothing