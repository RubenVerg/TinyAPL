{-# LANGUAGE PatternSynonyms, DerivingStrategies, GeneralizedNewtypeDeriving #-}

module System.Console.ReplXX
   where

import Control.Monad (void)
import Foreign hiding (void)
import Foreign.C

import Data.Text (Text)
import qualified Data.Text.Foreign as T

#include "replxx.h"

data ReplXX'
type RXX = Ptr ReplXX'

newtype ReplXX = ReplXX (ForeignPtr ReplXX')

withReplXX0 :: (RXX -> IO r) -> ReplXX -> IO r
withReplXX0 m (ReplXX f) = withForeignPtr f m

withReplXX1 :: (RXX -> a -> IO r) -> ReplXX -> a -> IO r
withReplXX1 m (ReplXX f) a = withForeignPtr f (\rxx -> m rxx a)

withReplXX2 :: (RXX -> a -> b -> IO r) -> ReplXX -> a -> b -> IO r
withReplXX2 m (ReplXX f) a b = withForeignPtr f (\rxx -> m rxx a b)

withReplXX3 :: (RXX -> a -> b -> c -> IO r) -> ReplXX -> a -> b -> c -> IO r
withReplXX3 m (ReplXX f) a b c = withForeignPtr f (\rxx -> m rxx a b c)

foreign import ccall replxx_init :: IO RXX
foreign import ccall "&" replxx_end :: FunPtr (RXX -> IO ())
foreign import ccall replxx_set_prompt :: RXX -> CString -> IO ()
foreign import ccall replxx_bind_key :: (RXX -> CInt -> FunPtr (KeyPressHandler' userData) -> Ptr userData -> IO ())
foreign import ccall replxx_set_highlighter_callback :: (RXX -> FunPtr (HighlighterCallback_' userData) -> Ptr userData -> IO ())
foreign import ccall replxx_get_state :: RXX -> Ptr State_ -> IO ()
foreign import ccall replxx_set_state :: RXX -> Ptr State_ -> IO ()
foreign import ccall replxx_set_modify_callback :: (RXX -> FunPtr (ModifyCallback_' userData) -> Ptr userData -> IO ())
foreign import ccall replxx_input :: RXX -> CString -> IO CString
foreign import ccall replxx_read_char :: RXX -> IO Char
foreign import ccall replxx_write :: RXX -> CString -> CInt -> IO CInt
foreign import ccall replxx_color_combine :: Color_ -> Color_ -> Color_
foreign import ccall replxx_color_bg :: Color_ -> Color_
foreign import ccall replxx_color_bold :: Color_ -> Color_
foreign import ccall replxx_color_underline :: Color_ -> Color_
foreign import ccall replxx_color_grayscale :: CInt -> Color_
foreign import ccall replxx_color_rgb666 :: CInt -> CInt -> CInt -> Color_
foreign import ccall "wrapper" wrapper_keyPressHandler :: (KeyPressHandler' userData -> IO (FunPtr (KeyPressHandler' userData)))
foreign import ccall "wrapper" wrapper_highlighterCallback_ :: (HighlighterCallback_' userData -> IO (FunPtr (HighlighterCallback_' userData)))
foreign import ccall "wrapper" wrapper_modifyCallback_ :: (ModifyCallback_' userData -> IO (FunPtr (ModifyCallback_' userData)))

new :: IO ReplXX
new = do
  rxx <- replxx_init
  f <- newForeignPtr replxx_end rxx
  pure $ ReplXX f

setPrompt :: ReplXX -> Text -> IO ()
setPrompt = withReplXX1 $ \rxx prompt -> T.withCString prompt $ replxx_set_prompt rxx

newtype ActionResult = ActionResult (#type ReplxxActionResult) deriving newtype (Eq, Storable)

pattern Continue, Return, Bail :: ActionResult
pattern Continue = ActionResult (#const REPLXX_ACTION_RESULT_CONTINUE)
pattern Return = ActionResult (#const REPLXX_ACTION_RESULT_RETURN)
pattern Bail = ActionResult (#const REPLXX_ACTION_RESULT_BAIL)
{-# COMPLETE Continue, Return, Bail #-}

data Key

type KeyPressHandler' userData = CInt -> Ptr userData -> IO ActionResult
type KeyPressHandler = CInt -> IO ActionResult

bindKey' :: ReplXX -> CInt -> KeyPressHandler' userData -> Ptr userData -> IO ()
bindKey' = withReplXX3 $ \rxx code handler userData -> do
  handler' <- wrapper_keyPressHandler handler
  replxx_bind_key rxx code handler' userData

bindKey :: ReplXX -> CInt -> KeyPressHandler -> IO ()
bindKey r code handler = bindKey' r code (const . handler) nullPtr

newtype Color_ = Color_ (#type ReplxxColor) deriving newtype (Eq, Show, Storable)

data Channel
  = Ch0
  | Ch1
  | Ch2
  | Ch3
  | Ch4
  | Ch5
  deriving (Eq, Ord, Show, Enum, Bounded)

newtype Level = Level CInt
  deriving newtype (Eq, Ord)
  deriving stock (Show)

instance Enum Level where
  fromEnum (Level i) = fromEnum i
  toEnum i | i >= 0 && i < 24 = Level $ toEnum i
  toEnum _ = error "toEnum @Level: out of range"

instance Bounded Level where
  minBound = Level 0
  maxBound = Level 23

data Color
  = Black
  | Red
  | Green
  | Brown
  | Blue
  | Magenta
  | Cyan
  | LightGray
  | Gray
  | BrightRed
  | BrightGreen
  | Yellow
  | BrightBlue
  | BrightMagenta
  | BrightCyan
  | White
  | Grayscale Level
  | RGB Channel Channel Channel
  deriving (Eq, Show)

colorToColor_ :: Color -> Color_
colorToColor_ Black = Color_ (#const REPLXX_COLOR_BLACK)
colorToColor_ Red = Color_ (#const REPLXX_COLOR_RED)
colorToColor_ Green = Color_ (#const REPLXX_COLOR_GREEN)
colorToColor_ Brown = Color_ (#const REPLXX_COLOR_BROWN)
colorToColor_ Blue = Color_ (#const REPLXX_COLOR_BLUE)
colorToColor_ Magenta = Color_ (#const REPLXX_COLOR_MAGENTA)
colorToColor_ Cyan = Color_ (#const REPLXX_COLOR_CYAN)
colorToColor_ LightGray = Color_ (#const REPLXX_COLOR_LIGHTGRAY)
colorToColor_ Gray = Color_ (#const REPLXX_COLOR_GRAY)
colorToColor_ BrightRed = Color_ (#const REPLXX_COLOR_BRIGHTRED)
colorToColor_ BrightGreen = Color_ (#const REPLXX_COLOR_BRIGHTGREEN)
colorToColor_ Yellow = Color_ (#const REPLXX_COLOR_YELLOW)
colorToColor_ BrightBlue = Color_ (#const REPLXX_COLOR_BRIGHTBLUE)
colorToColor_ BrightMagenta = Color_ (#const REPLXX_COLOR_BRIGHTMAGENTA)
colorToColor_ BrightCyan = Color_ (#const REPLXX_COLOR_BRIGHTCYAN)
colorToColor_ White = Color_ (#const REPLXX_COLOR_WHITE)
colorToColor_ (Grayscale (Level i)) = replxx_color_grayscale i
colorToColor_ (RGB r g b) = replxx_color_rgb666 (fromIntegral $ fromEnum r) (fromIntegral $ fromEnum g) (fromIntegral $ fromEnum b)

data Style = Style
  { background :: Maybe Color
  , foreground :: Maybe Color
  , bold :: Bool
  , underline :: Bool
  }

styleToColor_ :: Style -> Color_
styleToColor_ (Style bg fg b u) = let
  col = case (bg, fg) of
    (Nothing, Nothing) -> Color_ (#const REPLXX_COLOR_DEFAULT)
    (Just bg', Nothing) -> replxx_color_bg $ colorToColor_ bg'
    (Nothing, Just fg') -> colorToColor_ fg'
    (Just bg', Just fg') -> replxx_color_combine (colorToColor_ fg') (replxx_color_bg $ colorToColor_ bg')
  in (if b then replxx_color_bold else id) $ (if u then replxx_color_underline else id) col

type HighlighterCallback_' userData = CString -> Ptr Color_ -> CInt -> Ptr userData -> IO ()
type HighlighterCallback' userData = Text -> Ptr userData -> IO [Style]
type HighlighterCallback = Text -> IO [Style]

setHighlighterCallback' :: ReplXX -> HighlighterCallback' userData -> Ptr userData -> IO ()
setHighlighterCallback' = withReplXX2 $ \rxx cb userData -> do
  cb' <- wrapper_highlighterCallback_ $ \i o _ d -> do
    inp <- T.peekCString i
    outp <- cb inp d
    pokeArray o $ styleToColor_ <$> outp
  replxx_set_highlighter_callback rxx cb' userData

setHighlighterCallback :: ReplXX -> HighlighterCallback -> IO ()
setHighlighterCallback r cb = setHighlighterCallback' r (const . cb) nullPtr

clearHighlighterCallback :: ReplXX -> IO ()
clearHighlighterCallback = withReplXX0 $ \rxx -> replxx_set_highlighter_callback rxx nullFunPtr nullPtr

data State_ = State_ CString CInt

instance Storable State_ where
  sizeOf _ = (#size ReplxxState)
  alignment _ = (#alignment ReplxxState)
  peek ptr = State_ <$> (#peek ReplxxState, text) ptr <*> (#peek ReplxxState, cursorPosition) ptr
  poke ptr (State_ t c) = (#poke ReplxxState, text) ptr t *> (#poke ReplxxState, cursorPosition) ptr c

data State = State Text CInt deriving (Eq, Show)

getState :: ReplXX -> IO State
getState = withReplXX0 $ \rxx -> with (State_ nullPtr 0) $ \p -> do
  replxx_get_state rxx p
  (State_ t c) <- peek p
  flip State c <$> T.peekCString t

setState :: ReplXX -> State -> IO ()
setState = withReplXX1 $ \rxx (State t c) -> T.withCString t $ \t' -> with (State_ t' c) $ \p -> replxx_set_state rxx p

type ModifyCallback_' userData = Ptr CString -> Ptr CInt -> Ptr userData -> IO ()
type ModifyCallback' userData = State -> (State -> IO ()) -> Ptr userData -> IO ()
type ModifyCallback = State -> (State -> IO ()) -> IO ()

setModifyCallback' :: ReplXX -> ModifyCallback' userData -> Ptr userData -> IO ()
setModifyCallback' = withReplXX2 $ \rxx cb userData -> do
  cb' <- wrapper_modifyCallback_ $ \t c d -> do
    t0 <- peek t >>= T.peekCString
    c0 <- peek c
    cb (State t0 c0) (\(State t1 c1) -> do
      peek t >>= free
      t' <- mallocBytes $ T.lengthWord8 t1 + 1
      T.unsafeCopyToPtr t1 t'
      pokeByteOff t' (T.lengthWord8 t1) (0 :: Word8)
      poke t $ castPtr t'
      poke c c1) d
  replxx_set_modify_callback rxx cb' userData

setModifyCallback :: ReplXX -> ModifyCallback -> IO ()
setModifyCallback r cb = setModifyCallback' r (\s0 s _ -> cb s0 s) nullPtr

input :: ReplXX -> Text -> IO Text
input = withReplXX1 $ \rxx prompt -> T.withCString prompt $ \p -> replxx_input rxx p >>= T.peekCString

unsafeReadChar :: ReplXX -> IO Char
unsafeReadChar = withReplXX0 replxx_read_char

write :: ReplXX -> Text -> IO ()
write = withReplXX1 $ \rxx t -> T.withCStringLen t $ \(p, l) -> void $ replxx_write rxx p $ fromIntegral l
