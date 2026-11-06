{-# LANGUAGE CPP, LambdaCase, OverloadedStrings, ScopedTypeVariables, FlexibleContexts, MagicHash #-}

module TinyAPL.CLI where

#if !defined(TINYAPL_NO_REPLXX) && defined(wasm32_HOST_ARCH)
#define TINYAPL_NO_REPLXX "wasm"
#endif
import TinyAPL.Noun
import TinyAPL.Context
import TinyAPL.Quads
import TinyAPL.CoreQuads
import TinyAPL.Error
import TinyAPL.Util
import TinyAPL.Pretty
import qualified TinyAPL.Files as F
import qualified TinyAPL.Glyphs as G
import qualified TinyAPL.Primitives as P
import TinyAPL.Interpreter
import TinyAPL.Keymaps
import TinyAPL.Quads.File (file)
#ifndef wasm32_HOST_ARCH
import TinyAPL.Quads.FFI (ffi, ffiStruct)
#endif

import Prelude hiding (getLine, putStr, putStrLn)
import Control.Monad (void, when, unless)
import System.IO hiding (getLine, putStr, putStrLn, hPutStr, hPutStrLn)
import qualified System.IO
import Data.IORef
import Data.Maybe
import Data.List
import System.Info
import System.Exit
import Control.DeepSeq
import Control.Exception (displayException, SomeException, catch)
import System.Directory
import Data.String
import qualified Options.Applicative as Opts
import TinyAPL.Highlighter
import qualified Data.Text as T
#if !defined(TINYAPL_NO_REPLXX)
import qualified System.Console.ReplXX as XX
#endif
#if defined(mingw32_HOST_OS)
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Utils
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.ByteString.Char8 as BS
#endif

#if defined(mingw32_HOST_OS)
foreign import ccall "tinyapl_setup" platformSetup :: IO ()
foreign import ccall "tinyapl_read" read_ :: FunPtr (CString -> CSize -> IO ()) -> IO ()
foreign import ccall "tinyapl_write" write_ :: CString -> CSize -> CBool -> IO ()
foreign import ccall "wrapper" mkReadCallback :: (CString -> CSize -> IO ()) -> IO (FunPtr (CString -> CSize -> IO ()))

getLine :: IO String
getLine = do
  str <- newIORef (nullPtr, 0)
  cb <- mkReadCallback $ \ptr sz -> writeIORef str (ptr, fromIntegral sz)
  read_ cb
  bs <- BS.packCStringLen =<< readIORef str
  pure $ T.unpack $ T.decodeUtf8With T.lenientDecode bs

putStrOf :: Bool -> String -> IO ()
putStrOf err str = do
  let bs = T.encodeUtf8 $ T.pack str
  BS.useAsCStringLen bs $ \(ptr, sz) -> write_ ptr (fromIntegral sz) (fromBool err)

putStr :: String -> IO ()
putStr = putStrOf False

putStrLn :: String -> IO ()
putStrLn = putStrOf False . (++ "\n")

ePutStr :: String -> IO ()
ePutStr = putStrOf True

ePutStrLn :: String -> IO ()
ePutStrLn = putStrOf True . (++ "\n")
#else
platformSetup :: IO ()
platformSetup = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

getLine :: IO String
getLine = System.IO.getLine

putStr :: String -> IO ()
putStr = System.IO.putStr

putStrLn :: String -> IO ()
putStrLn = System.IO.putStrLn

ePutStr :: String -> IO ()
ePutStr = System.IO.hPutStr stderr

ePutStrLn :: String -> IO ()
ePutStrLn = System.IO.hPutStrLn stderr
#endif

data Allowed
  = Allowed
    { allowedFileImport :: Bool
    , allowedFFI :: Bool
    , allowedFileSystem :: Bool }

data CommonOptions
  = CommonOptions
    { noPretty :: Bool }

data InnerOptions
  = ReplOptions
    { replPrefixKey :: Char
    , replPlain :: Bool
    , replKeymap :: Keymap }
  | FileOptions
    { fileEchoLast :: Bool
    , filePath :: FilePath }

data Options = Options Allowed CommonOptions InnerOptions

instance IsString Char where
  fromString = headPromise

options :: Opts.Parser Options
options = Options
  <$> allowed 
  <*> (CommonOptions
    <$> Opts.switch
      (  Opts.long "no-pretty"
      <> Opts.help "Disable pretty-printing"))
  <*> (ReplOptions
    <$> Opts.strOption
      (  Opts.long "prefix"
      <> Opts.help "Prefix key for entering glyphs"
      <> Opts.metavar "PREFIX"
      <> Opts.value '`')
    <*> Opts.switch
      (  Opts.long "plain"
      <> Opts.short 'Z'
      <> Opts.help "Disable all fancy I/O")
    <*> Opts.option Opts.auto
      (  Opts.long "keymap"
      <> Opts.help "Selected keymap for entering glyphs"
      <> Opts.metavar "KEYMAP"
      <> Opts.value UsIntl)
    Opts.<|> FileOptions
    <$> Opts.switch
      (  Opts.long "echo-last"
      <> Opts.short 'E'
      <> Opts.help "Echo the result of the last expression" )
    <*> Opts.argument Opts.str (Opts.metavar "FILE"))
  where
    allowed = Opts.flag' (Allowed True True True)
      (  Opts.long "allow-all"
      <> Opts.short 'A'
      <> Opts.help "Allow all features" )
      Opts.<|> Allowed
        <$> Opts.switch
          (  Opts.long "allow-import"
          <> Opts.help "Allow importing from the filesystem" )
        <*> Opts.switch
          (  Opts.long "allow-ffi"
          <> Opts.help "Allow using the foreign function interface" )
        <*> Opts.switch
          (  Opts.long "allow-fs"
          <> Opts.help "Allow reading and writing to the filesystem" )

readImportFile :: FilePath -> St String
readImportFile path = liftToSt $ readFile path

stdin :: Nilad
stdin = Nilad (Just $ let
  go text = do
    closed <- liftToSt $ hIsClosed System.IO.stdin >>= (\x -> rnf x `seq` pure x)
    if closed then pure text else do
      done <- liftToSt $ isEOF >>= (\x -> rnf x `seq` pure x)
      if done then pure text
      else do
        ch <- liftToSt $ getChar >>= (\x -> rnf x `seq` pure x)
        go $ ch : text
  in vector . fmap Character . reverse <$> go "") Nothing (G.quad : "stdin") Nothing

ffiQuads :: Quads
#ifdef wasm32_HOST_ARCH
ffiQuads = mempty
#else
ffiQuads = quadsFromReprs [ ffiStruct ] [ ffi ] [] []
#endif

ansiRed :: String -> String
ansiRed str = "\x1b[31m" ++ str ++ "\x1b[0m"

cli :: IO ()
cli = do
  cwd <- getCurrentDirectory

  scope <- newIORef $ Scope [] [] [] [] Nothing True

  id <- newIORef 0

  pretty <- newIORef defaultConfig

  Options allowed (CommonOptions ugly) inner <- Opts.execParser $ Opts.info (Opts.helper <*> options) Opts.fullDesc
  let context = Context {
      contextScope = scope
    , contextQuads = core 
      <> (if allowedFFI allowed then ffiQuads else mempty)
      <> quadsFromReprs [ makeSystemInfo os arch False bigEndian, TinyAPL.CLI.stdin, boxes ] [] [] []
      <> (if allowedFileSystem allowed then quadsFromReprs [ file ] [] [] [] else mempty)
      <> quadsFromReprs [] [ makeImport (if allowedFileImport allowed then Just readImportFile else Nothing) Nothing ] [] []
    , contextIn = \pr _ -> do
      liftToSt $ ePutStr pr
      liftToSt $ hFlush stderr
      liftToSt getLine
    , contextOut = \str -> do
      liftToSt $ putStr str
      liftToSt $ hFlush stdout
    , contextErr = \str -> do
      liftToSt $ ePutStr str
      liftToSt $ hFlush stderr
    , contextIncrementalId = id
    , contextDirectory = cwd
    , contextParsingInfo = case P.primitives of (n, f, a, c) -> ParsingInfo n f a c Nothing
    , contextPretty = pretty
    , contextUgly = ugly }

  case inner of
    ReplOptions prefixKey plain keymap -> repl context prefixKey keymap plain
    FileOptions echo path -> do
      platformSetup
      code <- F.readUtf8 path
      void $ runCode echo path code context

runCode :: Bool -> String -> String -> Context -> IO Context
runCode output file code context = do
  (result, context') <- fmap fromRight' $ runResult $ flip runSt context $ runAndCatch $ run' file code
  case result of
    Panicked ex -> ePutStrLn $ ansiRed $ show $ HaskellError $ displayException ex
    Thrown err -> ePutStrLn $ ansiRed $ show err
    Succeeded res ->
      when output $ void $ runResult $ flip runSt context $ do
        str <- runPretty' res
        liftToSt $ putStrLn str
  pure context'

#if !defined(TINYAPL_NO_REPLXX)
charInserted :: XX.ReplXX -> Bool -> Char -> IO ()
charInserted xx replace' ch = do
  (XX.State t0 pos) <- XX.getState xx
  let replace = if pos == 0 then False else replace'
  let (bef, aft) = T.splitAt (if replace then fromIntegral pos - 1 else fromIntegral pos) t0
  let t1 = bef <> T.cons ch (if replace then T.drop 1 aft else aft)
  XX.setState xx $ XX.State t1 $ if replace then pos else pos + 1
#endif

repl :: Context -> Char -> Keymap -> Bool -> IO ()
repl context _ _ True = let
  go context = do
    line <- (Just <$> getLine) `catch` (\(_ :: SomeException) -> pure Nothing)
    case line of
      Nothing -> pure ()
      Just line' -> runCode True "<repl>" line' context >>= go
  in platformSetup *> go context
repl context prefixKey keymap False = let
#if !defined(TINYAPL_NO_REPLXX)
  go :: XX.ReplXX -> Context -> IO ()
#else
  go :: () -> Context -> IO ()
#endif
  go xx context = do
#if !defined(TINYAPL_NO_REPLXX)
    line <- T.unpack <$> XX.input xx "      "
#else
    putStr "      "
    line <- getLine
#endif
    unless (null line) $ runCode True "<repl>" line context >>= go xx
  in do
    putStrLn "TinyAPL REPL, empty line to exit"
#if !defined(TINYAPL_NO_REPLXX)
    (singleCharacters, doubleCharacters) <- case liftA2 (,) (singleChars keymap) (doubleChars keymap) of
                             Just c -> pure c
                             Nothing -> (do
                                ePutStrLn "You've attempted to use an existing, yet unimplemented keymap! This should not be possible. Please report this as a bug."
                                die "Keymap exists but is not implemented")
    xx <- XX.new
    XX.bindKey xx (fromIntegral $ fromEnum prefixKey) $ const $ do
      ch <- XX.unsafeReadChar xx
      if ch == prefixKey then do
        ch2 <- XX.unsafeReadChar xx
        charInserted xx False $ fromMaybe ch2 $ lookup ch2 doubleCharacters
      else charInserted xx False $ fromMaybe ch $ lookup ch singleCharacters
      pure XX.Continue
    let hlc t = pure $ (\case {
      CNumber -> XX.Style Nothing (Just XX.Red) False False ;
      CString -> XX.Style Nothing (Just XX.Cyan) False False ;
      CStringEscape -> XX.Style Nothing (Just XX.Blue) False False ;
      CArrayName -> XX.Style Nothing (Just XX.Red) False False ;
      CPrimArray -> XX.Style Nothing (Just XX.Red) False False ;
      CFunctionName -> XX.Style Nothing (Just XX.Green) False False ;
      CPrimFunction -> XX.Style Nothing (Just XX.Green) False False ;
      CAdverbName -> XX.Style Nothing (Just XX.Magenta) False False ;
      CPrimAdverb -> XX.Style Nothing (Just XX.Magenta) False False ;
      CConjunctionName -> XX.Style Nothing (Just XX.Yellow) False False ;
      CPrimConjunction -> XX.Style Nothing (Just XX.Yellow) False False ;
      CComment -> XX.Style Nothing (Just XX.Gray) False False ;
      _ -> XX.Style Nothing Nothing False False }) <$> highlight (T.unpack t)
    XX.setHighlighterCallback xx hlc
    go xx context
      { contextIn = \pr hl -> liftToSt $ do
        unless hl $ XX.clearHighlighterCallback xx
        i <- T.unpack <$> XX.input xx (T.pack pr)
        unless hl $ XX.setHighlighterCallback xx hlc
        pure i
      , contextOut = liftToSt . XX.write xx . T.pack
      , contextErr = liftToSt . XX.write xx . T.pack
      }
#else
    go () context
#endif
