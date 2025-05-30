{-# LANGUAGE CPP, LambdaCase, OverloadedStrings, ScopedTypeVariables #-}

#if defined(unix_HOST_OS) || defined(__unix___HOST_OS) || defined(__unix_HOST_OS) || defined(linux_HOST_OS) || defined(__linux___HOST_OS) || defined(__linux_HOST_OS) || defined(darwin_HOST_OS)
#define is_linux 1
#endif

module TinyAPL.CLI where

import TinyAPL.Noun
import TinyAPL.Context
import TinyAPL.Quads
import TinyAPL.CoreQuads
import TinyAPL.Error
import TinyAPL.Util
import qualified TinyAPL.Files as F
import qualified TinyAPL.Glyphs as G
import qualified TinyAPL.Primitives as P
import TinyAPL.Interpreter
import TinyAPL.Quads.File (file)
#ifndef wasm32_HOST_ARCH
import TinyAPL.Quads.FFI (ffi, ffiStruct)
#endif

import Control.Monad (void, when)
import System.IO
import Data.IORef
import Data.List
import System.Info
import Control.DeepSeq
import Control.Exception (displayException, SomeException, catch)
import System.Directory
import qualified Options.Applicative as Opts
import Data.String
#ifdef is_linux
import TinyAPL.Highlighter
import qualified System.Console.Edited as E
#endif

data Allowed
  = Allowed
    { allowedFileImport :: Bool
    , allowedFFI :: Bool
    , allowedFileSystem :: Bool }

data InnerOptions
  = ReplOptions
    { replPrefixKey :: Char
    , replPlain :: Bool }
  | FileOptions
    { fileEchoLast :: Bool
    , filePath :: FilePath }

data Options = Options Allowed InnerOptions

instance IsString Char where
  fromString = headPromise

options :: Opts.Parser Options
options = Options
  <$> allowed 
  <*> (ReplOptions
    <$> Opts.strOption
      (  Opts.long "prefix"
      <> Opts.help "Prefix key for entering glyphs"
      <> Opts.metavar "PREFIX"
      <> Opts.value defaultPrefixKey )
    <*> Opts.switch
      (  Opts.long "plain"
      <> Opts.short 'Z'
      <> Opts.help "Disable all fancy I/O")
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

defaultPrefixKey :: Char
defaultPrefixKey = '`'

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
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  cwd <- getCurrentDirectory

  scope <- newIORef $ Scope [] [] [] [] Nothing True

  id <- newIORef 0

  Options allowed inner <- Opts.execParser $ Opts.info (Opts.helper <*> options) Opts.fullDesc
  
  let context = Context {
      contextScope = scope
    , contextQuads = core 
      <> (if allowedFFI allowed then ffiQuads else mempty)
      <> quadsFromReprs [ makeSystemInfo os arch False bigEndian, TinyAPL.CLI.stdin ] [] [] []
      <> (if allowedFileSystem allowed then quadsFromReprs [ file ] [] [] [] else mempty)
      <> quadsFromReprs [] [ makeImport (if allowedFileImport allowed then Just readImportFile else Nothing) Nothing ] [] []
    , contextIn = liftToSt getLine
    , contextOut = \str -> do
      liftToSt $ putStr str
      liftToSt $ hFlush stdout
    , contextErr = \str -> do
      liftToSt $ hPutStr stderr str
      liftToSt $ hFlush stderr
    , contextIncrementalId = id
    , contextDirectory = cwd
    , contextPrimitives = P.primitives }

  case inner of
    ReplOptions prefixKey plain -> repl context prefixKey plain
    FileOptions echo path -> do
      code <- F.readUtf8 path
      void $ runCode echo path code context

runCode :: Bool -> String -> String -> Context -> IO Context
runCode output file code context = do
  (result, context') <- fmap fromRight' $ runResult $ flip runSt context $ runAndCatch $ run' file code
  case result of
    Panicked ex -> hPutStrLn stderr $ ansiRed $ show $ HaskellError $ displayException ex
    Thrown err -> hPutStrLn stderr $ ansiRed $ show err
    Succeeded res ->
      when output $ void $ runResult $ flip runSt context $ do
        str <- showM res
        liftToSt $ putStrLn str
  pure context'

singleCharacters :: [(Char, Char)]
singleCharacters =
  [ ('1', '¨')
  , ('2', '¯')
  , ('4', '≤')
  , ('3', '˝')
  , ('5', '⬚')
  , ('6', '≥')
  , ('7', '⌽')
  , ('8', '≠')
  , ('9', '∨')
  , ('0', '∧')
  , ('-', '×')
  , ('=', '÷')
  , ('q', '↗')
  , ('w', '⍵')
  , ('e', '∊')
  , ('r', '⍴')
  , ('t', '⊞')
  , ('y', '↑')
  , ('u', '↓')
  , ('i', '⍳')
  , ('o', '○')
  , ('p', '◡')
  , ('[', '←')
  , (']', '→')
  , ('a', '⍺')
  , ('s', '⌈')
  , ('d', '⌊')
  , ('f', '⍛')
  , ('g', '∇')
  , ('h', '∆')
  , ('j', '∘')
  , ('k', '⎊')
  , ('l', '⎕')
  , (';', '⍎')
  , ('\'', '⍕')
  , ('\\', '⊢')
  , ('z', '⊂')
  , ('x', '⊃')
  , ('c', '∩')
  , ('v', '∪')
  , ('b', '⊥')
  , ('n', '⊤')
  , ('m', '«')
  , (',', '⍪')
  , ('.', '∙')
  , ('/', '⌿')
  , (' ', '‿')
  
  , ('~', '⍨')
  , ('!', '⨳')
--, ('@', ' ')
  , ('#', '⍒')
  , ('$', '⍋')
  , ('%', '≈')
  , ('^', '⍉')
--, ('&', ' ')
  , ('*', '⍣')
  , ('(', '⍱')
  , (')', '⍲')
  , ('_', '⊗')
  , ('+', '⊕')
--, ('Q', ' ')
  , ('W', '⍹')
  , ('E', '⍷')
  , ('R', '√')
  , ('T', '⍨')
  , ('Y', '↟')
  , ('U', '↡')
  , ('I', '⍸')
  , ('O', '⍥')
  , ('P', '◠')
  , ('{', '⟨')
  , ('}', '⟩')
  , ('A', '⍶')
  , ('S', '§')
  , ('D', '⸠')
  , ('F', '∡')
  , ('G', '⍢')
  , ('H', '⍙')
  , ('J', '⍤')
  , ('K', '⌸')
  , ('L', '⌷')
  , (':', '≡')
  , ('"', '≢')
  , ('|', '⊣')
  , ('Z', '⊆')
  , ('X', '⊇')
  , ('C', '⍝')
  , ('V', '⁖')
  , ('B', '∵')
  , ('N', '·')
  , ('M', '»')
  , ('<', 'ᑈ')
  , ('>', 'ᐵ')
--, ('?', ' ')
  ]

doubleCharacters :: [(Char, Char)]
doubleCharacters =
  [ ('`', '⋄')
--, ('1', ' ')
--, ('2', ' ')
  , ('3', '⍫')
  , ('4', '⊴')
  , ('5', '⤺')
  , ('6', '⊵')
--, ('7', ' ')
  , ('8', '⍟')
  , ('9', '∻')
  , ('0', '⍬')
  , ('-', '⸚')
  , ('=', '⌹')
  , ('q', '⇾')
--, ('w', ' ')
  , ('e', '⋵')
  , ('r', 'ϼ')
  , ('t', '߹')
  , ('y', 'ᓚ')
  , ('u', 'ᓗ')
  , ('i', '…')
  , ('o', '⍜')
  , ('p', '⏨')
  , ('[', '⦅')
  , (']', '⦆')
  , ('a', 'ɛ')
  , ('s', '↾')
  , ('d', '⇂')
  , ('f', '∠')
  , ('g', '⫇')
  , ('h', '⊸')
  , ('j', 'ᴊ')
--, ('k', ' ')
--, ('l', ' ')
  , (';', '⍮')
  , ('\'', '⍘')
  , ('\\', '⊩')
  , ('z', '⊏')
  , ('x', '⊐')
  , ('c', '⟃')
  , ('v', '⫤')
  , ('b', '⇇')
  , ('n', '↚')
  , ('m', '↩')
  , (',', '⊲')
  , ('.', '⊳')
--, ('/', ' ')
  , (' ', '`')
  
  , ('~', '⌺')
  , ('!', '⑴')
--, ('@', ' ')
--, ('#', ' ')
--, ('$', ' ')
--, ('%', ' ')
--, ('^', ' ')
--, ('&', ' ')
  , ('*', '∞')
  , ('(', '⦋')
  , (')', '⦌')
  , ('_', 'ⵧ')
  , ('+', '⧺')
  , ('Q', '⇽')
--, ('W', ' ')
  , ('E', '⋷')
  , ('R', 'ℜ')
  , ('T', '‥')
  , ('Y', '⥽')
  , ('U', '⥼')
  , ('I', 'ℑ')
--, ('O', ' ')
  , ('P', '⌓')
  , ('{', '⦃')
  , ('}', '⦄')
--, ('A', ' ')
--, ('S', ' ')
  , ('D', '⩔')
--, ('F', ' ')
--, ('G', ' ')
  , ('H', '⟜')
--, ('J', ' ')
--, ('K', ' ')
--, ('L', ' ')
  , (':', '⍠')
  , ('"', '⍞')
  , ('|', '⫣')
  , ('Z', 'ᑣ')
  , ('X', 'ᑒ')
  , ('C', '⟄')
--, ('V', ' ')
--, ('B', ' ')
  , ('N', '⩓')
  , ('M', '⍦')
--, ('<', ' ')
  , ('>', '■')
  , ('?', '⍰')
  ]

repl :: Context -> Char -> Bool -> IO ()
repl context _ True = let
  go context = do
    line <- (Just <$> getLine) `catch` (\(_ :: SomeException) -> pure Nothing)
    case line of
      Nothing -> pure ()
      Just line' -> runCode True "<repl>" line' context >>= go
  in go context
repl context prefixKey False = let
#ifdef is_linux
  go :: E.Edited -> Context -> IO ()
#else
  go :: Int -> Context -> IO ()
#endif
  go el context = do
#ifdef is_linux
    line <- E.getString el
#else
    putStr "      "
    hFlush stdout
    line <- Just <$> getLine
#endif
    case line of
      Nothing -> pure ()
      Just "" -> pure ()
      Just line' -> runCode True "<repl>" line' context >>= go el
  in do
    putStrLn "TinyAPL REPL, empty line to exit"
#ifdef is_linux
    el <- E.edited "TinyAPL"
    E.setEditor el E.Emacs
    E.setPrompt' el "      "
    E.addFunction el "prefix" "Prefix for entering TinyAPL glyphs" $ \_ _ -> do
      chM <- E.getOneChar el
      case chM of
        Nothing -> pure E.EOF
        Just ch | ch == prefixKey -> do
          ch2M <- E.getOneChar el
          case ch2M of
            Nothing -> pure E.EOF
            Just ch2 -> case lookup ch2 doubleCharacters of
              Just replacement -> do
                E.insertString el [replacement]
                pure E.Refresh
              Nothing -> do
                E.insertString el [ch2]
                pure E.RefreshBeep
        Just ch -> case lookup ch singleCharacters of
          Just replacement -> do
            E.insertString el [replacement]
            pure E.Refresh
          Nothing -> do
            E.insertString el [ch]
            pure E.RefreshBeep
    E.addBind el (singleton prefixKey) "prefix"
    E.setUseStyle el True
    E.setStyleFunc el $ \_ str -> pure $ (\case
      CNumber -> E.EditedStyle E.Red E.Unset False False False False
      CString -> E.EditedStyle E.Cyan E.Unset False False False False
      CStringEscape -> E.EditedStyle E.Blue E.Unset False False False False
      CArrayName -> E.EditedStyle E.Red E.Unset False False False False
      CPrimArray -> E.EditedStyle E.Red E.Unset False False False False
      CFunctionName -> E.EditedStyle E.Green E.Unset False False False False
      CPrimFunction -> E.EditedStyle E.Green E.Unset False False False False
      CAdverbName -> E.EditedStyle E.Magenta E.Unset False False False False
      CPrimAdverb -> E.EditedStyle E.Magenta E.Unset False False False False
      CConjunctionName -> E.EditedStyle E.Yellow E.Unset False False False False
      CPrimConjunction -> E.EditedStyle E.Yellow E.Unset False False False False
      CComment -> E.EditedStyle E.Unset E.Unset False True False False
      _ -> E.EditedStyleReset) <$> highlight str
    E.addBind el "\\e[1~" "ed-move-to-beg"
    E.addBind el "\\e[4~" "ed-move-to-end"
    E.addBind el "\\e[7~" "ed-move-to-beg"
    E.addBind el "\\e[8~" "ed-move-to-end"
    E.addBind el "\\e[H" "ed-move-to-beg"
    E.addBind el "\\e[F" "ed-move-to-end"
    E.addBind el "\\e[3~" "ed-delete-next-char"
    E.addBind el "\\e[2~" "em-toggle-overwrite"
    E.addBind el "\\e[1;5C" "em-next-word"
    E.addBind el "\\e[1;5D" "ed-prev-word"
    E.addBind el "\\e[5C" "em-next-word"
    E.addBind el "\\e[5D" "ed-prev-word"
    E.addBind el "\\e\\e[C" "em-next-word"
    E.addBind el "\\e\\e[D" "ed-prev-word"
#else
    let el = 0
#endif
    
    go el context
