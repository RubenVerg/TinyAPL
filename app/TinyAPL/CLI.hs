{-# LANGUAGE CPP, LambdaCase, OverloadedStrings #-}

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

import System.Environment
import Control.Monad (void, when)
import System.IO
import Data.IORef
import Data.Foldable
import Data.Maybe
import Data.List
import System.Info
import Control.DeepSeq
import Control.Exception (Exception(displayException))
import System.Directory
#ifdef is_linux
import TinyAPL.Highlighter
import qualified System.Console.Edited as E
#endif

defaultPrefixKey :: String
defaultPrefixKey = "`"

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

  args <- getArgs
  let context = Context {
      contextScope = scope
    , contextQuads = core <> ffiQuads <> quadsFromReprs [ makeSystemInfo os arch False bigEndian, file, TinyAPL.CLI.stdin ] [ makeImport readImportFile Nothing ] [] []
    , contextIn = liftToSt getLine
    , contextOut = \str -> do
      liftToSt $ putStr str
      liftToSt $ hFlush stdout
    , contextErr = \str -> do
      liftToSt $ hPutStr stderr str
      liftToSt $ hFlush stderr
    , contextIncrementalId = id
    , contextDirectory = cwd
    , contextPrimitives = P.primitives
    , contextPrefix = fromMaybe defaultPrefixKey (asum $ map (stripPrefix "-prefix") args) }

  case filter (isNothing . stripPrefix "-") args of
    []     -> repl context
    [path] -> do
      code <- F.readUtf8 path
      void $ runCode False path code context
    _      -> do
      hPutStrLn stderr "Usage:"
      hPutStrLn stderr "tinyapl         Start a REPL"
      hPutStrLn stderr "tinyapl path    Run a file"

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
--, ('3', ' ')
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
--, ('D', ' ')
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
--, ('N', ' ')
  , ('M', '⍦')
--, ('<', ' ')
  , ('>', '■')
  , ('?', '⍰')
  ]

repl :: Context -> IO ()
repl context = let
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
    putStrLn "Supported primitives:"
    putStrLn $ "  " ++ unwords (fst <$> P.arrays)
    putStrLn $ "  " ++ unwords (fst <$> P.functions)
    putStrLn $ "  " ++ unwords (fst <$> P.adverbs)
    putStrLn $ "  " ++ unwords (fst <$> P.conjunctions)
    putStrLn "Supported quad names:"
    putStrLn $ "  " ++ unwords (fst <$> quadArrays (contextQuads context))
    putStrLn $ "  " ++ unwords (fst <$> quadFunctions (contextQuads context))
    putStrLn $ "  " ++ unwords (fst <$> quadAdverbs (contextQuads context))
    putStrLn $ "  " ++ unwords (fst <$> quadConjunctions (contextQuads context))
    putStrLn "Supported features:"
    putStrLn $ "* dfns " ++ [fst G.braces] ++ "code" ++ [snd G.braces] ++ ", d-monadic-ops " ++ [G.underscore, fst G.braces] ++ "code" ++ [snd G.braces] ++ ", d-dyadic-ops " ++ [G.underscore, fst G.braces] ++ "code" ++ [snd G.braces, G.underscore]
    putStrLn $ "  " ++ [G.alpha] ++ " left argument, " ++ [G.omega] ++ " right argument,"
    putStrLn $ "  " ++ [G.alpha, G.alpha] ++ " left array operand, " ++ [G.alphaBar, G.alphaBar] ++ " left function operand, " ++ [G.omega, G.omega] ++ " right array operand, " ++ [G.omegaBar, G.omegaBar] ++ " right function operand,"
    putStrLn $ "  " ++ [G.del] ++ " recurse function, " ++ [G.underscore, G.del] ++ " recurse monadic op, " ++ [G.underscore, G.del, G.underscore] ++ " recurse dyadic op"
    putStrLn $ "  " ++ [G.exit] ++ " early exit, " ++ [G.guard] ++ " guard"
    putStrLn $ "  " ++ [G.separator] ++ " multiple statements"
    putStrLn $ "* numbers: " ++ [G.decimal] ++ " decimal separator, " ++ [G.negative] ++ " negative sign, " ++ [G.exponent] ++ " exponent notation, " ++ [G.imaginary] ++ " complex separator"
    putStrLn $ "* character literals: " ++ [G.charDelimiter] ++ "abc" ++ [G.charDelimiter]
    putStrLn $ "* string literals: " ++ [G.stringDelimiter] ++ "abc" ++ [G.stringDelimiter] ++ " with escapes using " ++ [G.stringEscape]
    putStrLn $ "* names: abc array, Abc function, _Abc monadic op, _Abc_ dyadic op"
    putStrLn $ "* get " ++ [G.quad] ++ " read evaluated input, get " ++ [G.quadQuote] ++ " read string input, set " ++ [G.quad] ++ " print with newline, set " ++ [G.quadQuote] ++ " print without newline"
    putStrLn $ "* array notation: " ++ [fst G.vector, G.separator, snd G.vector] ++ " vector, " ++ [fst G.highRank, G.separator, snd G.highRank] ++ " higher rank array (combine major cells), " ++ [G.tie] ++ " tie (like vector notation)"
    putStrLn $ "* trains: " ++ [fst G.train, snd G.train] ++ " deriving function, " ++ [G.underscore, fst G.train, snd G.train] ++ " deriving adverb, " ++ [G.underscore, fst G.train, snd G.train, G.underscore] ++ " deriving conjunction"
    putStrLn $ "* structs: " ++ [fst G.struct] ++ "statements" ++ [snd G.struct] ++ ", qualified access " ++ [G.access]
    putStrLn $ "* assignment with " ++ [G.assign] ++ ", modify assignment with " ++ [G.assignModify] ++ ", constant assignment with " ++ [G.assignConstant] ++ ", private assignment with " ++ [G.assignPrivate]
    putStrLn $ "* array assignment with array notation of names"
    putStrLn $ "* comments: " ++ [G.comment] ++ " until end of line, " ++ [fst G.inlineComment, snd G.inlineComment] ++ " inline"
    
#ifdef is_linux
    el <- E.edited "TinyAPL"
    E.setEditor el E.Emacs
    E.setPrompt' el "      "
    E.addFunction el "prefix" "Prefix for entering TinyAPL glyphs" $ \_ _ -> do
      chM <- E.getOneChar el
      case chM of
        Nothing -> pure E.EOF
        Just '`' -> do
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
    E.addBind el (contextPrefix context) "prefix"
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
