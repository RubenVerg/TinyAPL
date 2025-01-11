module TinyAPL.Highlighter where

import qualified TinyAPL.Glyphs as G
import TinyAPL.Parser
import TinyAPL.Util

import Control.Monad.State
import Data.Char (chr)

data Color
  = COther
  | CSyntax
  | CNumber
  | CString
  | CStringEscape
  | CArrayName
  | CPrimArray
  | CFunctionName
  | CPrimFunction
  | CAdverbName
  | CPrimAdverb
  | CConjunctionName
  | CPrimConjunction
  | CComment
  deriving (Enum, Show)

data HState = HState
  { hColors :: [Color]
  , hStr :: String }

type HSt = State HState

highlight :: String -> [Color]
highlight str = take (length str) $ reverse $ hColors $ execState hl (HState [] str) where
  atEnd :: HSt Bool
  atEnd = do
    str <- gets hStr
    pure $ null str
  
  andNotAtEnd :: HSt Bool -> HSt Bool
  andNotAtEnd p = do
    e <- atEnd
    if e then pure False else p

  peek :: HSt Char
  peek = do
    str <- gets hStr
    pure $ case str of (s:_) -> s; [] -> chr 0

  peekNext :: HSt Char
  peekNext = do
    str <- gets hStr
    pure $ case str of (_:s:_) -> s; _ -> chr 0

  peekNextNext :: HSt Char
  peekNextNext = do
    str <- gets hStr
    pure $ case str of (_:_:s:_) -> s; _ -> chr 0

  advance :: HSt Char
  advance = do
    st <- get
    let ss = hStr st
    case ss of
      [] -> pure $ chr 0
      (s:ss') -> do
        put $ st{ hStr = ss' }
        pure s

  push :: Color -> HSt ()
  push c = do
    st@HState{ hColors = cs } <- get
    put $ st{ hColors = c : cs }

 
  pushMany :: [Color] -> HSt ()
  pushMany = mapM_ push

  string :: HSt ()
  string = do
    start <- advance
    let isString = start == G.stringDelimiter
    push CString
    whileM_ (andNotAtEnd $ (/= start) <$> peek) $ do
      c <- peek
      if isString && c == G.stringEscape then do
        advance
        push CStringEscape
        advance
        push CStringEscape
      else do
        advance
        push CString
    advance
    push CString

  numberChars = ['0'..'9'] ++ [G.decimal, G.negative, G.exponent, G.imaginary, G.infinity]

  number :: HSt ()
  number = whileM_ (andNotAtEnd $ (`elem` numberChars) <$> peek) $ do
    advance
    push CNumber

  inlineComment :: HSt ()
  inlineComment = do
    advance
    push CComment
    whileM_ (andNotAtEnd $ (/= (snd G.inlineComment)) <$> peek) $ do
      advance
      push CComment
    advance
    push CComment

  comment :: HSt ()
  comment = do
    advance
    push CComment
    whileM_ (andNotAtEnd $ (/= '\n') <$> peek) $ do
      advance
      push CComment
    advance
    push CComment

  identifierChars = G.underscore : ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ G.identifiers

  identifier :: HSt ()
  identifier = do
    is <- whileM (andNotAtEnd $ (`elem` identifierChars) <$> peek) advance
    if isArrayName is then pushMany $ const CArrayName <$> is
    else if isFunctionName is then pushMany $ const CFunctionName <$> is
    else if isAdverbName is then pushMany $ const CAdverbName <$> is
    else if isConjunctionName is then pushMany $ const CConjunctionName <$> is
    else pushMany $ const COther <$> is

  hl :: HSt ()
  hl = whileM_ (not <$> atEnd) $ do
    p <- peek
    n <- peekNext
    nn <- peekNextNext
    if p `elem` numberChars then number
    else if p `elem` [G.stringDelimiter, G.charDelimiter] then string
    else if p == fst G.braces then advance >> push CSyntax
    else if p == G.underscore && n == fst G.braces then advance >> advance >> pushMany [CSyntax, CSyntax]
    else if p == snd G.braces && n == G.underscore then advance >> advance >> pushMany [CSyntax, CSyntax]
    else if p == snd G.braces then advance >> push CSyntax
    else if p == fst G.train then advance >> push CSyntax
    else if p == G.underscore && n == fst G.train then advance >> advance >> pushMany [CSyntax, CSyntax]
    else if p == snd G.train && n == G.underscore then advance >> advance >> pushMany [CSyntax, CSyntax]
    else if p == snd G.train then advance >> push CSyntax
    else if p == G.underscore && n == G.unwrap && nn == G.underscore then advance >> advance >> advance >> pushMany [CSyntax, CSyntax, CSyntax]
    else if p == G.underscore && n == G.unwrap then advance >> advance >> pushMany [CSyntax, CSyntax]
    else if p == G.unwrap then advance >> push CSyntax
    else if p `elem` identifierChars then identifier
    else if p == fst G.inlineComment then inlineComment
    else if p == G.comment then comment
    else if p `elem` G.syntax then advance >> push CSyntax
    else if p `elem` G.arrays then advance >> push CPrimArray
    else if p `elem` G.functions then advance >> push CPrimFunction
    else if p `elem` G.adverbs then advance >> push CPrimAdverb
    else if p `elem` G.conjunctions then advance >> push CPrimConjunction
    else advance >> push COther
