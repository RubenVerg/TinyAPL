{-# LANGUAGE LambdaCase, BangPatterns, TupleSections #-}

module TinyAPL.Parser where

import TinyAPL.Complex
import TinyAPL.Error
import qualified TinyAPL.Glyphs as G
import TinyAPL.Util

import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Functor (($>), void)
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.List (elemIndex, intercalate, singleton)
import Control.Applicative (liftA3, (<**>))
import Data.Bifunctor
import Control.Monad ((>=>))
import Data.Void (Void)
import Text.Parser.Combinators (sepByNonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Char (isSpace)

type Parser = Parsec Void String

data AssignType
  = AssignNormal
  | AssignModify
  | AssignConstant
  | AssignPrivate
  deriving (Eq, Enum, Bounded)

assignTypes :: [AssignType]
assignTypes = [minBound .. maxBound]

instance Show AssignType where
  show AssignNormal = "normal"
  show AssignModify = "modify"
  show AssignConstant = "constant"
  show AssignPrivate = "private"

assignTypeArrow :: AssignType -> Char
assignTypeArrow AssignNormal = G.assign
assignTypeArrow AssignModify = G.assignModify
assignTypeArrow AssignConstant = G.assignConstant
assignTypeArrow AssignPrivate = G.assignPrivate

assignArrows :: [(AssignType, Char)]
assignArrows = map (liftA2 (,) id assignTypeArrow) assignTypes

data Token
  = TokenNumber (Complex Double) SourcePos
  | TokenChar String SourcePos
  | TokenString String SourcePos
  | TokenPrimArray Char SourcePos
  | TokenPrimFunction Char SourcePos
  | TokenPrimAdverb Char SourcePos
  | TokenPrimConjunction Char SourcePos
  | TokenDfn (NonEmpty [Token]) SourcePos
  | TokenDadv (NonEmpty [Token]) SourcePos
  | TokenDconj (NonEmpty [Token]) SourcePos
  | TokenArrayName String SourcePos
  | TokenFunctionName String SourcePos
  | TokenAdverbName String SourcePos
  | TokenConjunctionName String SourcePos
  | TokenQualifiedArrayName Token (NonEmpty String) SourcePos
  | TokenQualifiedFunctionName Token (NonEmpty String) SourcePos
  | TokenQualifiedAdverbName Token (NonEmpty String) SourcePos
  | TokenQualifiedConjunctionName Token (NonEmpty String) SourcePos
  | TokenArrayAssign String AssignType (NonEmpty Token) SourcePos
  | TokenFunctionAssign String AssignType (NonEmpty Token) SourcePos
  | TokenAdverbAssign String AssignType (NonEmpty Token) SourcePos
  | TokenConjunctionAssign String AssignType (NonEmpty Token) SourcePos
  | TokenQualifiedArrayAssign Token (NonEmpty String) AssignType (NonEmpty Token) SourcePos
  | TokenQualifiedFunctionAssign Token (NonEmpty String) AssignType (NonEmpty Token) SourcePos
  | TokenQualifiedAdverbAssign Token (NonEmpty String) AssignType (NonEmpty Token) SourcePos
  | TokenQualifiedConjunctionAssign Token (NonEmpty String) AssignType (NonEmpty Token) SourcePos
  | TokenVectorAssign [String] AssignType (NonEmpty Token) SourcePos
  | TokenHighRankAssign [String] AssignType (NonEmpty Token) SourcePos
  | TokenTieAssign (NonEmpty String) AssignType (NonEmpty Token) SourcePos
  | TokenStructAssign [(String, Maybe (AssignType, String))] AssignType (NonEmpty Token) SourcePos
  | TokenParens (NonEmpty Token) SourcePos
  | TokenGuard (NonEmpty Token) [Token] SourcePos
  | TokenExit (NonEmpty Token) SourcePos
  | TokenVector [NonEmpty Token] SourcePos
  | TokenHighRank [NonEmpty Token] SourcePos
  | TokenDictionary [(NonEmpty Token, NonEmpty Token)] SourcePos
  | TokenTrain [[Token]] SourcePos
  | TokenAdverbTrain [[Token]] SourcePos
  | TokenConjunctionTrain [[Token]] SourcePos
  | TokenWrap Token SourcePos
  | TokenUnwrap Token SourcePos
  | TokenUnwrapAdverb Token SourcePos
  | TokenUnwrapConjunction Token SourcePos
  | TokenStruct [NonEmpty Token] SourcePos
  | TokenTie (NonEmpty Token) SourcePos
  | TokenTernary (NonEmpty Token) (NonEmpty Token) (NonEmpty Token) SourcePos
  | TokenExtraArgs [(NonEmpty Token, NonEmpty Token)] SourcePos
  | TokenSpreadExtraArgs (NonEmpty Token) SourcePos
  | TokenNothing SourcePos

instance Eq Token where
  (TokenNumber x _) == (TokenNumber y _) = x == y
  (TokenChar x _) == (TokenChar y _) = x == y
  (TokenString x _) == (TokenString y _) = x == y
  (TokenPrimArray x _) == (TokenPrimArray y _) = x == y
  (TokenPrimFunction x _) == (TokenPrimFunction y _) = x == y
  (TokenPrimAdverb x _) == (TokenPrimAdverb y _) = x == y
  (TokenPrimConjunction x _) == (TokenPrimConjunction y _) = x == y
  (TokenDfn x _) == (TokenDfn y _) = x == y
  (TokenDadv x _) == (TokenDadv y _) = x == y
  (TokenDconj x _) == (TokenDconj y _) = x == y
  (TokenArrayName x _) == (TokenArrayName y _) = x == y
  (TokenFunctionName x _) == (TokenFunctionName y _) = x == y
  (TokenAdverbName x _) == (TokenAdverbName y _) = x == y
  (TokenConjunctionName x _) == (TokenConjunctionName y _) = x == y
  (TokenQualifiedArrayName x _ _) == (TokenQualifiedArrayName y _ _) = x == y
  (TokenQualifiedFunctionName x _ _) == (TokenQualifiedFunctionName y _ _) = x == y
  (TokenQualifiedAdverbName x _ _) == (TokenQualifiedAdverbName y _ _) = x == y
  (TokenQualifiedConjunctionName x _ _) == (TokenQualifiedConjunctionName y _ _) = x == y
  (TokenArrayAssign xn xc x _) == (TokenArrayAssign yn yc y _) = xn == yn && xc == yc && x == y
  (TokenFunctionAssign xn xc x _) == (TokenFunctionAssign yn yc y _) = xn == yn && xc == yc && x == y
  (TokenAdverbAssign xn xc x _) == (TokenAdverbAssign yn yc y _) = xn == yn && xc == yc && x == y
  (TokenConjunctionAssign xn xc x _) == (TokenConjunctionAssign yn yc y _) = xn == yn && xc == yc && x == y
  (TokenQualifiedArrayAssign xh xs xt xv _) == (TokenQualifiedArrayAssign yh ys yt yv _) = xh == yh && xs == ys && xt == yt && xv == yv
  (TokenQualifiedFunctionAssign xh xs xt xv _) == (TokenQualifiedFunctionAssign yh ys yt yv _) = xh == yh && xs == ys && xt == yt && xv == yv
  (TokenQualifiedAdverbAssign xh xs xt xv _) == (TokenQualifiedAdverbAssign yh ys yt yv _) = xh == yh && xs == ys && xt == yt && xv == yv
  (TokenQualifiedConjunctionAssign xh xs xt xv _) == (TokenQualifiedConjunctionAssign yh ys yt yv _) = xh == yh && xs == ys && xt == yt && xv == yv
  (TokenVectorAssign xn xt x _) == (TokenVectorAssign yn yt y _) = xn == yn && xt == yt && x == y
  (TokenHighRankAssign xn xt x _) == (TokenHighRankAssign yn yt y _) = xn == yn && xt == yt && x == y
  (TokenTieAssign xn xt x _) == (TokenTieAssign yn yt y _) = xn == yn && xt == yt && x == y
  (TokenStructAssign xn xt x _) == (TokenStructAssign yn yt y _) = xn == yn && xt == yt && x == y
  (TokenParens x _) == (TokenParens y _) = x == y
  (TokenGuard xc xe _) == (TokenGuard yc ye _) = xc == yc && xe == ye
  (TokenExit x _) == (TokenExit y _) = x == y
  (TokenVector x _) == (TokenVector y _) = x == y
  (TokenDictionary x _) == (TokenDictionary y _) = x == y
  (TokenHighRank x _) == (TokenHighRank y _) = x == y
  (TokenTrain x _) == (TokenTrain y _) = x == y
  (TokenAdverbTrain x _) == (TokenAdverbTrain y _) = x == y
  (TokenConjunctionTrain x _) == (TokenConjunctionTrain y _) = x == y
  (TokenWrap x _) == (TokenWrap y _) = x == y
  (TokenUnwrap x _) == (TokenUnwrap y _) = x == y
  (TokenUnwrapAdverb x _) == (TokenUnwrapAdverb y _) = x == y
  (TokenUnwrapConjunction x _) == (TokenUnwrapConjunction y _) = x == y
  (TokenStruct x _) == (TokenStruct y _) = x == y
  (TokenTie x _) == (TokenTie y _) = x == y
  (TokenTernary xh xt xf _) == (TokenTernary yh yt yf _) = xh == yh && xt == yt && xf == yf
  (TokenExtraArgs xes _) == (TokenExtraArgs yes _) = xes == yes
  (TokenSpreadExtraArgs x _) == (TokenSpreadExtraArgs y _) = x == y
  (TokenNothing _) == (TokenNothing _) = True
  _ == _ = False

instance Show Token where
  show (TokenNumber x _) = "(number " ++ show x ++ ")"
  show (TokenChar x _) = "(character " ++ [G.charDelimiter] ++ x ++ [G.charDelimiter] ++ ")"
  show (TokenString x _) = "(string " ++ [G.stringDelimiter] ++ x ++ [G.stringDelimiter] ++ ")"
  show (TokenPrimArray x _) = "(primitive array " ++ [x] ++ ")"
  show (TokenPrimFunction x _) = "(primitive function " ++ [x] ++ ")"
  show (TokenPrimAdverb x _) = "(primitive adverb " ++ [x] ++ ")"
  show (TokenPrimConjunction x _) = "(primitive conjunction " ++ [x] ++ ")"
  show (TokenDfn xs _) = "(dfn " ++ [fst G.braces, ' '] ++ intercalate [' ', G.separator, ' '] (unwords . fmap show <$> NE.toList xs) ++ [' ', snd G.braces] ++ ")"
  show (TokenDadv xs _) = "(dadv " ++ [G.underscore, fst G.braces, ' '] ++ intercalate [' ', G.separator, ' '] (unwords . fmap show <$> NE.toList xs) ++ [' ', snd G.braces] ++ ")"
  show (TokenDconj xs _) = "(dconj " ++ [G.underscore, fst G.braces, ' '] ++ intercalate [' ', G.separator, ' '] (unwords . fmap show <$> NE.toList xs) ++ [' ', snd G.braces, G.underscore] ++ ")"
  show (TokenArrayName x _) = "(array name " ++ x ++ ")"
  show (TokenFunctionName x _) = "(function name " ++ x ++ ")"
  show (TokenAdverbName x _) = "(adverb name " ++ x ++ ")"
  show (TokenConjunctionName x _) = "(conjunction name " ++ x ++ ")"
  show (TokenQualifiedArrayName t ns _) = "(qualified array name " ++ show t ++ [G.access] ++ intercalate [G.access] (NE.toList ns) ++ ")"
  show (TokenQualifiedFunctionName t ns _) = "(qualified function name " ++ show t ++ [G.access] ++ intercalate [G.access] (NE.toList ns) ++ ")"
  show (TokenQualifiedAdverbName t ns _) = "(qualified adverb name " ++ show t ++ [G.access] ++ intercalate [G.access] (NE.toList ns) ++ ")"
  show (TokenQualifiedConjunctionName t ns _) = "(qualified conjunction name " ++ show t ++ [G.access] ++ intercalate [G.access] (NE.toList ns) ++ ")"
  show (TokenArrayAssign x c xs _) = "(array assign " ++ x ++ [' ', assignTypeArrow c, ' '] ++ unwords (NE.toList $ show <$> xs) ++ ")"
  show (TokenFunctionAssign x c xs _) = "(function assign " ++ x ++ [' ', assignTypeArrow c, ' '] ++ unwords (NE.toList $ show <$> xs) ++ ")"
  show (TokenAdverbAssign x c xs _) = "(adverb assign " ++ x ++ [' ', assignTypeArrow c, ' '] ++ unwords (NE.toList $ show <$> xs) ++ ")"
  show (TokenConjunctionAssign x c xs _) = "(conjunction assign " ++ x ++ [' ', assignTypeArrow c, ' '] ++ unwords (NE.toList $ show <$> xs) ++ ")"
  show (TokenQualifiedArrayAssign t ns ty xs _) = "(qualified array assign " ++ show t ++ [G.access] ++ intercalate [G.access] (NE.toList ns) ++ [' ', assignTypeArrow ty, ' '] ++ unwords (NE.toList $ show <$> xs) ++ ")"
  show (TokenQualifiedFunctionAssign t ns ty xs _) = "(qualified function assign " ++ show t ++ [G.access] ++ intercalate [G.access] (NE.toList ns) ++ [' ', assignTypeArrow ty, ' '] ++ unwords (NE.toList $ show <$> xs) ++ ")"
  show (TokenQualifiedAdverbAssign t ns ty xs _) = "(qualified adverb assign " ++ show t ++ [G.access] ++ intercalate [G.access] (NE.toList ns) ++ [' ', assignTypeArrow ty, ' '] ++ unwords (NE.toList $ show <$> xs) ++ ")"
  show (TokenQualifiedConjunctionAssign t ns ty xs _) = "(qualified conjunction assign " ++ show t ++ [G.access] ++ intercalate [G.access] (NE.toList ns) ++ [' ', assignTypeArrow ty, ' '] ++ unwords (NE.toList $ show <$> xs) ++ ")"
  show (TokenVectorAssign ns c xs _) = "(vector assign " ++ unwords (show <$> ns) ++ " " ++ [assignTypeArrow c, ' '] ++ unwords (NE.toList $ show <$> xs) ++ ")"
  show (TokenHighRankAssign ns c xs _) = "(high rank assign " ++ unwords (show <$> ns) ++ " " ++ [assignTypeArrow c, ' '] ++ unwords (NE.toList $ show <$> xs) ++ ")"
  show (TokenTieAssign ns c xs _) = "(tie assign " ++ unwords (NE.toList $ show <$> ns) ++ " " ++ [assignTypeArrow c, ' '] ++ unwords (NE.toList $ show <$> xs) ++ ")"
  show (TokenStructAssign ns c xs _) = "(struct assign " ++ unwords (uncurry (++) . second (maybe "" (\(c, n) -> assignTypeArrow c : n)) <$> ns) ++ " " ++ [assignTypeArrow c, ' '] ++ unwords (NE.toList $ show <$> xs) ++ ")"
  show (TokenParens xs _) = "(parens (" ++ unwords (NE.toList $ show <$> xs) ++ "))"
  show (TokenGuard gs rs _) = "(guard " ++ unwords (NE.toList $ show <$> gs) ++ " : " ++ unwords (show <$> rs) ++ ")"
  show (TokenExit xs _) = "(exit " ++ [G.exit, ' '] ++ unwords (NE.toList $ show <$> xs) ++ ")"
  show (TokenVector xs _) = "(vector " ++ [fst G.vector, ' '] ++ intercalate [' ', G.separator, ' '] (unwords . NE.toList . fmap show <$> xs) ++ [snd G.vector] ++ ")"
  show (TokenHighRank xs _) = "(high rank " ++ [fst G.highRank, ' '] ++ intercalate [' ', G.separator, ' '] (unwords . NE.toList . fmap show <$> xs) ++ [snd G.highRank] ++ ")"
  show (TokenDictionary xs _) = "(dictionary " ++ [fst G.vector, ' '] ++ intercalate [' ', G.separator, ' '] ((\(k, v) -> show k ++ [G.guard] ++ show v) <$> xs) ++ [snd G.vector] ++ ")"
  show (TokenTrain xs _) = "(train " ++ [fst G.train, ' '] ++ intercalate [' ', G.separator, ' '] (unwords . fmap show <$> xs) ++ [snd G.train] ++ ")" where
  show (TokenAdverbTrain xs _) = "(adverb train " ++ [G.underscore, fst G.train, ' '] ++ intercalate [' ', G.separator, ' '] (unwords . fmap show <$> xs) ++ [snd G.train] ++ ")" where
  show (TokenConjunctionTrain xs _) = "(conjunction train " ++ [G.underscore, fst G.train, ' '] ++ intercalate [' ', G.separator, ' '] (unwords . fmap show <$> xs) ++ [snd G.train, G.underscore] ++ ")" where
  show (TokenWrap x _) = "(wrap " ++ [G.wrap] ++ show x ++ ")"
  show (TokenUnwrap x _) = "(unwrap " ++ [G.unwrap] ++ show x ++ ")"
  show (TokenUnwrapAdverb x _) = "(unwrap adverb " ++ [G.underscore, G.unwrap] ++ show x ++ ")"
  show (TokenUnwrapConjunction x _) = "(unwrap conjunction " ++ [G.underscore, G.unwrap, G.underscore] ++ show x ++ ")"
  show (TokenStruct xs _) = "(struct " ++ [fst G.struct] ++ intercalate [' ', G.separator, ' '] (unwords . NE.toList . fmap show <$> xs) ++ [snd G.struct] ++ ")"
  show (TokenTie xs _) = "(tie " ++ intercalate [G.tie] (NE.toList $ fmap show xs) ++ ")"
  show (TokenTernary xh xt xf _) = "(ternary " ++ unwords (NE.toList $ show <$> xh) ++ [' ', fst G.ternary, ' '] ++ unwords (NE.toList $ show <$> xt) ++ [' ', snd G.ternary, ' '] ++ unwords (NE.toList $ show <$> xf) ++ ")"
  show (TokenExtraArgs xs _) = "(extra args " ++ [fst G.extraArgs] ++ intercalate [' ', G.separator, ' '] ((\(k, v) -> unwords (NE.toList $ show <$> k) ++ [G.guard] ++ unwords (NE.toList $ show <$> v)) <$> xs) ++ [snd G.extraArgs] ++ ")"
  show (TokenSpreadExtraArgs x _) = "(spread extra args " ++ [fst G.extraArgs] ++ unwords (NE.toList $ show <$> x) ++ [snd G.extraArgs] ++ ")"
  show (TokenNothing _) = "(nothing)"

tokenPos :: Token -> SourcePos
tokenPos (TokenNumber _ pos) = pos
tokenPos (TokenChar _ pos) = pos
tokenPos (TokenString _ pos) = pos
tokenPos (TokenPrimArray _ pos) = pos
tokenPos (TokenPrimFunction _ pos) = pos
tokenPos (TokenPrimAdverb _ pos) = pos
tokenPos (TokenPrimConjunction _ pos) = pos
tokenPos (TokenDfn _ pos) = pos
tokenPos (TokenDadv _ pos) = pos
tokenPos (TokenDconj _ pos) = pos
tokenPos (TokenArrayName _ pos) = pos
tokenPos (TokenFunctionName _ pos) = pos
tokenPos (TokenAdverbName _ pos) = pos
tokenPos (TokenConjunctionName _ pos) = pos
tokenPos (TokenQualifiedArrayName _ _ pos) = pos
tokenPos (TokenQualifiedFunctionName _ _ pos) = pos
tokenPos (TokenQualifiedAdverbName _ _ pos) = pos
tokenPos (TokenQualifiedConjunctionName _ _ pos) = pos
tokenPos (TokenArrayAssign _ _ _ pos) = pos
tokenPos (TokenFunctionAssign _ _ _ pos) = pos
tokenPos (TokenAdverbAssign _ _ _ pos) = pos
tokenPos (TokenConjunctionAssign _ _ _ pos) = pos
tokenPos (TokenQualifiedArrayAssign _ _ _ _ pos) = pos
tokenPos (TokenQualifiedFunctionAssign _ _ _ _ pos) = pos
tokenPos (TokenQualifiedAdverbAssign _ _ _ _ pos) = pos
tokenPos (TokenQualifiedConjunctionAssign _ _ _ _ pos) = pos
tokenPos (TokenVectorAssign _ _ _ pos) = pos
tokenPos (TokenHighRankAssign _ _ _ pos) = pos
tokenPos (TokenTieAssign _ _ _ pos) = pos
tokenPos (TokenStructAssign _ _ _ pos) = pos
tokenPos (TokenParens _ pos) = pos
tokenPos (TokenGuard _ _ pos) = pos
tokenPos (TokenExit _ pos) = pos
tokenPos (TokenVector _ pos) = pos
tokenPos (TokenHighRank _ pos) = pos
tokenPos (TokenDictionary _ pos) = pos
tokenPos (TokenTrain _ pos) = pos
tokenPos (TokenAdverbTrain _ pos) = pos
tokenPos (TokenConjunctionTrain _ pos) = pos
tokenPos (TokenWrap _ pos) = pos
tokenPos (TokenUnwrap _ pos) = pos
tokenPos (TokenUnwrapAdverb _ pos) = pos
tokenPos (TokenUnwrapConjunction _ pos) = pos
tokenPos (TokenStruct _ pos) = pos
tokenPos (TokenTie _ pos) = pos
tokenPos (TokenTernary _ _ _ pos) = pos
tokenPos (TokenExtraArgs _ pos) = pos
tokenPos (TokenSpreadExtraArgs _ pos) = pos
tokenPos (TokenNothing pos) = pos

emptyPos :: SourcePos
emptyPos = SourcePos "<empty>" (mkPos 1) (mkPos 1)

prettyError :: SourcePos -> String -> String
prettyError pos source = let
  ls = lines source
  line = subtract 1 $ unPos $ sourceLine pos
  column = subtract 1 $ unPos $ sourceColumn pos
  theLine = if length ls <= line then "" else ls !! line
  in sourceName pos ++ ":" ++ show (unPos $ sourceLine pos) ++ ":" ++ show (unPos $ sourceColumn pos) ++ "\n" ++ theLine ++ "\n" ++ replicate column ' ' ++ "^\n"

prettyParseError :: String -> SourcePos -> ParseError String Void -> String
prettyParseError source pos err = prettyError pos source ++ parseErrorTextPretty err

makeSyntaxError :: SourcePos -> String -> String -> Error
makeSyntaxError pos source msg = SyntaxError $ prettyError pos source ++ msg ++ "\n"

makeParseErrors :: String -> ParseErrorBundle String Void -> Error
makeParseErrors source es = case attachSourcePos errorOffset (bundleErrors es) (bundlePosState es) of
  (r :| rs, _) -> SyntaxError $ concatMap (uncurry $ flip $ prettyParseError source) $ r : rs

tokenize :: String -> String -> Result [[Token]]
tokenize file source = first (makeParseErrors source) $ Text.Megaparsec.parse (sepBy1 bitsMaybe separator <* eof) file source where
  withPos :: Parser (SourcePos -> a) -> Parser a
  withPos = (<**>) getSourcePos

  spaceConsumer :: Parser ()
  spaceConsumer = L.space (void $ satisfy (liftA2 (&&) isSpace (/= '\n')) <|> try (char '\n' <* notFollowedBy (char '\n'))) (L.skipLineComment [G.comment]) (L.skipBlockComment [fst G.inlineComment] [snd G.inlineComment])

  lexeme :: Parser a -> Parser a
  lexeme = L.lexeme spaceConsumer

  commitOn :: Parser a -> Parser b -> Parser a
  commitOn p q = try (p <* lookAhead q) <* q

  commitOn' :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
  commitOn' f p q = liftA2 f (try $ p <* lookAhead q) q

  arrayStart :: String
  arrayStart = G.delta : ['a'..'z']

  functionStart :: String
  functionStart = G.deltaBar : ['A'..'Z']

  identifierRest :: String
  identifierRest = arrayStart ++ functionStart ++ ['0'..'9']

  assignArrow :: Parser AssignType
  assignArrow = choice $ map (\(ty, c) -> char c $> ty) assignArrows
  
  assign' :: (a -> AssignType -> NonEmpty Token -> SourcePos -> b) -> Parser a -> Parser b
  assign' con name = withPos $ liftA2 ($) (commitOn' con (lexeme name) assignArrow) bits

  arrayName :: Parser String
  arrayName = try (liftA3 (\x y z -> x : y : z) (char G.quad) (oneOf arrayStart) (many $ oneOf identifierRest)) <|> try (string [G.alpha, G.alpha]) <|> try (string [G.omega, G.omega]) <|> try (string [G.alpha]) <|> try (string [G.omega]) <|> try (string [G.epsilon]) <|> try (string [G.quad]) <|> try (string [G.quadQuote]) <|> liftA2 (:) (oneOf arrayStart) (many $ oneOf identifierRest)

  functionName :: Parser String
  functionName = try (liftA3 (\x y z -> x : y : z) (char G.quad) (oneOf functionStart) (many $ oneOf identifierRest)) <|> try (string [G.del]) <|> try (string [G.alphaBar, G.alphaBar]) <|> try (string [G.omegaBar, G.omegaBar]) <|> liftA2 (:) (oneOf functionStart) (many $ oneOf identifierRest)

  adverbName :: Parser String
  adverbName = try (liftA3 (\x y z -> x : y : z) (char G.quad) (char G.underscore) (many $ oneOf identifierRest)) <|> try (string [G.underscore, G.del]) <|> liftA2 (:) (char G.underscore) (some $ oneOf identifierRest)

  conjunctionName :: Parser String
  conjunctionName = try ((\x y z w -> x : y : z ++ [w]) <$> char G.quad <*> char G.underscore <*> many (oneOf identifierRest) <*> char G.underscore) <|> try (string [G.underscore, G.del, G.underscore]) <|> liftA3 (\a b c -> a : b ++ [c]) (char G.underscore) (some $ oneOf identifierRest) (char G.underscore)

  anyName :: Parser String
  anyName = try conjunctionName <|> try adverbName <|> try functionName <|> try arrayName

  maybeQualifiedTie :: (NonEmpty Token -> SourcePos -> Token) -> [(Token -> NonEmpty String -> SourcePos -> Token, Token -> NonEmpty String -> AssignType -> NonEmpty Token -> SourcePos -> Token, Parser String)] -> Parser Token
  maybeQualifiedTie tie xs = do
    pos <- getSourcePos
    first <- bit'
    choice [do
      lexeme $ char G.access
      (middle, (name, assign, last)) <- commitOn' (,) (many $ lexeme arrayName `commitOn` char G.access) (choice $ (\(n, a, p) -> (n, a, ) <$> p) <$> xs)
      option (name first (snocNE middle last) pos) $ do
        as <- assignArrow
        w <- bits
        pure $ assign first (snocNE middle last) as w pos, do
      lexeme $ char G.tie
      rest <- sepBy1 bit' (lexeme $ char G.tie)
      pure $ tie (first :| rest) pos, pure first]

  nothing :: Parser Token
  nothing = withPos $ TokenNothing <$ lexeme (char G.nothing)

  array' :: Parser Token
  array' = number <|> charVec <|> str <|> try (withPos $ TokenArrayName <$> arrayName) <|> dictionaryNotation <|> vectorNotation <|> highRankNotation <|> primArray <|> wrap <|> struct where
    number :: Parser Token
    number = withPos $ TokenNumber <$> complex where
      sign :: Parser Double
      sign = option 1 (char G.negative $> (-1))

      natural :: Parser String
      natural = some digitChar

      integer :: Parser (Double, String)
      integer = liftA2 (,) sign natural

      float :: Parser Double
      float = do
        (s, i) <- integer
        d <- option "" $ liftA2 (:) (char G.decimal) natural
        return $ s * read (i ++ d)

      scientific :: Parser Double
      scientific = do
        f <- float
        option f $ do
          _ <- char G.exponent
          (es, ei) <- integer
          return $ f * 10 ** (es * read ei)
      
      real :: Parser Double
      real = choice [string [G.infinity] $> inf, string [G.negative, G.infinity] $> ninf, scientific]

      complex :: Parser (Complex Double)
      complex = liftA2 (:+) real (option 0 (char G.imaginary *> real))

    charVec :: Parser Token
    charVec = withPos $ TokenChar <$> between (char G.charDelimiter) (char G.charDelimiter) (many $ noneOf ['\''])

    str :: Parser Token
    str = withPos $ TokenString <$> between (char G.stringDelimiter) (char G.stringDelimiter) (many (escape <|> nonEscape)) where
      escape :: Parser Char
      escape = do
        _ <- char G.stringEscape
        c <- oneOf $ fst <$> G.escapes
        return $ fromJust $ lookup c G.escapes

      nonEscape :: Parser Char
      nonEscape = noneOf [G.stringDelimiter, G.stringEscape]

    vectorNotation :: Parser Token
    vectorNotation = withPos $ between (char $ fst G.vector) (char $ snd G.vector) (TokenVector <$> sepBy bits separator)

    highRankNotation :: Parser Token
    highRankNotation = withPos $ between (char $ fst G.highRank) (char $ snd G.highRank) (TokenHighRank <$> sepBy bits separator)

    dictionaryNotation :: Parser Token
    dictionaryNotation = withPos $ choice
      [ try $ between (char $ fst G.vector) (char $ snd G.vector) (lexeme $ char G.guard) $> TokenDictionary []
      , try $ between (char $ fst G.vector) (char $ snd G.vector) (TokenDictionary <$> sepBy1 (liftA2 (,) (bits `commitOn` (lexeme $ char G.guard)) bits) separator ) ]

    primArray :: Parser Token
    primArray = withPos $ TokenPrimArray <$> oneOf G.arrays

    wrap :: Parser Token
    wrap = withPos $ TokenWrap <$> (char G.wrap *> bit)

    struct :: Parser Token
    struct = withPos $ TokenStruct <$> (string [fst G.struct] *> sepBy bits separator <* string [snd G.struct])

  array'' :: Parser Token
  array'' = vectorAssign <|> highRankAssign <|> tieAssign <|> structAssign <|> arrayAssign where
    vectorAssign :: Parser Token
    vectorAssign = assign' TokenVectorAssign $ between (lexeme $ char $ fst G.vector) (lexeme $ char $ snd G.vector) (sepBy (lexeme arrayName) separator)

    highRankAssign :: Parser Token
    highRankAssign = assign' TokenHighRankAssign $ between (lexeme $ char $ fst G.highRank) (lexeme $ char $ snd G.highRank) (sepBy (lexeme arrayName) separator)

    tieAssign :: Parser Token
    tieAssign = assign' TokenTieAssign $ liftA2 (:|) (lexeme arrayName `commitOn` lexeme (char G.tie)) (sepBy (lexeme arrayName) (lexeme $ char G.tie))

    structAssign :: Parser Token
    structAssign = assign' TokenStructAssign $ between (lexeme $ char $ fst G.struct) (lexeme $ char $ snd G.struct) (sepBy (liftA2 (,) (lexeme anyName) (option Nothing $ Just <$> liftA2 (,) (lexeme assignArrow) (lexeme anyName))) separator)

    arrayAssign :: Parser Token
    arrayAssign = assign' TokenArrayAssign arrayName

  function' :: Parser Token
  function' = dfn <|> train <|> try (withPos $ TokenFunctionName <$> functionName) <|> primFunction <|> unwrap where
    dfn :: Parser Token
    dfn = withPos $ TokenDfn <$> (string [fst G.braces] *> sepByNonEmpty definedBits separator <* string [snd G.braces])

    train :: Parser Token
    train = withPos $ TokenTrain <$> (string [fst G.train] *> sepBy1 bitsMaybe separator <* string [snd G.train])

    primFunction :: Parser Token
    primFunction = withPos $ TokenPrimFunction <$> oneOf G.functions

    unwrap :: Parser Token
    unwrap = withPos $ TokenUnwrap <$> (char G.unwrap *> bit)

  function'' :: Parser Token
  function'' = functionAssign where
    functionAssign :: Parser Token
    functionAssign = assign' TokenFunctionAssign functionName

  adverb' :: Parser Token
  adverb' = try dadv <|> try adverbTrain <|> try (withPos $ TokenAdverbName <$> adverbName) <|> primAdverb <|> unwrapAdverb where
    dadv :: Parser Token
    dadv = withPos $ TokenDadv <$> (string [G.underscore, fst G.braces] *> sepByNonEmpty definedBits separator <* string [snd G.braces] <* notFollowedBy (char G.underscore))

    adverbTrain :: Parser Token
    adverbTrain = withPos $ TokenAdverbTrain <$> (string [G.underscore, fst G.train] *> sepBy1 bitsMaybe separator <* string [snd G.train] <* notFollowedBy (char G.underscore))

    primAdverb :: Parser Token
    primAdverb = withPos $ TokenPrimAdverb <$> oneOf G.adverbs

    unwrapAdverb :: Parser Token
    unwrapAdverb = withPos $ TokenUnwrapAdverb <$> (string [G.underscore, G.unwrap] *> bit)

  adverb'' :: Parser Token
  adverb'' = adverbAssign where
    adverbAssign :: Parser Token
    adverbAssign = assign' TokenAdverbAssign adverbName

  conjunction' :: Parser Token
  conjunction' = try dconj <|> try conjunctionTrain <|> try (withPos $ TokenConjunctionName <$> conjunctionName) <|> primConjunction <|> unwrapConjunction where
    dconj :: Parser Token
    dconj = withPos $ TokenDconj <$> (string [G.underscore, fst G.braces] *> sepByNonEmpty definedBits separator <* string [snd G.braces, G.underscore])

    conjunctionTrain :: Parser Token
    conjunctionTrain = withPos $ TokenConjunctionTrain <$> (string [G.underscore, fst G.train] *> sepBy1 bitsMaybe separator <* string [snd G.train, G.underscore])

    primConjunction :: Parser Token
    primConjunction = withPos $ TokenPrimConjunction <$> oneOf G.conjunctions

    unwrapConjunction :: Parser Token
    unwrapConjunction = withPos $ TokenUnwrapConjunction <$> (string [G.underscore, G.unwrap, G.underscore] *> bit)

  conjunction'' :: Parser Token
  conjunction'' = conjunctionAssign where
    conjunctionAssign :: Parser Token
    conjunctionAssign = assign' TokenConjunctionAssign conjunctionName

  extraArgs :: Parser Token
  extraArgs = withPos $ do
    lexeme $ char $ fst G.extraArgs
    choice [TokenExtraArgs [] <$ lexeme (char $ snd G.extraArgs), do
      firstKey <- bits
      choice [TokenSpreadExtraArgs firstKey <$ lexeme (char $ snd G.extraArgs), do
        lexeme $ char G.guard
        firstValue <- bits
        choice [TokenExtraArgs [(firstKey, firstValue)] <$ lexeme (char $ snd G.extraArgs), do
          separator
          otherEntries <- sepBy1 (liftA2 (,) (bits <* lexeme (char G.guard)) bits) separator
          lexeme $ char $ snd G.extraArgs
          pure $ TokenExtraArgs $ (firstKey, firstValue) : otherEntries]]]

  bracketed :: Parser Token
  bracketed = withPos $ TokenParens <$> between (char $ fst G.parens) (char $ snd G.parens) bits

  separator :: Parser ()
  separator = void $ lexeme (char G.separator) <|> char '\n' <* some (char '\n')

  bit' :: Parser Token
  bit' = lexeme $ bracketed <|> nothing <|> conjunction' <|> adverb' <|> function' <|> array'

  bit :: Parser Token
  bit = lexeme $ nothing <|> conjunction'' <|> adverb'' <|> function'' <|> array'' <|>
    maybeQualifiedTie
      TokenTie
      [ (TokenQualifiedConjunctionName, TokenQualifiedConjunctionAssign, try conjunctionName)
      , (TokenQualifiedAdverbName, TokenQualifiedAdverbAssign, adverbName)
      , (TokenQualifiedFunctionName, TokenQualifiedFunctionAssign, functionName)
      , (TokenQualifiedArrayName, TokenQualifiedArrayAssign, arrayName) ]
    <|> conjunction' <|> adverb' <|> function' <|> array' <|> extraArgs

  bitsMaybe :: Parser [Token]
  bitsMaybe = spaceConsumer *> option [] (NE.toList <$> bits)

  bits :: Parser (NonEmpty Token)
  bits = spaceConsumer *> (do
    one <- NE.some1 bit
    option one $ fmap NE.singleton $ withPos $ do
      char $ fst G.ternary
      two <- NE.some1 bit
      char $ snd G.ternary
      three <- bits
      pure $ TokenTernary one two three)

  definedBits :: Parser [Token]
  definedBits = spaceConsumer *> (do
    choice [fmap singleton $ withPos $ do
      char G.exit
      TokenExit <$> bits, do
      head <- bits
      option (NE.toList head) $ fmap singleton $ withPos $ do
        char G.guard
        tail <- definedBits
        pure $ TokenGuard head tail, pure []])

isArrayName :: String -> Bool
isArrayName [] = False
isArrayName [x] | x `elem` [G.quad, G.quadQuote] = True
isArrayName (x : xs)
  | x == G.quad = isArrayName xs
  | otherwise = x `elem` ['a'..'z'] ++ [G.alpha, G.omega, G.delta, G.epsilon]

isFunctionName :: String -> Bool
isFunctionName [] = False
isFunctionName (x : xs)
  | x == G.quad = isFunctionName xs
  | otherwise = x `elem` ['A'..'Z'] ++ [G.alphaBar, G.omegaBar, G.deltaBar, G.del]

isAdverbName :: String -> Bool
isAdverbName [] = False
isAdverbName [_] = False
isAdverbName (x : xs) 
  | x == G.quad = isAdverbName xs
  | otherwise = x == '_' && not (last xs == '_')

isConjunctionName :: String -> Bool
isConjunctionName [] = False
isConjunctionName [_] = False
isConjunctionName (x : xs)
  | x == G.quad = isConjunctionName xs
  | otherwise = x == '_' && last xs == '_'

data Category
  = CatArray
  | CatFunction
  | CatAppliedFunction
  | CatAdverb
  | CatConjunction
  | CatExtraArgs
  | CatNothing
  deriving (Enum, Bounded, Eq, Ord)

instance Show Category where
  show CatArray           = "array"
  show CatFunction        = "function"
  show CatAppliedFunction = "applied function"
  show CatAdverb          = "monadic operator"
  show CatConjunction     = "dyadic operator"
  show CatExtraArgs       = "extra args"
  show CatNothing         = "nothing"

data Tree
  = Leaf { leafCategory :: Category, leafToken :: Token }
  | QualifiedBranch { qualifiedBranchCategory :: Category, qualifiedBranchHead :: Tree, qualifiedBranchNames :: NonEmpty String }
  | MonadCallBranch { monadCallBranchLeft :: Tree, monadCallBranchRight :: Tree }
  | DyadCallBranch { dyadCallBranchLeft :: Tree, dyadCallBranchRight :: Tree }
  | AdverbCallBranch { adverbCallBranchLeft :: Tree, adverbCallBranchRight :: Tree }
  | ConjunctionCallBranch { conjunctionCallBranchLeft :: Tree, conjunctionCallBranchRight :: Tree }
  | AssignBranch { assignmentBranchCategory :: Category, assignmentName :: String, assignBranchType :: AssignType, assignmentValue :: Tree }
  | QualifiedAssignBranch { qualifiedAssignBranchCategory :: Category, qualifiedAssignBranchHead :: Tree, qualifiedAssignBranchNames :: NonEmpty String, qualifiedAssignBranchType :: AssignType, qualifiedAssignBranchValue :: Tree }
  | VectorAssignBranch { vectorAssignBranchNames :: [String], vectorAssignBranchType :: AssignType, vectorAssignBranchValue :: Tree }
  | HighRankAssignBranch { highRankAssignBranchNames :: [String], highRankAssignBranchType :: AssignType, highRankAssignBranchValue :: Tree }
  | StructAssignBranch { structAssignBranchNames :: [(String, Maybe (AssignType, String))], structAssignBranchType :: AssignType, structAssignBranchValue :: Tree }
  | DefinedBranch { definedBranchCategory :: Category, definedBranchStatements :: NonEmpty Tree }
  | GuardBranch { guardBranchCheck :: Tree, guardBranchResult :: Tree }
  | ExitBranch { exitBranchResult :: Tree }
  | VectorBranch { vectorBranchEntries :: [Tree] }
  | HighRankBranch { highRankBranchEntries :: [Tree] }
  | DictionaryBranch { dictionaryBranchEntries :: [(Tree, Tree)] }
  | TrainBranch { trainBranchCategory :: Category, trainBranchStatements :: [Maybe Tree] }
  | WrapBranch { wrapBranchValue :: Tree }
  | UnwrapBranch { unwrapBranchCategory :: Category, unwrapBranchValue :: Tree }
  | StructBranch { structBranchStatements :: [Tree] }
  | TernaryBranch { ternaryBranchCondition :: Tree, ternaryBranchTrue :: Tree, ternaryBranchFalse :: Tree }
  | UnboundExtraArgsBranch { unboundExtraArgsBranchArgs :: Tree }
  | ExtraArgsBranch { extraArgsBranchCategory :: Category, extraArgsModified :: Tree, extraArgsBranchArgs :: Tree }
  deriving (Eq)

instance Show Tree where
  show tree = unlines $ go 0 tree where
    indentCount = 2
    go :: Int -> Tree -> [String]
    go i t = let indent = replicate (indentCount * i) ' ' in case t of
      (Leaf c l)                         -> [indent ++ show c ++ ": " ++ show l]
      (QualifiedBranch c h ns)           -> [indent ++ show c ++ ": ..." ++ [G.access] ++ intercalate [G.access] (NE.toList ns)] ++ go (i + 1) h
      (MonadCallBranch l r)              -> [indent ++ "monad call"] ++ go (i + 1) l ++ go (i + 1) r
      (DyadCallBranch l r)               -> [indent ++ "dyad left call"] ++ go (i + 1) l ++ go (i + 1) r
      (AdverbCallBranch l r)             -> [indent ++ "adverb call"] ++ go (i + 1) l ++ go (i + 1) r
      (ConjunctionCallBranch l r)        -> [indent ++ "conjunction right call"] ++ go (i + 1) l ++ go (i + 1) r
      (AssignBranch c n t v)             -> (indent ++ show c ++ " " ++ n ++ " " ++ [assignTypeArrow t] ++ "") : go (i + 1) v
      (QualifiedAssignBranch c h ns t v) -> (indent ++ show c ++ " ..." ++ [assignTypeArrow t] ++ intercalate [G.access] (NE.toList ns) ++ " ← ...") : go (i + 1) h ++ (indent ++ "←") : go (i + 1) v
      (VectorAssignBranch ns c v)        -> (indent ++ "⟨⟩ " ++ unwords (show <$> ns) ++ [' ', assignTypeArrow c]) : go (i + 1) v
      (HighRankAssignBranch ns c v)      -> (indent ++ "[] " ++ unwords (show <$> ns) ++ [' ', assignTypeArrow c]) : go (i + 1) v
      (StructAssignBranch ns c v)        -> (indent ++ "⦃⦄ " ++ unwords (uncurry (++) . second (maybe "" (\(c, n) -> assignTypeArrow c : n)) <$> ns) ++ [' ', assignTypeArrow c]) : go (i + 1) v
      (DefinedBranch c ts)               -> (indent ++ show c ++ " {}") : concatMap (go (i + 1)) ts
      (GuardBranch ch res)               -> [indent ++ "guard"] ++ go (i + 1) ch ++ [indent ++ ":"] ++ go (i + 1) res
      (ExitBranch res)                   -> (indent ++ "■") : go (i + 1) res
      (VectorBranch es)                  -> (indent ++ "⟨⟩") : concatMap (go (i + 1)) es
      (HighRankBranch es)                -> (indent ++ "[]") : concatMap (go (i + 1)) es
      (DictionaryBranch es)              -> (indent ++ "⟨:⟩") : concatMap (\(k, v) -> go (i + 1) k ++ go (i + 1) v) es
      (TrainBranch c ts)                 -> (indent ++ (if c == CatFunction then "" else "_") ++ "⦅" ++ (if c == CatConjunction then "_" else "") ++ "⦆") : concatMap (maybe [""] (go (i + 1))) ts
      (WrapBranch fn)                    -> (indent ++ "⊏") : go (i + 1) fn
      (UnwrapBranch c fn)                -> (indent ++ (if c == CatFunction then "" else "_") ++ "⊐" ++ (if c == CatConjunction then "_" else "")) : go (i + 1) fn
      (StructBranch ts)                  -> (indent ++ "⦃⦄") : concatMap (go (i + 1)) ts
      (TernaryBranch c t f)              -> (indent ++ "⍰⍠") : go (i + 1) c ++ go (i + 1) t ++ go (i + 1) f
      (UnboundExtraArgsBranch es)        -> (indent ++ "⦋⦌") : go (i + 1) es
      (ExtraArgsBranch _ m es)           -> (indent ++ "⦋⦌") : go (i + 1) m ++ go (i + 1) es

treeCategory :: Tree -> Category
treeCategory (Leaf c _)                        = c
treeCategory (QualifiedBranch c _ _)           = c
treeCategory (MonadCallBranch _ _)             = CatArray
treeCategory (DyadCallBranch _ _)              = CatAppliedFunction
treeCategory (AdverbCallBranch _ _)            = CatFunction
treeCategory (ConjunctionCallBranch _ _)       = CatAdverb
treeCategory (AssignBranch c _ _ _)            = c
treeCategory (QualifiedAssignBranch c _ _ _ _) = c
treeCategory (VectorAssignBranch _ _ _)        = CatArray
treeCategory (HighRankAssignBranch _ _ _)      = CatArray
treeCategory (StructAssignBranch _ _ _)        = CatArray
treeCategory (DefinedBranch c _)               = c
treeCategory (GuardBranch _ t)                 = treeCategory t
treeCategory (ExitBranch _)                    = CatArray
treeCategory (VectorBranch _)                  = CatArray
treeCategory (HighRankBranch _)                = CatArray
treeCategory (DictionaryBranch _)              = CatArray
treeCategory (TrainBranch c _)                 = c
treeCategory (WrapBranch _)                    = CatArray
treeCategory (UnwrapBranch c _)                = c
treeCategory (StructBranch _)                  = CatArray
treeCategory (TernaryBranch _ _ _)             = CatArray
treeCategory (UnboundExtraArgsBranch _)        = CatExtraArgs
treeCategory (ExtraArgsBranch c _ _)           = c

type BindingMap = [((Category, Category), (Int, Tree -> Tree -> Tree))]

bindingMap :: BindingMap
bindingMap =
  [ ((CatArray,           CatFunction),  (2, DyadCallBranch))
  , ((CatFunction,        CatArray),     (1, MonadCallBranch))
  , ((CatAppliedFunction, CatArray),     (1, MonadCallBranch))
  , ((CatFunction,        CatAdverb),    (3, AdverbCallBranch))
  , ((CatArray,           CatAdverb),    (3, AdverbCallBranch))
  , ((CatConjunction,     CatArray),     (3, ConjunctionCallBranch))
  , ((CatConjunction,     CatFunction),  (3, ConjunctionCallBranch))
  , ((CatFunction,        CatExtraArgs), (4, ExtraArgsBranch CatFunction))
  , ((CatAdverb,          CatExtraArgs), (4, ExtraArgsBranch CatAdverb))
  , ((CatConjunction,     CatExtraArgs), (4, ExtraArgsBranch CatConjunction)) ]

compactTrainBindingMap :: BindingMap
compactTrainBindingMap = filter (\case
  ((CatArray, CatFunction), _) -> False
  ((CatFunction, CatArray), _) -> False
  ((CatAppliedFunction, CatArray), _) -> False
  _ -> True) bindingMap

pairs :: BindingMap -> [Tree] -> [(Int, Tree -> Tree -> Tree)]
pairs map = mapAdjacent $ fromMaybe (0, undefined) .: (curry (`lookup` map) `on` treeCategory)

bindPairMaybe :: BindingMap -> NonEmpty Tree -> Result (Maybe (NonEmpty Tree))
bindPairMaybe _ x@(_ :| []) = pure $ Just x
bindPairMaybe map xs = let
  xs' = NE.toList xs
  (sts, trees) = unzip $ pairs map xs'
  maxBind = maximum sts
  nextBind = fromJust $ maxBind `elemIndex` sts
  tree = trees !! nextBind
  indexed = zip [0..] xs'
  in if maxBind == 0 then pure Nothing else pure $ Just $ NE.fromList $ mapMaybe (\(idx, el) ->
    if idx == nextBind then Just $ tree el $ xs' !! (idx + 1)
    else if idx == nextBind + 1 then Nothing
    else Just el) indexed

bindPair :: NonEmpty Tree -> Result (NonEmpty Tree)
bindPair x = bindPairMaybe bindingMap x >>= (\case
  Nothing -> throwError $ SyntaxError "No binding found"
  Just x' -> pure x')

bindAll :: NonEmpty Tree -> Result Tree
bindAll (x :| []) = pure x
bindAll xs = bindPair xs >>= bindAll

bindAllMultiple :: BindingMap -> NonEmpty Tree -> Result (NonEmpty Tree)
bindAllMultiple _ (x :| []) = pure $ NE.singleton x
bindAllMultiple map xs = bindPairMaybe map xs >>= (\case
  Nothing -> pure xs
  Just xs' -> bindAllMultiple map xs')

categorize :: String -> String -> Result [[Tree]]
categorize name source = tokenize name source >>= mapM (\xs -> case NE.nonEmpty xs of
  Nothing -> pure []
  Just xs -> NE.toList <$> categorizeTokens xs) where
  orEmptyToken :: [Token] -> NonEmpty Token
  orEmptyToken [] = TokenVector [] emptyPos :| []
  orEmptyToken xs = NE.fromList xs

  categorizeTokens :: NonEmpty Token -> Result (NonEmpty Tree)
  categorizeTokens = mapM tokenToTree

  categorizeAndBind :: NonEmpty Token -> Result Tree
  categorizeAndBind = categorizeTokens >=> bindAll

  categorizeAndBindMultiple :: BindingMap -> NonEmpty Token -> Result (NonEmpty Tree)
  categorizeAndBindMultiple map = categorizeTokens >=> bindAllMultiple map

  requireOfCategory :: Category -> (Category -> Error) -> Tree -> Result Tree
  requireOfCategory cat msg tree | treeCategory tree == cat = pure tree
                                 | otherwise                = throwError $ msg $ treeCategory tree

  qualified :: Category -> Token -> NonEmpty String -> Result Tree
  qualified cat h ns = QualifiedBranch cat <$> (tokenToTree h >>=
    requireOfCategory CatArray (\c -> makeSyntaxError (tokenPos h) source $ "Invalid qualified access to value of type " ++ show c)) <*> pure ns

  defined :: Category -> String -> NonEmpty [Token] -> SourcePos -> Result Tree
  defined cat name statements pos = do
    let statements' = orEmptyToken <$> statements
    ss <- mapM categorizeAndBind statements'
    if null ss then throwError $ makeSyntaxError pos source $ "Invalid empty " ++ name
    else if treeCategory (NE.last ss) /= CatArray then throwError $ makeSyntaxError (tokenPos $ NE.head $ NE.last statements') source $ "Invalid " ++ name ++ ": last statement must be an array"
    else Right $ DefinedBranch cat ss

  assignment :: Category -> String -> AssignType -> NonEmpty Token -> SourcePos -> Result Tree
  assignment cat name ty ts pos = AssignBranch cat name ty <$> (categorizeAndBind ts >>=
    requireOfCategory cat (\c -> makeSyntaxError pos source $ "Invalid assignment of " ++ show c ++ " to " ++ show cat ++ " name"))

  qualifiedAssignment :: Category -> Token -> NonEmpty String -> AssignType -> NonEmpty Token -> Result Tree
  qualifiedAssignment cat h ns ty ts = liftA2 (\h' as -> QualifiedAssignBranch cat h' ns ty as) (tokenToTree h >>=
    requireOfCategory CatArray (\c -> makeSyntaxError (tokenPos h) source $ "Invalid qualified access to value of type " ++ show c)) (categorizeAndBind ts >>=
    requireOfCategory cat (\c -> makeSyntaxError (tokenPos $ NE.head ts) source $ "Invalid assignment of " ++ show c ++ " to " ++ show cat ++ " name"))

  destructureAssignment :: ([String] -> AssignType -> Tree -> Tree) -> [String] -> AssignType -> NonEmpty Token -> SourcePos -> Result Tree
  destructureAssignment h names ty ts pos = h names ty <$> (categorizeAndBind ts >>= requireOfCategory CatArray (\c -> makeSyntaxError pos source $ "Invalid destructure assignment of " ++ show c ++ ", array required"))

  structAssignment :: [(String, Maybe (AssignType, String))] -> AssignType -> NonEmpty Token -> SourcePos -> Result Tree
  structAssignment names ty ts pos = do
    let ns = mapMaybe (\case { (_, Nothing) -> Nothing; (n, Just (_, n')) -> Just (n, n') }) names
    mapM_ (\(n, n') -> if not $
      (isArrayName n && isArrayName n') ||
      (isFunctionName n && isFunctionName n') ||
      (isAdverbName n && isAdverbName n') ||
      (isConjunctionName n && isConjunctionName n')
      then throwError $ makeSyntaxError pos source $ "Struct assignment: same type required for both the original and aliased name"
      else pure ()) ns
    StructAssignBranch names ty <$> (categorizeAndBind ts >>= requireOfCategory CatArray (\c -> makeSyntaxError pos source $ "Invalid struct assignment of " ++ show c ++ ", array required"))

  vector :: [NonEmpty Token] -> SourcePos -> Result Tree
  vector es _ = VectorBranch <$> mapM (\x -> categorizeAndBind x) es

  highRank :: [NonEmpty Token] -> SourcePos -> Result Tree
  highRank es _ = HighRankBranch <$> mapM (\x -> categorizeAndBind x >>=
    requireOfCategory CatArray (\c -> makeSyntaxError (tokenPos $ NE.head x) source $ "Invalid array entry of type " ++ show c ++ ", array required")) es

  dictionary :: [(NonEmpty Token, NonEmpty Token)] -> SourcePos -> Result Tree
  dictionary es _ = DictionaryBranch <$> mapM (\(k, v) -> liftA2 (,) (categorizeAndBind k) (categorizeAndBind v)) es

  train :: Category -> [[Token]] -> SourcePos -> Result Tree
  train cat es _ = TrainBranch cat <$> (mapM (\e -> case NE.nonEmpty e of
    Nothing -> return Nothing
    Just e' -> Just <$> categorizeAndBind e') es)

  compactTrain :: Category -> NonEmpty Token -> SourcePos -> Result Tree
  compactTrain cat es _ = TrainBranch cat . fmap Just . NE.toList <$> categorizeAndBindMultiple compactTrainBindingMap es

  struct :: [NonEmpty Token] -> SourcePos -> Result Tree
  struct es _ = StructBranch <$> mapM (\x -> categorizeAndBind x) es

  ternary :: NonEmpty Token -> NonEmpty Token -> NonEmpty Token -> Result Tree
  ternary h t f = do
    cond <- categorizeAndBind h >>= requireOfCategory CatArray (\c -> makeSyntaxError (tokenPos $ NE.head h) source $ "Invalid ternary condition of type " ++ show c ++ ", array required")
    true <- categorizeAndBind t >>= requireOfCategory CatArray (\c -> makeSyntaxError (tokenPos $ NE.head t) source $ "Invalid ternary true of type " ++ show c ++ ", array required")
    false <- categorizeAndBind f >>= requireOfCategory CatArray (\c -> makeSyntaxError (tokenPos $ NE.head f) source $ "Invalid ternary false of type " ++ show c ++ ", array required")
    pure $ TernaryBranch cond true false

  tokenToTree :: Token -> Result Tree
  tokenToTree num@(TokenNumber _ _)                         = return $ Leaf CatArray num
  tokenToTree ch@(TokenChar _ _)                            = return $ Leaf CatArray ch
  tokenToTree str@(TokenString _ _)                         = return $ Leaf CatArray str
  tokenToTree arr@(TokenPrimArray _ _)                      = return $ Leaf CatArray arr
  tokenToTree fn@(TokenPrimFunction _ _)                    = return $ Leaf CatFunction fn
  tokenToTree adv@(TokenPrimAdverb _ _)                     = return $ Leaf CatAdverb adv
  tokenToTree conj@(TokenPrimConjunction _ _)               = return $ Leaf CatConjunction conj
  tokenToTree (TokenDfn statements pos)                     = defined CatFunction "dfn" statements pos
  tokenToTree (TokenDadv statements pos)                    = defined CatAdverb "dadv" statements pos
  tokenToTree (TokenDconj statements pos)                   = defined CatConjunction "dconj" statements pos
  tokenToTree arr@(TokenArrayName _ _)                      = return $ Leaf CatArray arr
  tokenToTree fn@(TokenFunctionName _ _)                    = return $ Leaf CatFunction fn
  tokenToTree adv@(TokenAdverbName _ _)                     = return $ Leaf CatAdverb adv
  tokenToTree conj@(TokenConjunctionName _ _)               = return $ Leaf CatConjunction conj
  tokenToTree (TokenQualifiedArrayName h ns _)              = qualified CatArray h ns
  tokenToTree (TokenQualifiedFunctionName h ns _)           = qualified CatFunction h ns
  tokenToTree (TokenQualifiedAdverbName h ns _)             = qualified CatAdverb h ns
  tokenToTree (TokenQualifiedConjunctionName h ns _)        = qualified CatConjunction h ns
  tokenToTree (TokenArrayAssign name c ts pos)              = assignment CatArray name c ts pos
  tokenToTree (TokenFunctionAssign name c ts pos)           = assignment CatFunction name c ts pos
  tokenToTree (TokenAdverbAssign name c ts pos)             = assignment CatAdverb name c ts pos
  tokenToTree (TokenConjunctionAssign name c ts pos)        = assignment CatConjunction name c ts pos
  tokenToTree (TokenQualifiedArrayAssign h ns c ts _)       = qualifiedAssignment CatArray h ns c ts
  tokenToTree (TokenQualifiedFunctionAssign h ns c ts _)    = qualifiedAssignment CatFunction h ns c ts
  tokenToTree (TokenQualifiedAdverbAssign h ns c ts _)      = qualifiedAssignment CatAdverb h ns c ts
  tokenToTree (TokenQualifiedConjunctionAssign h ns c ts _) = qualifiedAssignment CatConjunction h ns c ts
  tokenToTree (TokenVectorAssign names c ts pos)            = destructureAssignment VectorAssignBranch names c ts pos
  tokenToTree (TokenHighRankAssign names c ts pos)          = destructureAssignment HighRankAssignBranch names c ts pos
  tokenToTree (TokenTieAssign names c ts pos)               = destructureAssignment VectorAssignBranch (NE.toList names) c ts pos
  tokenToTree (TokenStructAssign names c ts pos)            = structAssignment names c ts pos
  tokenToTree (TokenParens ts _)                            = categorizeAndBind ts
  tokenToTree (TokenGuard check result _)                   = liftA2 GuardBranch (categorizeAndBind check >>= requireOfCategory CatArray (\c -> makeSyntaxError (tokenPos $ NE.head check) source $ "Invalid guard of type " ++ show c ++ ", array required")) (categorizeAndBind $ orEmptyToken result)
  tokenToTree (TokenExit result _)                          = ExitBranch <$> (categorizeAndBind result >>= requireOfCategory CatArray (\c -> makeSyntaxError (tokenPos $ NE.head result) source $ "Invalid exit statement of type " ++ show c ++ ", array required"))
  tokenToTree (TokenVector es pos)                          = vector es pos
  tokenToTree (TokenHighRank es pos)                        = highRank es pos
  tokenToTree (TokenDictionary es pos)                      = dictionary es pos
  tokenToTree (TokenTrain [f : fs] pos)                     = compactTrain CatFunction (f :| fs) pos
  tokenToTree (TokenTrain fs pos)                           = train CatFunction fs pos
  tokenToTree (TokenAdverbTrain [f : fs] pos)               = compactTrain CatAdverb (f :| fs) pos
  tokenToTree (TokenAdverbTrain fs pos)                     = train CatAdverb fs pos
  tokenToTree (TokenConjunctionTrain [f : fs] pos)          = compactTrain CatConjunction (f :| fs) pos
  tokenToTree (TokenConjunctionTrain fs pos)                = train CatConjunction fs pos
  tokenToTree (TokenWrap val _)                             = WrapBranch <$> (tokenToTree val >>= (\x -> case treeCategory x of
    CatFunction -> pure x
    CatAdverb -> pure x
    CatConjunction -> pure x
    _ -> throwError $ makeSyntaxError (tokenPos val) source $ "Invalid wrap of type " ++ show (treeCategory x) ++ ", function, adverb or conjunction required"))
  tokenToTree (TokenUnwrap val _)                           = UnwrapBranch CatFunction <$> (tokenToTree val >>= requireOfCategory CatArray (\c -> makeSyntaxError (tokenPos val) source $ "Invalid unwrap of type " ++ show c ++ ", array required"))
  tokenToTree (TokenUnwrapAdverb val _)                     = UnwrapBranch CatAdverb <$> (tokenToTree val >>= requireOfCategory CatArray (\c -> makeSyntaxError (tokenPos val) source $ "Invalid unwrap adverb of type " ++ show c ++ ", array required"))
  tokenToTree (TokenUnwrapConjunction val _)                = UnwrapBranch CatConjunction <$> (tokenToTree val >>= requireOfCategory CatArray (\c -> makeSyntaxError (tokenPos val) source $ "Invalid unwrap conjunction of type " ++ show c ++ ", array required"))
  tokenToTree (TokenStruct es pos)                          = struct es pos
  tokenToTree (TokenTie es pos)                             = vector (NE.toList $ fmap NE.singleton $ es) pos
  tokenToTree (TokenTernary c t f _)                        = ternary c t f
  tokenToTree (TokenExtraArgs es pos)                       = UnboundExtraArgsBranch <$> dictionary es pos
  tokenToTree (TokenSpreadExtraArgs es _)                   = categorizeAndBind es >>= (\e -> case treeCategory e of
    CatArray -> pure $ UnboundExtraArgsBranch e
    _ -> throwError $ makeSyntaxError (tokenPos $ NE.head es) source $ "Extra args must be arrays")
  tokenToTree (TokenNothing pos)                            = pure $ Leaf CatNothing (TokenNothing pos)

parse :: String -> String -> Result [Maybe Tree]
parse name = categorize name >=> mapM (\xs -> case NE.nonEmpty xs of
  Nothing -> pure Nothing
  Just xs -> Just <$> bindAll xs)
