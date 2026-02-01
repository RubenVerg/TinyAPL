{-# LANGUAGE PatternSynonyms, OverloadedStrings, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, FunctionalDependencies, NamedFieldPuns, TupleSections #-}

module TinyAPL.Pretty
  ( Doc
  , pattern Doc
  , render'
  , render
  , PrettyPrint(..)
  , runPretty
  , BoxDrawing(..)
  , PrettyConfig(..)
  , defaultConfig
  , boxed
  , list ) where

import TinyAPL.Noun
import TinyAPL.Function
import TinyAPL.Adverb
import TinyAPL.Conjunction
import TinyAPL.Value
import TinyAPL.Context
import TinyAPL.Interpreter
import TinyAPL.Util
import qualified TinyAPL.Glyphs as G

import Data.Functor.Identity
import Data.Maybe
import Data.List (intersperse)
import qualified Data.List as List
import qualified Data.Char.BoxDrawing as B
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as T

newtype Doc = Doc' [Text] deriving (Eq)

{-# COMPLETE Doc #-}
pattern Doc :: [Text] -> Doc
pattern Doc xs <- Doc' xs where
  Doc [] = Doc' []
  Doc xs = Doc' $ fmap (flip T.justifyLeft ' ' $ maximum $ T.length <$> xs) xs

instance Show Doc where
  show = render

render' :: Doc -> Text
render' (Doc xs) = T.intercalate "\n" xs

render :: Doc -> String
render = T.unpack . render'

empty :: Doc
empty = Doc []

width :: Doc -> Int
width (Doc []) = 0
width (Doc xs) = maximum $ T.length <$> xs

height :: Doc -> Int
height (Doc xs) = length xs

size :: Doc -> (Int, Int)
size = (,) <$> width <*> height

toSize :: (Int, Int) -> Doc -> Doc
toSize (w', h') (Doc xs) = Doc $ take h' $ map (T.take w' . flip T.justifyLeft ' ' w') xs ++ repeat ""

text' :: Text -> Doc
text' = Doc . T.splitOn "\n"

text :: String -> Doc
text = text' . T.pack

char :: Char -> Doc
char = text' . T.singleton

transpose :: Doc -> Doc
transpose (Doc xs) = Doc $ T.transpose xs

reverseH :: Doc -> Doc
reverseH (Doc xs) = Doc $ map T.reverse xs

joinH :: Doc -> Doc -> Doc
joinH x@(Doc xs) y@(Doc ys) = let
  hx = height x
  hy = height y
  hr = max hx hy
  Doc xp = Doc $ take hr $ xs ++ repeat ""
  Doc yp = Doc $ take hr $ ys ++ repeat ""
  rs = zipWith (<>) xp yp
  in Doc rs

infixr 6 <|>
(<|>) :: Doc -> Doc -> Doc
(<|>) = joinH

joinV :: Doc -> Doc -> Doc
joinV (Doc xs) (Doc ys) = Doc $ xs ++ ys

infixr 6 <->
(<->) :: Doc -> Doc -> Doc
(<->) = joinV

class Monad m => PrettyPrint m s a | a -> s where
  prettyM :: a -> ReaderT s m Doc

runPretty :: (Monad m, PrettyPrint m s a) => s -> a -> m String
runPretty s a = fmap render $ flip runReaderT s $ prettyM a

data BoxDrawing
  = TopLeft
  | TopTee
  | TopRight
  | LeftTee
  | Cross
  | RightTee
  | BottomLeft
  | BottomTee
  | BottomRight
  | Vertical
  | Horizontal
  | Enclosed
  | Wrapped
  | Axis0
  | Axis1
  | Axis2
  | Pairs
  | Struct1
  | Struct2
  deriving (Eq, Ord, Enum, Bounded, Show)

defaultBoxDrawings :: BoxDrawing -> Either B.Drawing Char
defaultBoxDrawings TopLeft = Left B.cornerTL
defaultBoxDrawings TopTee = Left B.intersectT
defaultBoxDrawings TopRight = Left B.cornerTR
defaultBoxDrawings LeftTee = Left B.intersectL
defaultBoxDrawings Cross = Left B.intersectFull
defaultBoxDrawings RightTee = Left B.intersectR
defaultBoxDrawings BottomLeft = Left B.cornerBL
defaultBoxDrawings BottomTee = Left B.intersectB
defaultBoxDrawings BottomRight = Left B.cornerBR
defaultBoxDrawings Vertical = Left B.vertical
defaultBoxDrawings Horizontal = Left B.horizontal
defaultBoxDrawings Enclosed = Right G.enclose
defaultBoxDrawings Wrapped = Right G.wrap
defaultBoxDrawings Axis0 = Right '→'
defaultBoxDrawings Axis1 = Right '↓'
defaultBoxDrawings Axis2 = Right '↘'
defaultBoxDrawings Pairs = Right ':'
defaultBoxDrawings Struct1 = Right $ fst G.struct
defaultBoxDrawings Struct2 = Right $ snd G.struct

data PrettyConfig = PrettyConfig { drawings :: [(BoxDrawing, Char)] }

defaultConfig :: PrettyConfig
defaultConfig = PrettyConfig{ drawings = [] }

renderBox :: MonadReader PrettyConfig m => BoxDrawing -> m Char
renderBox box = do
  ds <- asks drawings
  pure $ case lookup box ds of
    Just c -> c
    Nothing -> case defaultBoxDrawings box of
      Left b -> B.render B.unicode b
      Right c -> c

data Annot = Annot { top :: Maybe BoxDrawing, side :: Maybe BoxDrawing, corner :: Maybe BoxDrawing }

annot :: Annot
annot = Annot { top = Nothing, side = Nothing, corner = Nothing }

boxed :: MonadReader PrettyConfig m => Annot -> Doc -> m Doc
boxed Annot{ top = top', side = side', corner = corner' } doc = do
  top <- mapM renderBox top'
  side <- mapM renderBox side'
  corner <- mapM renderBox corner'
  horiz <- renderBox Horizontal
  vert <- renderBox Vertical
  tl <- renderBox TopLeft
  tr <- renderBox TopRight
  bl <- renderBox BottomLeft
  br <- renderBox BottomRight
  let (w, h) = size doc
  let topLine = text $ fromMaybe horiz top : replicate (w-1) horiz
  let bottomLine = text $ replicate w horiz
  let vp = topLine <-> doc <-> bottomLine
  let leftLine = transpose $ text $ (fromMaybe tl corner : fromMaybe vert side : replicate (h-1) vert) :> bl
  let rightLine = transpose $ text $ (tr : replicate h vert) :> br
  pure $ leftLine <|> vp <|> rightLine

list :: MonadReader PrettyConfig m => Annot -> [Doc] -> m Doc
list Annot{ top = top', side = side', corner = corner' } [] = do
  top <- mapM renderBox top'
  side <- mapM renderBox side'
  corner <- mapM renderBox corner'
  horiz <- renderBox Horizontal
  vert <- renderBox Vertical
  tl <- renderBox TopLeft
  tr <- renderBox TopRight
  bl <- renderBox BottomLeft
  br <- renderBox BottomRight
  let topLine = text [fromMaybe tl corner, fromMaybe horiz top, tr]
  let middleLine = text [fromMaybe vert side, ' ', vert]
  let bottomLine = text [bl, horiz, br]
  pure $ topLine <-> middleLine <-> bottomLine
list Annot{ top = top', side = side', corner = corner' } docs = do
  top <- mapM renderBox top'
  side <- mapM renderBox side'
  corner <- mapM renderBox corner'
  horiz <- renderBox Horizontal
  vert <- renderBox Vertical
  tl <- renderBox TopLeft
  tr <- renderBox TopRight
  bl <- renderBox BottomLeft
  br <- renderBox BottomRight
  it <- renderBox TopTee
  ib <- renderBox BottomTee
  let h = maximum $ map height docs
  let tb first doc = let {
    w = width doc ;
    topLine = text $ (if first then fromMaybe horiz top else horiz) : replicate (w-1) horiz ;
    bottomLine = text $ replicate w horiz ;
    } in topLine <-> toSize (w, h) doc <-> bottomLine
  let middleLine = transpose $ text $ (it : replicate h vert) :> ib
  let j = foldr1 (<|>) $ intersperse middleLine $ zipWith tb (True : repeat False) docs
  let leftLine = transpose $ text $ (fromMaybe tl corner : fromMaybe vert side : replicate (h-1) vert) :> bl
  let rightLine = transpose $ text $ (tr : replicate h vert) :> br
  pure $ leftLine <|> j <|> rightLine

table :: MonadReader PrettyConfig m => Annot -> [[Doc]] -> m Doc
table Annot{ top = top', side = side', corner = corner' } docss | docss == [] || docss == [[]] = do
  top <- mapM renderBox top'
  side <- mapM renderBox side'
  corner <- mapM renderBox corner'
  horiz <- renderBox Horizontal
  vert <- renderBox Vertical
  tl <- renderBox TopLeft
  tr <- renderBox TopRight
  bl <- renderBox BottomLeft
  br <- renderBox BottomRight
  let topLine = text [fromMaybe tl corner, fromMaybe horiz top, tr]
  let middleLine = text [fromMaybe vert side, ' ', vert]
  let bottomLine = text [bl, horiz, br]
  pure $ topLine <-> middleLine <-> bottomLine
table Annot{ top = top', side = side', corner = corner' } docss = do
  top <- mapM renderBox top'
  side <- mapM renderBox side'
  corner <- mapM renderBox corner'
  horiz <- renderBox Horizontal
  vert <- renderBox Vertical
  tl <- renderBox TopLeft
  tr <- renderBox TopRight
  bl <- renderBox BottomLeft
  br <- renderBox BottomRight
  it <- renderBox TopTee
  ib <- renderBox BottomTee
  il <- renderBox LeftTee
  ir <- renderBox RightTee
  cross <- renderBox Cross
  let hs = map (maximum . map height) docss
  let ws = map (maximum . map width) $ List.transpose docss
  let row first h docs = let {
    middleLine = transpose $ text $ replicate h vert ;
    leftLine = transpose $ text $ (if first then fromMaybe vert side else vert) : replicate (h-1) vert ;
    rightLine = transpose $ text $ replicate h vert
    } in leftLine <|> foldr1 (<|>) (intersperse middleLine $ zipWith (\w doc -> toSize (w, h) doc) ws docs) <|> rightLine
  let middleLine = text $ (il : concat (intersperse [cross] $ map (flip replicate horiz) ws)) :> ir
  let j = foldr1 (<->) $ intersperse middleLine $ zipWith3 row (True : repeat False) hs docss
  let topLine = text $ (fromMaybe tl corner : concat (intersperse [it] $ zipWith (\first w -> if first then fromMaybe horiz top : replicate (w-1) horiz else replicate w horiz) (True : repeat False) ws)) :> tr
  let bottomLine = text $ (bl : concat (intersperse [ib] $ map (flip replicate horiz) ws)) :> br
  pure $ topLine <-> j <-> bottomLine

instance PrettyPrint Identity PrettyConfig ScalarValue where
  prettyM (Number x) = pure $ text $ showComplex x
  prettyM (Character x) = pure $ char x
  prettyM (Box xs) = prettyM xs >>= boxed annot{ top = Just Enclosed }
  prettyM (Wrap fn) = prettyM fn >>= boxed annot{ top = Just Wrapped }
  prettyM (AdverbWrap adv) = prettyM adv >>= boxed annot{ top = Just Wrapped }
  prettyM (ConjunctionWrap conj) = prettyM conj >>= boxed annot{ top = Just Wrapped }
  prettyM (Struct _) = pure $ text $ [fst G.struct] ++ "..." ++ [snd G.struct]

instance PrettyPrint St PrettyConfig ScalarValue where
  prettyM (Number x) = pure $ text $ showComplex x
  prettyM (Character x) = pure $ char x
  prettyM (Box xs) = prettyM xs >>= boxed annot{ top = Just Enclosed }
  prettyM (Wrap fn) = prettyM fn >>= boxed annot{ top = Just Wrapped }
  prettyM (AdverbWrap adv) = prettyM adv >>= boxed annot{ top = Just Wrapped }
  prettyM (ConjunctionWrap conj) = prettyM conj >>= boxed annot{ top = Just Wrapped }
  prettyM (Struct ctx) = do
    scope <- lift $ readRef $ contextScope ctx
    dShow <- lift $ scopeLookupNoun False (G.delta : "show") scope
    case dShow of
      Just dShow' -> prettyM dShow'
      Nothing -> do
        let entries = scopeEntries scope
        let
          varArrow VariableNormal = G.assign
          varArrow VariableConstant = G.assignConstant
          varArrow VariablePrivate = G.assignPrivate
        pairs <- flip mapM entries $ \(name, (typ, val)) -> (text name, ) . (text [' ', varArrow typ, ' '] <|>) <$> prettyM val
        let mnw = maximum $ map (width . fst) pairs
        let ps = map (\(n, r) -> toSize (mnw, height n) n <|> r) pairs
        boxed annot{ corner = Just Struct1, top = Just Struct2 } $ foldr (<->) empty ps

prettyElementM :: (Monad m, PrettyPrint m PrettyConfig ScalarValue, MonadShow m ScalarValue) => ScalarValue -> ReaderT PrettyConfig m Doc
prettyElementM (Box xs) = prettyM xs
prettyElementM (Wrap fn) = prettyM fn
prettyElementM (AdverbWrap adv) = prettyM adv
prettyElementM (ConjunctionWrap conj) = prettyM conj
prettyElementM x = prettyM x

instance (Monad m, PrettyPrint m PrettyConfig ScalarValue, MonadShow m ScalarValue) => PrettyPrint m PrettyConfig Noun where
  prettyM (Array [] [x]) = prettyM x
  prettyM (Array [_] []) = pure $ text [fst $ G.vector, snd $ G.vector]
  prettyM (Array sh []) = pure $ text $ (List.intercalate [G.tie] $ map show sh) ++ [G.rho, G.zilde]
  prettyM (Array [_] xs)
    | not (null xs) && all isCharacter xs = pure $ text $ xs >>= runIdentity . showM
    | otherwise = mapM prettyElementM xs >>= list annot{ top = Just Axis0 }
  prettyM arr@(Array [_, _] _) = mapM (mapM prettyElementM . arrayContents) (majorCells arr) >>= table annot{ top = Just Axis0, side = Just Axis1 }
  prettyM (Dictionary [] []) = pure $ text [fst $ G.vector, G.guard, snd $ G.vector]
  prettyM (Dictionary ks vs) = do
    ks'' <- mapM prettyElementM ks
    let mkw = maximum $ map width ks''
    let ks' = map (\k -> reverseH $ toSize (mkw, height k) $ reverseH k) ks''
    vs' <- mapM prettyElementM vs
    let ps = zipWith (\k v -> k <|> text " : " <|> v) ks' vs'
    boxed annot{ top = Just Pairs } $ foldr (<->) empty ps
  prettyM arr = fmap text $ lift $ showM arr

-- copout
instance (Monad m, MonadShow m ScalarValue) => PrettyPrint m PrettyConfig Function where
  prettyM = fmap text . lift . showM

instance (Monad m, MonadShow m ScalarValue) => PrettyPrint m PrettyConfig Adverb where
  prettyM = fmap text . lift . showM

instance (Monad m, MonadShow m ScalarValue) => PrettyPrint m PrettyConfig Conjunction where
  prettyM = fmap text . lift . showM

instance (Monad m, MonadShow m ScalarValue, PrettyPrint m PrettyConfig ScalarValue) => PrettyPrint m PrettyConfig Value where
  prettyM (VNoun n) = prettyM n
  prettyM (VFunction fn) = prettyM fn
  prettyM (VAdverb adv) = prettyM adv
  prettyM (VConjunction conj) = prettyM conj
