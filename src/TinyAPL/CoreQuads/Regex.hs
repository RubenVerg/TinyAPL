{-# LANGUAGE FlexibleContexts #-}

module TinyAPL.CoreQuads.Regex where

import TinyAPL.Context
import TinyAPL.Error
import TinyAPL.Function
import TinyAPL.Noun
import TinyAPL.Quads
import qualified TinyAPL.Glyphs as G
import TinyAPL.Util (headPromise)

import Control.Monad.Error.Class (MonadError)
import qualified Text.Regex.Base as RE
import qualified Text.Regex.PCRE as RE
import Data.Array (elems)
import Data.List

search :: MonadError Error m => String -> String -> m [[(Int, Int, String)]]
search pat str = do
  let matArr = str RE.=~ pat :: [RE.MatchText String]
  let mats = elems <$> matArr
  pure $ fmap (\(str, (o, l)) -> (o, l, str)) <$> mats

search' :: MonadError Error m => Noun -> Noun -> m Noun
search' pattern arr = do
  pat <- asString (DomainError "Search left argument must be a string") pattern
  (sh, strs) <- asArrayOfStrings (DomainError "Search right argument must be an array of strings") arr
  (\r -> if null sh then fromScalar (headPromise r) else Array sh r) <$> (mapM (\str -> do
    s <- search pat str
    pure $ box $ Array [genericLength s] $ (\mats ->
      box $ Array [genericLength mats, 3] $ concatMap (\(o, l, str) ->
        [Number $ fromInteger $ toInteger o, Number $ fromInteger $ toInteger l, box $ vector $ Character <$> str]) mats) <$> s) strs)

regex = Nilad (Just $ do
  scope <- createRef $ Scope [] [ ("Search", (VariableConstant, PrimitiveFunction (FunctionCalls Nothing (Just $ const search') Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) "Search" Nothing)) ] [] [] Nothing True
  ctx <- getContext
  pure $ scalar $ Struct $ ctx{ contextScope = scope } ) Nothing (G.quad : "regex") Nothing
