{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, DeriveGeneric, DeriveAnyClass #-}

module TinyAPL.Context
  ( VariableType(..)
  , Scope(..)
  , scopeShallowLookupNoun
  , scopeShallowLookupFunction
  , scopeShallowLookupAdverb
  , scopeShallowLookupConjunction
  , scopeLookupNoun
  , scopeLookupFunction
  , scopeLookupAdverb
  , scopeLookupConjunction
  , scopeUpdateNoun
  , scopeUpdateFunction
  , scopeUpdateAdverb
  , scopeUpdateConjunction
  , scopeModifyNoun
  , scopeModifyFunction
  , scopeModifyAdverb
  , scopeModifyConjunction
  , scopeShallowModifyNoun
  , scopeShallowModifyFunction
  , scopeShallowModifyAdverb
  , scopeShallowModifyConjunction
  , Primitives
  , Context(..)
  , assignId
  , St
  , runSt
  , runWithContext
  , liftToSt
  , getContext
  , getsContext
  , putContext
  , putScope
  , createRef
  , readRef
  , writeRef
  , modifyRef ) where

import TinyAPL.Noun
import TinyAPL.Error
import TinyAPL.Util
import {-# SOURCE #-} TinyAPL.Adverb
import {-# SOURCE #-} TinyAPL.Conjunction
import {-# SOURCE #-} TinyAPL.Function
import {-# SOURCE #-} TinyAPL.Quads
import qualified TinyAPL.Glyphs as G

import Control.Monad.State.Strict
import Control.Monad.Except
import Control.DeepSeq
import Data.IORef (IORef)
import qualified Data.IORef as IORef
import Data.Functor.Identity (Identity(runIdentity))
import GHC.Generics
import Text.Printf

data VariableType
  = VariableNormal
  | VariableConstant
  | VariablePrivate
  deriving (Show, Eq, Generic, NFData)

instance Monad m => MonadShow m VariableType where
  showM = pure . show

data Scope = Scope
  { scopeNouns :: [(String, (VariableType, Noun))]
  , scopeFunctions :: [(String, (VariableType, Function))]
  , scopeAdverbs :: [(String, (VariableType, Adverb))]
  , scopeConjunctions :: [(String, (VariableType, Conjunction))]
  , scopeParent :: Maybe (IORef Scope)
  , scopeIsStruct :: Bool }
  deriving (Generic, NFData)

instance (Monad m, MonadShow m ScalarValue) => MonadShow m Scope where
  showM (Scope nouns fns advs conjs parent str) = (printf "Scope { nouns = %s, functions = %s, adverbs = %s, conjunctions = %s, %s, is struct: %s }") <$> showM nouns <*> showM fns <*> showM advs <*> showM conjs <*> (pure $ case parent of
    Nothing -> "no parent"
    Just _ -> "a parent") <*> pure (show str)

specialNames :: [String]
specialNames = [[G.alpha], [G.omega], [G.alpha, G.alpha], [G.omega, G.omega], [G.alphaBar, G.alphaBar], [G.omegaBar, G.omegaBar], [G.del], [G.underscore, G.del], [G.underscore, G.del, G.underscore]]

scopeShallowLookupNoun :: Bool -> String -> Scope -> Maybe Noun
scopeShallowLookupNoun private name sc = case lookup name (scopeNouns sc) of
  Nothing -> Nothing
  Just (VariableNormal, x) -> Just x
  Just (VariableConstant, x) -> Just x
  Just (VariablePrivate, x) | private -> Just x
  Just (VariablePrivate, _) -> Nothing

scopeShallowLookupFunction :: Bool -> String -> Scope -> Maybe Function
scopeShallowLookupFunction private name sc = case lookup name (scopeFunctions sc) of
  Nothing -> Nothing
  Just (VariableNormal, x) -> Just x
  Just (VariableConstant, x) -> Just x
  Just (VariablePrivate, x) | private -> Just x
  Just (VariablePrivate, _) -> Nothing

scopeShallowLookupAdverb :: Bool -> String -> Scope -> Maybe Adverb
scopeShallowLookupAdverb private name sc = case lookup name (scopeAdverbs sc) of
  Nothing -> Nothing
  Just (VariableNormal, x) -> Just x
  Just (VariableConstant, x) -> Just x
  Just (VariablePrivate, x) | private -> Just x
  Just (VariablePrivate, _) -> Nothing

scopeShallowLookupConjunction :: Bool -> String -> Scope -> Maybe Conjunction
scopeShallowLookupConjunction private name sc = case lookup name (scopeConjunctions sc) of
  Nothing -> Nothing
  Just (VariableNormal, x) -> Just x
  Just (VariableConstant, x) -> Just x
  Just (VariablePrivate, x) | private -> Just x
  Just (VariablePrivate, _) -> Nothing

scopeLookupNoun :: Bool ->  String -> Scope -> St (Maybe Noun)
scopeLookupNoun private name sc = case scopeShallowLookupNoun private name sc of
  Just x -> pure $ Just x
  Nothing -> if name `elem` specialNames then pure Nothing else case scopeParent sc of
    Nothing -> pure Nothing
    Just p -> (liftToSt $ IORef.readIORef p) >>= scopeLookupNoun private name

scopeLookupFunction :: Bool -> String -> Scope -> St (Maybe Function)
scopeLookupFunction private name sc = case scopeShallowLookupFunction private name sc of
  Just x -> pure $ Just x
  Nothing -> if name `elem` specialNames then pure $ Nothing else case scopeParent sc of
    Nothing -> pure $ Nothing
    Just p -> (liftToSt $ IORef.readIORef p) >>= scopeLookupFunction private name

scopeLookupAdverb :: Bool -> String -> Scope -> St (Maybe Adverb)
scopeLookupAdverb private name sc = case scopeShallowLookupAdverb private name sc of
  Just x -> pure $ Just x
  Nothing -> if name `elem` specialNames then pure $ Nothing else case scopeParent sc of
    Nothing -> pure $ Nothing
    Just p -> (liftToSt $ IORef.readIORef p) >>= scopeLookupAdverb private name

scopeLookupConjunction :: Bool -> String -> Scope -> St (Maybe Conjunction)
scopeLookupConjunction private name sc = case scopeShallowLookupConjunction private name sc of
  Just x -> pure $ Just x
  Nothing -> if name `elem` specialNames then pure $ Nothing else case scopeParent sc of
    Nothing -> pure $ Nothing
    Just p -> (liftToSt $ IORef.readIORef p) >>= scopeLookupConjunction private name

scopeUpdateNoun :: Bool ->  String -> VariableType -> Noun -> Scope -> St Scope
scopeUpdateNoun private name ty val sc = case lookup name (scopeNouns sc) of
  Nothing -> pure $ sc{ scopeNouns = update name (ty, val) (scopeNouns sc) }
  Just (VariableNormal, _) -> pure $ sc{ scopeNouns = update name (VariableNormal, val) (scopeNouns sc) }
  Just (VariablePrivate, _) | private -> pure $ sc{ scopeNouns = update name (VariablePrivate, val) (scopeNouns sc) }
  Just (VariablePrivate, _) -> throwError $ DomainError "Cannot access private variable"
  Just (VariableConstant, _) -> throwError $ DomainError "Cannot modify constant variable"

scopeUpdateFunction :: Bool -> String -> VariableType -> Function -> Scope -> St Scope
scopeUpdateFunction private name ty val sc = case lookup name (scopeFunctions sc) of
  Nothing -> pure $ sc{ scopeFunctions = update name (ty, val) (scopeFunctions sc) }
  Just (VariableNormal, _) -> pure $ sc{ scopeFunctions = update name (VariableNormal, val) (scopeFunctions sc) }
  Just (VariablePrivate, _) | private -> pure $ sc{ scopeFunctions = update name (VariablePrivate, val) (scopeFunctions sc) }
  Just (VariablePrivate, _) -> throwError $ DomainError "Cannot access private variable"
  Just (VariableConstant, _) -> throwError $ DomainError "Cannot modify constant variable"

scopeUpdateAdverb :: Bool -> String -> VariableType -> Adverb -> Scope -> St Scope
scopeUpdateAdverb private name ty val sc = case lookup name (scopeAdverbs sc) of
  Nothing -> pure $ sc{ scopeAdverbs = update name (ty, val) (scopeAdverbs sc) }
  Just (VariableNormal, _) -> pure $ sc{ scopeAdverbs = update name (VariableNormal, val) (scopeAdverbs sc) }
  Just (VariablePrivate, _) | private -> pure $ sc{ scopeAdverbs = update name (VariablePrivate, val) (scopeAdverbs sc) }
  Just (VariablePrivate, _) -> throwError $ DomainError "Cannot access private variable"
  Just (VariableConstant, _) -> throwError $ DomainError "Cannot modify constant variable"

scopeUpdateConjunction :: Bool -> String -> VariableType -> Conjunction -> Scope -> St Scope
scopeUpdateConjunction private name ty val sc = case lookup name (scopeConjunctions sc) of
  Nothing -> pure $ sc{ scopeConjunctions = update name (ty, val) (scopeConjunctions sc) }
  Just (VariableNormal, _) -> pure $ sc{ scopeConjunctions = update name (VariableNormal, val) (scopeConjunctions sc) }
  Just (VariablePrivate, _) | private -> pure $ sc{ scopeConjunctions = update name (VariablePrivate, val) (scopeConjunctions sc) }
  Just (VariablePrivate, _) -> throwError $ DomainError "Cannot access private variable"
  Just (VariableConstant, _) -> throwError $ DomainError "Cannot modify constant variable"

scopeModifyNoun :: Bool -> String -> Noun -> Scope -> St Scope
scopeModifyNoun private name val sc = if name `elem` specialNames then throwError $ DomainError "Cannot modify special variable" else case lookup name (scopeNouns sc) of
  Just (t, _) -> scopeUpdateNoun private name t val sc
  Nothing -> case scopeParent sc of
    Nothing -> throwError $ DomainError "Modifying a non-existent variable"
    Just p -> readRef p >>= scopeModifyNoun private name val >>= writeRef p >>= const (pure sc)

scopeModifyFunction :: Bool -> String -> Function -> Scope -> St Scope
scopeModifyFunction private name val sc = if name `elem` specialNames then throwError $ DomainError "Cannot modify special variable" else case lookup name (scopeFunctions sc) of
  Just _ -> scopeUpdateFunction private name VariableNormal val sc
  Nothing -> case scopeParent sc of
    Nothing -> throwError $ DomainError "Modifying a non-existent variable"
    Just p -> readRef p >>= scopeModifyFunction private name val >>= writeRef p >>= const (pure sc)

scopeModifyAdverb :: Bool -> String -> Adverb -> Scope -> St Scope
scopeModifyAdverb private name val sc = if name `elem` specialNames then throwError $ DomainError "Cannot modify special variable" else case lookup name (scopeAdverbs sc) of
  Just _ -> scopeUpdateAdverb private name VariableNormal val sc
  Nothing -> case scopeParent sc of
    Nothing -> throwError $ DomainError "Modifying a non-existent variable"
    Just p -> readRef p >>= scopeModifyAdverb private name val >>= writeRef p >>= const (pure sc)

scopeModifyConjunction :: Bool -> String -> Conjunction -> Scope -> St Scope
scopeModifyConjunction private name val sc = if name `elem` specialNames then throwError $ DomainError "Cannot modify special variable" else case lookup name (scopeConjunctions sc) of
  Just _ -> scopeUpdateConjunction private name VariableNormal val sc
  Nothing -> case scopeParent sc of
    Nothing -> throwError $ DomainError "Modifying a non-existent variable"
    Just p -> readRef p >>= scopeModifyConjunction private name val >>= writeRef p >>= const (pure sc)

scopeShallowModifyNoun :: Bool -> String -> Noun -> Scope -> St Scope
scopeShallowModifyNoun private name val sc = if name `elem` specialNames then throwError $ DomainError "Cannot modify special variable" else case lookup name (scopeNouns sc) of
  Just _ -> scopeUpdateNoun private name VariableNormal val sc
  Nothing -> throwError $ DomainError "Modifying a non-existent variable"

scopeShallowModifyFunction :: Bool -> String -> Function -> Scope -> St Scope
scopeShallowModifyFunction private name val sc = if name `elem` specialNames then throwError $ DomainError "Cannot modify special variable" else case lookup name (scopeFunctions sc) of
  Just _ -> scopeUpdateFunction private name VariableNormal val sc
  Nothing -> throwError $ DomainError "Modifying a non-existent variable"

scopeShallowModifyAdverb :: Bool -> String -> Adverb -> Scope -> St Scope
scopeShallowModifyAdverb private name val sc = if name `elem` specialNames then throwError $ DomainError "Cannot modify special variable" else case lookup name (scopeAdverbs sc) of
  Just _ -> scopeUpdateAdverb private name VariableNormal val sc
  Nothing -> throwError $ DomainError "Modifying a non-existent variable"

scopeShallowModifyConjunction :: Bool -> String -> Conjunction -> Scope -> St Scope
scopeShallowModifyConjunction private name val sc = if name `elem` specialNames then throwError $ DomainError "Cannot modify special variable" else case lookup name (scopeConjunctions sc) of
  Just _ -> scopeUpdateConjunction private name VariableNormal val sc
  Nothing -> throwError $ DomainError "Modifying a non-existent variable"

type Primitives = ([(String, Noun)], [(String, Function)], [(String, Adverb)], [(String, Conjunction)])

data Context = Context
  { contextScope :: IORef Scope
  , contextQuads :: Quads
  , contextIn :: St String
  , contextOut :: String -> St ()
  , contextErr :: String -> St ()
  , contextIncrementalId :: IORef Integer
  , contextDirectory :: FilePath
  , contextPrimitives :: Primitives
  , contextPrefix :: String }

instance NFData Context where
  rnf (Context s q i o e d r p pr) = rnf s `seq` rnf q `seq` rwhnf i `seq` rnf o `seq` rnf e `seq` rnf d `seq` rnf r `seq` rnf p `seq` rnf pr `seq`()

assignId :: St Integer
assignId = do
  idRef <- gets contextIncrementalId
  id <- readRef idRef
  writeRef idRef $ id + 1
  pure id

type St = StateT Context (ExceptT Error IO)

runSt :: St a -> Context -> ResultIO (a, Context)
runSt = runStateT

runWithContext :: Context -> St a -> St a
runWithContext ctx f = do
  r <- liftToSt $ runExceptT $ runSt f ctx
  case r of
    Left e -> throwError e
    Right (x, _) -> pure x

liftToSt :: IO a -> St a
liftToSt = liftIO

getContext :: St Context
getContext = get

getsContext :: (Context -> a) -> St a
getsContext = gets

putContext :: Context -> St ()
putContext = put

putScope :: IORef Scope -> St ()
putScope sc = do
  context <- get
  put $ context{ contextScope = sc }

createRef :: a -> St (IORef a)
createRef = liftToSt . IORef.newIORef

readRef :: IORef a -> St a
readRef = liftToSt . IORef.readIORef

writeRef :: IORef a -> a -> St ()
writeRef = liftToSt .: IORef.writeIORef

modifyRef :: IORef a -> (a -> a) -> St ()
modifyRef = liftToSt .: IORef.modifyIORef

instance MonadShow St ScalarValue where
  showM (Struct ctx) = do
    scope <- readRef $ contextScope ctx
    dShow <- scopeLookupNoun False (G.delta : "show") scope
    case dShow of
      Just dShow' -> asString (DomainError "Show must be a string") dShow'
      Nothing -> pure $ [fst G.struct] ++ "..." ++ [snd G.struct]
  showM (Box xs) = (G.enclose :) <$> showM xs
  showM (Wrap fn) = (\x -> [G.wrap, fst G.parens] ++ x ++ [snd G.parens]) <$> showM fn
  showM (AdverbWrap adv) = (\x -> [G.wrap, fst G.parens] ++ x ++ [snd G.parens]) <$> showM adv
  showM (ConjunctionWrap conj) = (\x -> [G.wrap, fst G.parens] ++ x ++ [snd G.parens]) <$> showM conj
  showM other = pure $ runIdentity $ showM other
