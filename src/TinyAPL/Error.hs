{-# LANGUAGE FlexibleContexts, DeriveGeneric #-}

module TinyAPL.Error
  ( module TinyAPL.Error
  , module Control.Monad.Except ) where
import GHC.Stack (HasCallStack)
import Control.Monad.Except (liftEither, throwError, tryError)
import qualified Control.Monad.Except as E
import Control.DeepSeq
import GHC.Generics
import Control.Monad.Catch (MonadCatch(..), try)
import Control.Exception (SomeException(..), evaluate)
import Control.Monad.IO.Class (MonadIO(..))

data Error
  = UserError String
  | DomainError String
  | LengthError String
  | RankError String
  | NYIError String
  | SyntaxError String
  | AssertionError String
  | IndexError String
  | IOError String
  | HaskellError String
  deriving (Eq, Ord, Generic)

instance NFData Error

unreachable = AssertionError "Unreachable code reached!"

errorCode :: Error -> Int
errorCode (UserError _) = 1
errorCode (DomainError _) = 2
errorCode (LengthError _) = 3
errorCode (RankError _) = 4
errorCode (NYIError _) = 5
errorCode (SyntaxError _) = 6
errorCode (AssertionError _) = 7
errorCode (IndexError _) = 8
errorCode (IOError _) = 9
errorCode (HaskellError _) = 10

errorMessage :: Error -> String
errorMessage (UserError e) = e
errorMessage (DomainError e) = e
errorMessage (LengthError e) = e
errorMessage (RankError e) = e
errorMessage (NYIError e) = e
errorMessage (SyntaxError e) = e
errorMessage (AssertionError e) = e
errorMessage (IndexError e) = e
errorMessage (IOError e) = e
errorMessage (HaskellError e) = e

fromErrorCode :: Int -> String -> Error
fromErrorCode 1 = UserError
fromErrorCode 2 = DomainError
fromErrorCode 3 = LengthError
fromErrorCode 4 = RankError
fromErrorCode 5 = NYIError
fromErrorCode 6 = SyntaxError
fromErrorCode 7 = AssertionError
fromErrorCode 8 = IndexError
fromErrorCode 9 = IOError
fromErrorCode 10 = HaskellError
fromErrorCode _ = error "fromErrorCode: unknown error type"

instance Show Error where
  show err = let
    (errorName, errorMessage) = case err of
      UserError msg -> ("User error", msg)
      DomainError msg -> ("Domain error", msg)
      LengthError msg -> ("Length error", msg)
      RankError msg -> ("Rank error", msg)
      NYIError msg -> ("Not yet implemented!", msg)
      SyntaxError msg -> ("Syntax error", msg)
      AssertionError msg -> ("Assertion failed", msg)
      IndexError msg -> ("Index error", msg)
      IOError msg -> ("IO error", msg)
      HaskellError msg -> ("Haskell error", msg)
    hasNewline = '\n' `elem` errorMessage
    in if hasNewline
      then errorName ++ '\n' : errorMessage
      else errorName ++ ": " ++ errorMessage

type Result = Either Error
type ResultIO = E.ExceptT Error IO

-- sadly we need this.
unerror :: HasCallStack => Result a -> a
unerror x = case x of
  Right x -> x
  Left  e -> error $ show e

except :: Monad m => Either e a -> E.ExceptT e m a
except m = E.ExceptT $ return m

runResult :: ResultIO a -> IO (Result a)
runResult = E.runExceptT

data CatchResult a
  = Thrown !Error
  | Panicked !SomeException
  | Succeeded !a

runAndCatch :: (E.MonadError Error m, MonadCatch m, MonadIO m, NFData a) => m a -> m (CatchResult a)
runAndCatch action = do
  r <- tryError action
  res <- try $ liftIO $ evaluate $ force r
  case res of
    Left ex -> pure $ Panicked ex
    Right (Left err) -> pure $ Thrown err
    Right (Right res) -> pure $ Succeeded res
