module TinyAPL.Files where

import TinyAPL.Context
import TinyAPL.Error

import System.IO
import Control.Monad.Catch
import Control.Exception (IOException)
import Control.DeepSeq
import Control.Monad

makeUtf8 :: Handle -> IO Handle
makeUtf8 h = do
  hSetEncoding h utf8
  pure h

readUtf8 :: FilePath -> IO String
readUtf8 path = bracket (openFile path ReadMode >>= makeUtf8) hClose (hGetContents >=> (\x -> rnf x `seq` pure x))

read :: FilePath -> St String
read path = liftToSt (readUtf8 path) `catch` (\e -> throwError (IOError $ show (e :: IOException)))

writeUtf8 :: FilePath -> String -> IO ()
writeUtf8 path d = bracket (openFile path WriteMode >>= makeUtf8) hClose (flip hPutStr d >=> (\x -> rnf x `seq` pure x))

write :: FilePath -> String -> St ()
write path d = liftToSt (writeUtf8 path d) `catch` (\e -> throwError (IOError $ show (e :: IOException)))
