{-# LANGUAGE CPP #-}

#ifndef wasm32_HOST_ARCH
{-# LANGUAGE TemplateHaskell #-}
#endif

module TinyAPL.StandardLibrary.Internal where

import System.FilePath
import System.Directory
import Control.Monad
import TinyAPL.Util
import Data.List
import Data.Bifunctor
#ifndef wasm32_HOST_ARCH
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
#endif

{-
The following license covers this documentation, and the source code, except
where otherwise indicated.

Copyright 2008, Michael Snoyman. All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS "AS IS" AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

fileList :: FilePath -> IO [(FilePath, String)]
fileList top = go top "" where
  notHidden :: FilePath -> Bool
  notHidden path = not $ "." `isPrefixOf` path

  go :: FilePath -> FilePath -> IO [(FilePath, String)]
  go realTop top = do
    allContents <- filter notHidden <$> getDirectoryContents (realTop </> top)
    let all' = map (liftA2 (,) (top </>) (\x -> realTop </> top </> x)) allContents
    files <- filterM (doesFileExist . snd) all' >>= mapM (secondM readFile)
    dirs <- filterM (doesDirectoryExist . snd) all' >>= mapM (go realTop . fst)
    pure $ sortOn fst $ concat $ files : dirs

listToStd :: [(FilePath, String)] -> [([String], String)]
listToStd = map (first $ fmap dropTrailingPathSeparator . splitPath . dropExtension)

#ifndef wasm32_HOST_ARCH
strToExp :: String -> Q Exp
strToExp s = pure $ LitE $ StringL s

pairToExp :: String -> (FilePath, String) -> Q Exp
pairToExp _root (path, bs) = do
  qAddDependentFile $ _root ++ '/' : path
  exp' <- strToExp bs
  return $! TupE $ map Just [LitE $ StringL path, exp']
#endif