{-# LANGUAGE CPP #-}

#ifndef wasm32_HOST_ARCH
{-# LANGUAGE TemplateHaskell #-}
#endif

module TinyAPL.StandardLibrary where

import TinyAPL.StandardLibrary.Internal

#ifdef wasm32_HOST_ARCH
import System.IO.Unsafe
import Control.Exception (evaluate)
import Control.DeepSeq (NFData(rnf))
import Control.Monad
#else
import Language.Haskell.TH
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

#ifdef wasm32_HOST_ARCH
standardLibrary :: [([String], String)]
standardLibrary = unsafePerformIO $ listToStd <$> fileList "/std"
{-# NOINLINE standardLibrary #-}

foreign export ccall loadStandardLibrary :: IO ()

loadStandardLibrary :: IO ()
loadStandardLibrary = void $ evaluate $ rnf standardLibrary
#else
standardLibrary :: [([String], String)]
standardLibrary = listToStd $ $(ListE <$> ((runIO $ fileList "std") >>= mapM (pairToExp "std")))
#endif
