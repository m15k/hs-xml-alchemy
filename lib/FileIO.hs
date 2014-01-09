{-# LANGUAGE OverloadedStrings #-}
module FileIO where

import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams
import System.IO
import Data.Monoid ((<>))

useFile :: FilePath -> FilePath -> a -> IO a
useFile file1 file2 useFn =
    withFile file1 ReadMode  $ \h1 ->
    withFile file2 WriteMode $ \h2 -> do
    is <- Streams.handleToInputStream h1
    os <- Streams.handleToOutputStream h2
    Streams.connect is os
