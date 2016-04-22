module Main where

import qualified MapReduce
import System.Environment

-- will run the indice matcher if given no arguments
main = do
    [matcher] <- getArgs
    MapReduce.runWith matcher
