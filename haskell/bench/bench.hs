module Main where

import Criterion.Main
import qualified MapReduce as MR

main = defaultMain [
  bgroup "main"
    [
      bench "regex"  $ nfIO $ MR.runWith "regex"
    , bench "indice" $ nfIO $ MR.runWith "indice"
    ]
  ]
