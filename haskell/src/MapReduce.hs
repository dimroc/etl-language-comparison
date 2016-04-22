{-# LANGUAGE OverloadedStrings #-}

module MapReduce where

import Matcher

import Data.Foldable (traverse_)
import System.FilePath.Glob (compile, globDir1)

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified ListT
import qualified STMContainers.Map as Map

intToBs = T.encodeUtf8 . T.pack . show

glue r (k, v) =
    return (ByteString.concat
            [ r
            , ByteString.Lazy.Char8.toStrict k
            , "\t"
            , intToBs v
            , "\n"
            ])

getMatcher :: String -> (ByteString.Lazy.ByteString -> Bool)
getMatcher args =
    case args of
      "regex" -> regexMatch
      "indice" -> indiceMatch
      _ -> indiceMatch

runWith :: String -> IO ()
runWith matcherArg = do
    files <- globDir1 (compile "tweets_*") "../tmp/tweets"

    m <- Map.newIO

    let matcher = getMatcher matcherArg
    let increment key = STM.atomically (do
            x <- Map.lookup key m
            case x of
                Nothing -> Map.insert 1 key m
                Just n  -> n' `seq` Map.insert n' key m  where n' = n + 1 )

    let processFile file = Async.Concurrently (do
            bytes <- ByteString.Lazy.readFile file
            traverse_ increment (parseFile matcher bytes) )

    Async.runConcurrently (traverse_ processFile files)

    fileContents <- STM.atomically (ListT.fold glue "" (Map.stream m))
    ByteString.writeFile ("../tmp/haskell_" ++ matcherArg ++  "_results.txt") fileContents
