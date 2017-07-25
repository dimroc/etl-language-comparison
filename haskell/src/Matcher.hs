{-# LANGUAGE OverloadedStrings #-}
module Matcher where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import qualified Data.ByteString.Search as Search
import qualified Text.Regex.TDFA.ByteString as Regex
import Text.Regex.TDFA.Common
import Data.Maybe (isJust)

parseFile
    :: (ByteString.Lazy.ByteString -> Bool)
    -> ByteString.Lazy.ByteString
    -> [ByteString.Lazy.ByteString]
parseFile matchFunction bytes0 =
    [ neighborhood line
    | line <- ByteString.Lazy.Char8.lines bytes0
    , matchFunction line
    ]
  where
    neighborhood =
          ByteString.Lazy.takeWhile (/= 9)
        . ByteString.Lazy.drop 1
        . ByteString.Lazy.dropWhile (/= 9)

regexPattern :: ByteString.ByteString -> Regex.Regex
regexPattern bs = case Regex.compile comp ex bs of
                    Left err -> error (show err)
                    Right reg -> reg
  where
    comp = CompOption { caseSensitive = False
                      , multiline = False
                      , rightAssoc = True
                      , newSyntax = False
                      , lastStarGreedy = False
                      } --defaultCompOpt { caseSensitive = False }
    ex = ExecOption { captureGroups = False}

regexMatch :: ByteString.Lazy.ByteString -> Bool
regexMatch bs =
    case regexMatch' bs of
      Left _ -> False
      Right m -> isJust m

regexMatch' =
      Regex.execute (regexPattern "knicks")
    . ByteString.Lazy.toStrict

indiceMatch :: ByteString.Lazy.ByteString -> Bool
indiceMatch =
      not
    . null
    . Search.indices "knicks"
    . ByteString.map lower
    . ByteString.Lazy.toStrict

  where
    lower w8 = if 65 <= w8 && w8 < 91 then w8 + 32 else w8
