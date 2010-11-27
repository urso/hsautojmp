
module HsAutojump.Config where

import Control.Applicative ((<$>) )
import Data.Function (on)
import Data.ByteString as BS (ByteString,length)
import System.Directory (getHomeDirectory)
import Data.ByteString.UTF8 (fromString)

import HsAutojump.Utils

data MatchCaseSensitivity = MatchCaseSensitive 
                          | MatchCaseInsensitive 
                          | MatchCaseSensitiveThenInsensitive

data MatchSorting = MatchShortestFirst
                  | MatchLongestFirst
                  | MatchHighestScore
                  | MatchLowestScore

type ScoringTest = (ByteString, Float) -> (ByteString, Float) -> Ordering

sorting2CmpFn :: ByteString -> MatchSorting -> ScoringTest
sorting2CmpFn _ MatchShortestFirst = compare `on` (BS.length . fst)
sorting2CmpFn _ MatchLongestFirst  = invert (compare `on` (BS.length . fst))
sorting2CmpFn _ MatchLowestScore   = compare `on` snd
sorting2CmpFn _ MatchHighestScore  = invert (compare `on` snd)

type MatchType = (MatchCaseSensitivity, ScoringTest)

data Configuration = Configuration { maxSize :: Int
                                   , numRemove :: Int 
                                   , maxWeight :: Float
                                   , incWeight :: Float
                                   , matching :: MatchType
                                   , homeDirectory :: ByteString
                                   }

getDefaultConfig :: IO Configuration
getDefaultConfig = Configuration 1000 
                                 100 
                                 1000
                                 1
                                 (MatchCaseSensitiveThenInsensitive, 
                                  sorting2CmpFn undefined MatchHighestScore) . 
                                 fromString <$> getHomeDirectory

