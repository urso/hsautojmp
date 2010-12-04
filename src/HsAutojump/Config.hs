
module HsAutojump.Config where

import Control.Applicative ((<$>) )
import Data.Function (on)
import Data.ByteString as BS (ByteString,length)
import Data.List (sortBy)
import Data.Monoid
import System.Directory (getHomeDirectory)
import Data.ByteString.UTF8 (fromString)

import HsAutojump.JumpDB
import HsAutojump.Utils

data Configuration = Configuration { maxSize :: Int
                                   , numRemove :: Int
                                   , maxWeight :: Float
                                   , incWeight :: Float
                                   , strategie :: Strategie
                                   , homeDirectory :: ByteString
                                   }

highestScore,lowestScore :: ScoringTest
lowestScore  = compare `on` snd
highestScore = invert lowestScore

shortestPath,longestPath :: ScoringTest
shortestPath  = compare `on` (BS.length . fst)
longestPath   = invert shortestPath

sa `andThen` sb = \x y -> case sa x y of
                            EQ ->  sb x y
                            r  ->  r

defaultStrategie = mconcat [sExact, 
                            sMatch (sortBy highestScore) False, 
                            sMatch (sortBy highestScore) True]

getDefaultConfig = Configuration 1000 100 1000 1 defaultStrategie . 
                    fromString <$> getHomeDirectory
