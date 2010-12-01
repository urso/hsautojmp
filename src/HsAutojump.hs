{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Applicative ((<$>))
import Control.Arrow (right)
import Control.Monad (filterM)
import qualified Data.Trie as T (Trie, mapBy, toList)
import Data.Foldable (foldl')
import Data.List (nub)
import Data.ByteString as BS (ByteString, putStrLn, empty)
import Data.Maybe (isJust)
import qualified Text.Regex.PCRE.Light as R (compileM, caseless, match, exec_no_utf8_check)
import System.Directory (getHomeDirectory)
import System (getArgs)
import System.IO as SIO (putStrLn)
import Data.ByteString.UTF8 (fromString, toString)

import HsAutojump.Config
import HsAutojump.IO
import HsAutojump.JumpDB
import HsAutojump.Utils

getDBFile = (++ "/.hsautojmp.db") <$> getHomeDirectory
getConfigFile = (++ "/.hsautojmp.conf") <$> getHomeDirectory

main = do
  args   <- getArgs
  dbFile <- getDBFile
  db     <- loadDB dbFile
  cfg    <- getDefaultConfig
  case head args of
    "add"      -> saveDB dbFile $ cmdAdd (tail args) cfg db
    "stats"    -> putLinesWith showEntry $ cmdStats cfg db
    "complete" -> cmdLstMatch (map globToRegex (tail args)) cfg db >>=
                  putLinesWith fst 
    "lstmatch" -> cmdLstMatch (map globToRegex (tail args)) cfg db >>=
                  putLinesWith showEntry 
    "match"    -> cmdMatch (map globToRegex (tail args)) cfg db >>= 
                  BS.putStrLn
    _          -> SIO.putStrLn "error: unknown command"

cmdAdd args cfg db = adjustSize (maxSize cfg) (numRemove cfg) (snd $ matching cfg) $ 
                     foldl' (flip (`addEntry` incWeight cfg)) db' $
                     filter (/= homeDirectory cfg) $ map fromString args
  where db' = graduallyForget (maxWeight cfg) db

cmdStats cfg = sortedList Asc (snd $ matching cfg) . dbToList

cmdLstMatch [] cfg    = filterM (isValidPath.fst) . sortedList Des (snd $ matching cfg) . dbToList
cmdLstMatch (x:_) cfg = filterM (isValidPath.fst) . match (matching cfg) Des (fromString x) 

cmdMatch [] cfg (JumpDB _ db) = return BS.empty
cmdMatch (x:_) cfg db = do
    lst <- filterM (isValidPath.fst) $ match (matching cfg) Des (fromString x) db
    return $ case lst of
               []    -> BS.empty
               (x:_) -> fst x

match (matching,matchSorting) sortOpt path db = 
  case matching of
    MatchCaseSensitive   -> sortedList sortOpt matchSorting $ fromRight [] $ match' path True db
    MatchCaseInsensitive -> sortedList sortOpt matchSorting $ fromRight [] $  match' path False db
    MatchCaseSensitiveThenInsensitive -> 
      nub (match (MatchCaseSensitive,matchSorting) sortOpt path db ++ 
           match (MatchCaseInsensitive,matchSorting) sortOpt path db)

match' :: ByteString -> Bool -> JumpDB -> Either String [(ByteString, Float)]
match' path caseSensitive db = right findWithRegex $ R.compileM path opts
  where
    opts | caseSensitive = [R.caseless]
         | otherwise     = []

    findWithRegex regex   = dbFindByPath (matchRegex regex) db
    matchRegex regex path = isJust (R.match regex path [R.exec_no_utf8_check])

