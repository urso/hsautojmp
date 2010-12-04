
module Main where

import Control.Applicative ((<$>))
import Control.Monad (filterM)
import Data.Foldable (foldl')
import Data.List (sortBy)
import Data.ByteString as BS (putStrLn, empty)
import System.Directory (getHomeDirectory)
import System (getArgs)
import System.IO as SIO (putStrLn)
import Data.ByteString.UTF8 (fromString, toString)

import HsAutojump.Config
import HsAutojump.IO
import HsAutojump.JumpDB
import HsAutojump.Utils

getDBFile = (++ "/.hsautojmp.db") <$> getHomeDirectory
-- getConfigFile = (++ "/.hsautojmp.conf") <$> getHomeDirectory

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

cmdAdd args cfg db = adjustSize (maxSize cfg) (numRemove cfg) $ 
                     foldl' (flip (`addEntry` incWeight cfg)) db' $
                     filter (/= homeDirectory cfg) $ map fromString args
  where db' = graduallyForget (maxWeight cfg) db

cmdStats cfg = sortBy (lowestScore `andThen` longestPath) . dbToList

cmdLstMatch [] cfg = filterM (isValidPath.fst) . sortBy highestScore . dbToList
cmdLstMatch (x:_) cfg = filterM (isValidPath.fst) . 
                          dbApplyStrategie (strategie cfg) (fromString x)

cmdMatch [] _ _ = return BS.empty
cmdMatch (x:_) cfg db = do
  lst <- filterM (isValidPath.fst) $ dbApplyStrategie (strategie cfg) (fromString x) db
  return $ case lst of
             [] -> BS.empty
             (x:_) -> fst x

