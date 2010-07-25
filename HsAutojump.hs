{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Applicative ((<$>))
import Control.Monad (filterM)
import Control.Exception (bracket)
import qualified Data.Trie as T (Trie, size, member, empty, adjust, insert, mapBy, toList, fromList)
import Data.Foldable (foldl')
import Data.Function (on)
import Data.List (sortBy, nub)
import Data.ByteString as BS (ByteString, putStrLn, append, empty, readFile)
import qualified Data.ByteString.Lazy as LBS (fromChunks)
import Data.Binary (Binary, encodeFile, decode, get, put)
import Data.Maybe (isJust)
import qualified Text.Regex.PCRE.Light as R (compileM, caseless, match, exec_no_utf8_check)
import System.Directory (doesFileExist, doesDirectoryExist, getHomeDirectory)
import System (getArgs)
import System.IO as SIO (putStrLn)
import Data.ByteString.UTF8 (fromString, toString)

getDBFile = (++ "/.hsautojmp.db") <$> getHomeDirectory
getConfigFile = (++ "/.hsautojmp.conf") <$> getHomeDirectory

data MatchType = MatchCaseSensitive 
               | MatchCaseInsensitive 
               | MatchCaseSensitiveThenInsensitive

data Sorting = Asc | Des | None

data Configuration = Configuration { maxSize :: Int
                                   , numRemove :: Int 
                                   , maxWeight :: Float
                                   , incWeight :: Float
                                   , matching :: MatchType
                                   , homeDirectory :: ByteString
                                   }

getDefaultConfig = Configuration 1000 
                                 100 
                                 1000
                                 1
                                 MatchCaseSensitiveThenInsensitive . 
                                 fromString <$> getHomeDirectory

data JumpDB = JumpDB { size :: !Int,
                       _map :: !(T.Trie Float) }
  deriving(Show)

instance Binary JumpDB where
  put (JumpDB _ map) = put map
  get = do m <- get
           return $ JumpDB (T.size m) m

main = do
  args   <- getArgs
  dbFile <- getDBFile
  db     <- loadDB dbFile
  cfg    <- getDefaultConfig
  case head args of
    "add"      -> saveDB dbFile $ cmdAdd (tail args) cfg db
    "stats"    -> putLinesWith showEntry $ cmdStats db
    "complete" -> cmdLstMatch (map globToRegex (tail args)) cfg db >>=
                  putLinesWith fst 
    "lstmatch" -> cmdLstMatch (map globToRegex (tail args)) cfg db >>=
                  putLinesWith showEntry 
    "match"    -> cmdMatch (map globToRegex (tail args)) cfg db >>= 
                  BS.putStrLn
    _          -> SIO.putStrLn "error: unknown command"

loadDB file = do b <- doesFileExist file
                 if b then decodeFile' file else return (JumpDB 0 T.empty)

saveDB file db = encodeFile file db

cmdAdd args cfg db = adjustSize cfg $ 
                     foldl' (flip (`addEntry` incWeight cfg)) db' $
                     filter (/= homeDirectory cfg) $ map fromString args
  where db' = graduallyForget (maxWeight cfg) db

cmdStats (JumpDB _ db)  = sortedList Asc $ T.toList db

cmdLstMatch [] cfg    = filterM isValidPath . sortedList Des . T.toList . _map
cmdLstMatch (x:_) cfg = filterM isValidPath . match (matching cfg) Des (fromString x) 

cmdMatch [] cfg (JumpDB _ db) = return BS.empty
cmdMatch (x:_) cfg db = do
    lst <- filterM isValidPath $ match (matching cfg) Des (fromString x) db
    return $ case lst of
               []    -> BS.empty
               (x:_) -> fst x

addEntry path inc (JumpDB size map) 
    |  T.member path map = JumpDB size $ T.adjust (+inc) path map
    |  otherwise         = JumpDB (size+1) $ T.insert path inc map

graduallyForget maxWeight (JumpDB size db) = JumpDB size $ mapValues forget db
  where
    !totalWeight = foldl' (+) 0.0 db

    forget w = 0.9 * w * maxWeight / totalWeight

adjustSize cfg jdb@(JumpDB size db)
    | size >= maxSize cfg = JumpDB newSize $ upd db
    | otherwise           = jdb
  where
    newSize = maxSize cfg - numRemove cfg
    upd = T.fromList . take newSize . sortedList Des . T.toList

match matching sortOpt path jdb@(JumpDB _ db) = 
  case matching of
    MatchCaseSensitive   -> sortedList sortOpt $ fromRight [] $ match' path True db
    MatchCaseInsensitive -> sortedList sortOpt $ fromRight [] $  match' path False db
    MatchCaseSensitiveThenInsensitive -> 
      nub (match MatchCaseSensitive sortOpt path jdb ++ 
           match MatchCaseInsensitive sortOpt path jdb)

match' path caseSensitive trie = 
    withRight filterWithRegex $ R.compileM path opts
  where
    opts | caseSensitive = [R.caseless]
         | otherwise     = []

    filterWithRegex regex = T.toList $ T.mapBy (matchRegex regex) trie

    matchRegex regex k x 
      | isJust (R.match regex k [R.exec_no_utf8_check]) = Just x
      | otherwise                                       = Nothing

isValidPath (path, _) = doesDirectoryExist $ toString path

sortedList Asc  = sortBy (compare `on`  snd) 
sortedList Des  = reverse . sortedList Asc
sortedList None = id

mapValues f m = T.mapBy fn m
  where fn _ = Just . f

showEntry (path, w) = fromString ("(" ++ show w ++ "): ") `append` path

decodeFile' path = do !v <- decode . LBS.fromChunks . return <$> BS.readFile path
                      return v

globToRegex "" = ""
globToRegex ('*':xs) = ".*" ++ globToRegex xs
globToRegex ('?':xs) = "." ++ globToRegex xs
globToRegex (x:xs)   = escape x ++ globToRegex xs
  where
    special = "\\+()$.{}]|"
    escape c | c `elem` special = '\\' : [c]
             | otherwise        = [c]

fromRight def (Left _) = def
fromRight _ (Right x)  = x

withRight f (Left x) = Left x
withRight f (Right x) = Right (f x)

putLinesWith f = mapM_ (BS.putStrLn . f)

