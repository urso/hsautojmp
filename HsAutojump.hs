{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Applicative ((<$>) )
import Control.Arrow (right)
import Control.Monad (filterM)
import Control.Exception (bracket)
import qualified Data.Trie as T (Trie, size, member, empty, adjust, insert, mapBy, toList, fromList)
import Data.Foldable (foldl')
import Data.Function (on)
import Data.List (sortBy, nub)
import Data.ByteString as BS (ByteString, putStrLn, append, empty, readFile, length)
import qualified Data.ByteString.Lazy as LBS (fromChunks, empty)
import qualified Data.ByteString.Lazy.Internal as LBS
import Data.Binary (Binary, encodeFile, decode, encode, get, put)
import Data.Maybe (isJust)
import qualified Text.Regex.PCRE.Light as R (compileM, caseless, match, exec_no_utf8_check)
import System.Directory (doesFileExist, doesDirectoryExist, getHomeDirectory)
import System (getArgs)
import System.IO as SIO (putStrLn, withBinaryFile, IOMode(ReadMode))
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Posix.IO
import Data.ByteString.UTF8 (fromString, toString)
import Data.ByteString.Unsafe (unsafePackCStringFinalizer, unsafeUseAsCString)
import Foreign (mallocBytes, free, castPtr)
import GHC.IO.Device (SeekMode(AbsoluteSeek))
import Data.Monoid

instance Monoid (a -> a -> Ordering) where
  mempty = \ a b -> EQ
  cmpA `mappend` cmpB = \ x y -> case cmpA x y of
                                   EQ -> cmpB x y
                                   c  -> c

getDBFile = (++ "/.hsautojmp.db") <$> getHomeDirectory
getConfigFile = (++ "/.hsautojmp.conf") <$> getHomeDirectory

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

data Sorting = Asc | Des | None

type MatchType = (MatchCaseSensitivity, ScoringTest)

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
                                 (MatchCaseSensitiveThenInsensitive, 
                                  sorting2CmpFn undefined MatchHighestScore) . 
                                 fromString <$> getHomeDirectory

data JumpDB = JumpDB { size :: !Int,
                       dbData :: !(T.Trie Float) }
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
    "stats"    -> putLinesWith showEntry $ cmdStats cfg db
    "complete" -> cmdLstMatch (map globToRegex (tail args)) cfg db >>=
                  putLinesWith fst 
    "lstmatch" -> cmdLstMatch (map globToRegex (tail args)) cfg db >>=
                  putLinesWith showEntry 
    "match"    -> cmdMatch (map globToRegex (tail args)) cfg db >>= 
                  BS.putStrLn
    _          -> SIO.putStrLn "error: unknown command"

loadDB file = safeDecodeFile (JumpDB 0 T.empty) file

saveDB file db = safeEncodeFile file db

cmdAdd args cfg db = adjustSize cfg $ 
                     foldl' (flip (`addEntry` incWeight cfg)) db' $
                     filter (/= homeDirectory cfg) $ map fromString args
  where db' = graduallyForget (maxWeight cfg) db

cmdStats cfg (JumpDB _ db)  = sortedList Asc (snd $ matching cfg) $ T.toList db

cmdLstMatch [] cfg    = filterM isValidPath . sortedList Des (snd $ matching cfg) . T.toList . dbData
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
    upd = T.fromList . take newSize . sortedList Des (snd $ matching cfg) . T.toList

match (matching,matchSorting) sortOpt path jdb@(JumpDB _ db) = 
  case matching of
    MatchCaseSensitive   -> sortedList sortOpt matchSorting $ fromRight [] $ match' path True db
    MatchCaseInsensitive -> sortedList sortOpt matchSorting $ fromRight [] $  match' path False db
    MatchCaseSensitiveThenInsensitive -> 
      nub (match (MatchCaseSensitive,matchSorting) sortOpt path jdb ++ 
           match (MatchCaseInsensitive,matchSorting) sortOpt path jdb)

match' path caseSensitive trie = 
    right filterWithRegex $ R.compileM path opts
  where
    opts | caseSensitive = [R.caseless]
         | otherwise     = []

    filterWithRegex regex = T.toList $ T.mapBy (matchRegex regex) trie

    matchRegex regex k x 
      | isJust (R.match regex k [R.exec_no_utf8_check]) = Just x
      | otherwise                                       = Nothing

isValidPath (path, _) = doesDirectoryExist $ toString path

sortedList Des f  = sortBy f
sortedList Asc f  = sortBy (invert f)
sortedList None _ = id

invert cmpr a b = case cmpr a b of
                    LT -> GT
                    GT -> LT
                    EQ -> EQ

mapValues f m = T.mapBy fn m
  where fn _ = Just . f

showEntry (path, w) = fromString ("(" ++ show w ++ "): ") `BS.append` path

tryLockFile fd lock = waitToSetLock fd (lock, AbsoluteSeek, 0, 0)

safeEncodeFile path value = do
    fd <- openFd path WriteOnly (Just 0o600) (defaultFileFlags {trunc = False})
    tryLockFile fd WriteLock
    let cs = encode value
    let outFn = LBS.foldrChunks (\c rest -> writeChunk fd c >> rest) (return ()) cs
    outFn
    closeFd fd
  where
    writeChunk fd bs = unsafeUseAsCString bs $ \ptr ->
                         fdWriteBuf fd (castPtr ptr) (fromIntegral $ BS.length bs)

safeDecodeFile def path = do 
    e <- doesFileExist path
    if e 
      then do fd <- openFd path ReadOnly Nothing 
                           (defaultFileFlags{nonBlock=True})
              tryLockFile fd ReadLock
              c  <- fdGetContents fd
              let !v = decode $! c
              return v
      else return def

fdGetContents fd = lazyRead
  where 
    lazyRead = unsafeInterleaveIO loop

    loop = do blk <- readBlock fd
              case blk of
                Nothing -> return LBS.Empty
                Just c  -> LBS.Chunk c <$> lazyRead

readBlock fd = do buf <- mallocBytes 4096
                  readSize <- fdReadBuf fd buf 4096
                  if readSize == 0
                    then do free buf 
                            closeFd fd 
                            return Nothing
                    else Just <$> unsafePackCStringFinalizer buf
                                         (fromIntegral readSize)
                                         (free buf)

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

putLinesWith f = mapM_ (BS.putStrLn . f)

