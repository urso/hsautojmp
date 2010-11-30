
module HsAutojump.JumpDB where

import Data.ByteString as BS (ByteString,append)
import Data.Foldable (foldl')
import qualified Data.Trie as T (Trie, size, member, empty, adjust, insert, mapBy, toList, fromList)

import Data.ByteString.UTF8 (fromString)

import HsAutojump.Config
import HsAutojump.Utils

data JumpDB = JumpDB { size :: !Int,
                       dbData :: !(T.Trie Float) }
  deriving(Show)

emptyDB :: JumpDB
emptyDB = (JumpDB 0 T.empty)

dbFromTrie :: T.Trie Float -> JumpDB
dbFromTrie t = JumpDB (T.size t) t

dbToTrie :: JumpDB -> T.Trie Float
dbToTrie (JumpDB _ t) = t

dbFromList :: [(ByteString,Float)] -> JumpDB
dbFromList = dbFromTrie . T.fromList

dbToList :: JumpDB -> [(ByteString, Float)]
dbToList = T.toList . dbToTrie

addEntry :: BS.ByteString -> Float -> JumpDB -> JumpDB
addEntry path inc (JumpDB size map) 
    |  T.member path map = JumpDB size $ T.adjust (+inc) path map
    |  otherwise         = JumpDB (size+1) $ T.insert path inc map

graduallyForget :: Float -> JumpDB -> JumpDB
graduallyForget maxWeight (JumpDB size db) = JumpDB size $ mapValues forget db
  where
    !totalWeight = foldl' (+) 0.0 db

    forget w = 0.9 * w * maxWeight / totalWeight

mapValues :: (a -> a) -> T.Trie a -> T.Trie a
mapValues = T.mapBy . const . (Just.)

adjustSize :: Int -> Int -> ScoringTest -> JumpDB -> JumpDB
adjustSize maxSize numRemove matching jdb@(JumpDB size db)
    | size >= maxSize = JumpDB newSize $ upd db
    | otherwise       = jdb
  where
    newSize = maxSize - numRemove 
    upd = T.fromList . take newSize . sortedList Des matching . T.toList

showEntry :: (BS.ByteString, Float) -> BS.ByteString
showEntry (path, w) = fromString ("(" ++ show w ++ "): ") `BS.append` path

