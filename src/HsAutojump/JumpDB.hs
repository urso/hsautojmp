{-# LANGUAGE FlexibleContexts #-}

module HsAutojump.JumpDB where

import Control.Arrow (right)
import Data.ByteString as BS (ByteString,append)
import Data.Foldable (foldl')
import Data.Monoid
import qualified Data.Trie as T (Trie, size, member, empty, adjust, insert, 
                                 mapBy, toList, fromList, lookup)

import Data.ByteString.UTF8 (fromString)

import HsAutojump.Config
import HsAutojump.Utils

import Text.Regex.PCRE.Light.Extra

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

dbContains :: ByteString -> JumpDB -> Bool
dbContains e (JumpDB _ t) = T.member e t

dbLookup :: ByteString -> JumpDB -> Maybe Float
dbLookup e (JumpDB _ t) = T.lookup e t

dbFindBy :: (ByteString -> Float -> Bool) -> JumpDB -> [(ByteString,Float)]
dbFindBy f = filter (uncurry f) . dbToList

dbFindByPath :: (ByteString -> Bool) -> JumpDB -> [(ByteString,Float)]
dbFindByPath f = dbFindBy (const.f)

dbMatch :: (RegexLike (RegexType rl), RegexLike rl) =>
            rl -> JumpDB -> Either String [(ByteString, Float)]
dbMatch rl db = right findWithRegex $ compile rl
  where findWithRegex r = dbFindByPath (=~ r) db

dbMatch' :: (RegexLike (RegexType rl), RegexLike rl) =>
            rl -> JumpDB -> [(ByteString, Float)]
dbMatch' rl db = either error id $ dbMatch rl db

dbFindByWeight :: (Float -> Bool) -> JumpDB -> [(ByteString,Float)]
dbFindByWeight f = dbFindBy (\_ w -> f w)

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

