
{-# LANGUAGE BangPatterns #-}

module HsAutojump.IO (loadDB, saveDB) where

import Data.Binary (Binary,encode,put,get,decodeFile,encodeFile)
import System.Directory (doesFileExist)

import System.IO.Cautious

import HsAutojump.JumpDB

instance Binary JumpDB where
  put (JumpDB _ map) = put map
  get = fmap dbFromTrie get

loadDB :: FilePath -> IO JumpDB
loadDB file = decodeFileWithDefault emptyDB file

saveDB :: FilePath -> JumpDB -> IO ()
saveDB file db = do
  e <- doesFileExist file
  if not e
    then encodeFile file db
    else writeFileL file $ encode db

decodeFileWithDefault :: Binary a => a -> FilePath -> IO a
decodeFileWithDefault def path = do
    e <- doesFileExist path
    if not e
      then return def
      else do !c <- decodeFile path
              return c

