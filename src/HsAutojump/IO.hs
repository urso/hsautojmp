
module HsAutojump.IO (loadDB, saveDB) where

import Control.Applicative ((<$>) )
import Control.Concurrent (threadDelay)
import Data.Binary (Binary,encode,decode,put,get)
import qualified Data.ByteString as BS (ByteString,length)
import qualified Data.ByteString.Lazy as LBS (fromChunks, empty)
import qualified Data.ByteString.Lazy.Internal as LBS
import Data.ByteString.Unsafe (unsafePackCStringFinalizer,unsafeUseAsCString)
import GHC.IO.Device (SeekMode(AbsoluteSeek))
import System.Directory (doesFileExist)
import System.Posix.IO 
import System.Posix.Types (Fd)
import System.IO.Unsafe (unsafeInterleaveIO)
import Foreign (mallocBytes, free, castPtr)

import qualified Data.Trie as T (empty,size)
import System.IO.Cautious

import HsAutojump.JumpDB

instance Binary JumpDB where
  put (JumpDB _ map) = put map
  get = do m <- get
           return $ JumpDB (T.size m) m

loadDB :: FilePath -> IO JumpDB
loadDB file = safeDecodeFile (JumpDB 0 T.empty) file

saveDB :: FilePath -> JumpDB -> IO ()
saveDB file db = safeEncodeFile file db

safeDecodeFile :: Binary a => a -> FilePath -> IO a
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

safeEncodeFile :: Binary a => FilePath -> a -> IO ()
safeEncodeFile path = writeFileL path . encode
-- safeEncodeFile path value = do
--     fd <- openFd path WriteOnly (Just 0o600) (defaultFileFlags {trunc = False})
--     tryLockFile fd WriteLock
--     let cs = encode value
--     let outFn = LBS.foldrChunks (\\c rest -> writeChunk fd c >> rest) (return ()) cs
--     outFn
--     closeFd fd
--   where
--     writeChunk fd bs = unsafeUseAsCString bs $ \\ptr ->
--                          fdWriteBuf fd (castPtr ptr) (fromIntegral $ BS.length bs)

readBlock' :: Int -> Fd-> IO (Maybe BS.ByteString)
readBlock' size fd = do 
    buf <- mallocBytes size
    readSize <- fdReadBuf fd buf $ fromIntegral size
    if readSize == 0
      then do free buf 
              closeFd fd 
              return Nothing
      else Just <$> unsafePackCStringFinalizer buf
                        (fromIntegral readSize)
                        (free buf)

readBlock :: Fd -> IO (Maybe BS.ByteString)
readBlock = readBlock' 4096

fdGetContents ::  Fd -> IO LBS.ByteString
fdGetContents fd = lazyRead
  where 
    lazyRead = unsafeInterleaveIO loop

    loop = do blk <- readBlock fd
              case blk of
                Nothing -> return LBS.Empty
                Just c  -> LBS.Chunk c <$> lazyRead

tryLockFile :: Fd -> LockRequest -> IO ()
tryLockFile fd lock = waitToSetLock fd (lock, AbsoluteSeek, 0, 0) `catch` (\_ ->
  threadDelay 500 >> waitToSetLock fd (lock, AbsoluteSeek, 0, 0))

