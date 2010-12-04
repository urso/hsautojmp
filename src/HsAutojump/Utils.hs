module HsAutojump.Utils where

import Data.Monoid

import Data.ByteString as BS (ByteString, putStrLn)
import Data.ByteString.UTF8 (toString)
import System.Directory (doesDirectoryExist)
import Data.List (sortBy)

putLinesWith ::  (a -> ByteString) -> [a] -> IO ()
putLinesWith f = mapM_ (BS.putStrLn . f)

globToRegex ::  String -> String
globToRegex "" = ""
globToRegex ('*':xs) = ".*" ++ globToRegex xs
globToRegex ('?':xs) = "." ++ globToRegex xs
globToRegex (x:xs)   = escape x ++ globToRegex xs
  where
    special = "\\+()$.{}]|"
    escape c | c `elem` special = '\\' : [c]
             | otherwise        = [c]

invert ::  (a -> a -> Ordering) -> a -> a -> Ordering
invert cmpr a b = case cmpr a b of
                    LT -> GT
                    GT -> LT
                    EQ -> EQ

isValidPath ::  ByteString -> IO Bool
isValidPath = doesDirectoryExist . toString

