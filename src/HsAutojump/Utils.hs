{-# LANGUAGE FlexibleInstances #-}

module HsAutojump.Utils where

import Data.Monoid

import Data.ByteString as BS (ByteString, putStrLn)
import Data.ByteString.UTF8 (toString)
import System.Directory (doesDirectoryExist)
import Data.List (sortBy)

data Sorting = Asc | Des | None

sortedList ::  Sorting -> (a -> a -> Ordering) -> [a] -> [a]
sortedList Des f  = sortBy f
sortedList Asc f  = sortBy (invert f)
sortedList None _ = id

putLinesWith ::  (a -> ByteString) -> [a] -> IO ()
putLinesWith f = mapM_ (BS.putStrLn . f)

fromRight ::  b -> Either a b -> b
fromRight def (Left _) = def
fromRight _ (Right x)  = x

globToRegex ::  String -> String
globToRegex "" = ""
globToRegex ('*':xs) = ".*" ++ globToRegex xs
globToRegex ('?':xs) = "." ++ globToRegex xs
globToRegex (x:xs)   = escape x ++ globToRegex xs
  where
    special = "\\+()$.{}]|"
    escape c | c `elem` special = '\\' : [c]
             | otherwise        = [c]

instance Monoid (a -> a -> Ordering) where
  mempty = \ a b -> EQ
  cmpA `mappend` cmpB = \ x y -> case cmpA x y of
                                   EQ -> cmpB x y
                                   c  -> c

invert ::  (a -> a -> Ordering) -> a -> a -> Ordering
invert cmpr a b = case cmpr a b of
                    LT -> GT
                    GT -> LT
                    EQ -> EQ

isValidPath ::  ByteString -> IO Bool
isValidPath = doesDirectoryExist . toString

