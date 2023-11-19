{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Array.IO (IOUArray)
import Data.Array.MArray
import Data.Foldable (traverse_)
import Data.Text hiding (head, map, tail)
import Data.Text.IO
import Data.Text.Read
import Prelude hiding (getContents, lines, read, words)

main :: IO ()
main = do
  wordRows <- map words . lines <$> getContents
  uf <- newUnionFind (decimalU (head (head wordRows)) + 1)
  traverse_ (act uf . mkQuery) (tail wordRows)

data Query = Merge !Int !Int | Size !Int deriving (Show)

mkQuery :: [Text] -> Query
mkQuery ["t", a, b] = Merge (decimalU a) (decimalU b)
mkQuery ["s", a] = Size (decimalU a)

decimalU :: Text -> Int
decimalU = fst . either error id . decimal

act :: UnionFind -> Query -> IO ()
act uf (Merge a b) = unite uf a b
act uf (Size a) = do
  i <- root uf a
  sz <- readArray (szs uf) i
  print sz

data UnionFind = UnionFind
  { ids :: IOUArray Int Int,
    szs :: IOUArray Int Int
  }

newUnionFind :: Int -> IO UnionFind
newUnionFind n = UnionFind <$> newListArray (0, n - 1) [0 .. n - 1] <*> newArray (0, n - 1) 1

root :: UnionFind -> Int -> IO Int
root uf i = do
  id <- readArray (ids uf) i
  if id /= i
    then do
      gpid <- readArray (ids uf) id
      writeArray (ids uf) i gpid
      root uf id
    else return i

unite :: UnionFind -> Int -> Int -> IO ()
unite uf p q = do
  i <- root uf p
  j <- root uf q
  szi <- readArray (szs uf) i
  szj <- readArray (szs uf) j
  if i == j
    then return ()
    else
      if szi < szj
        then do
          writeArray (ids uf) i j
          writeArray (szs uf) j (szi + szj)
        else do
          writeArray (ids uf) j i
          writeArray (szs uf) i (szj + szi)
