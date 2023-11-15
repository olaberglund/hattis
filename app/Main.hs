{-# LANGUAGE TupleSections #-}

module Main where

import Data.Bits
import Data.IntMap (IntMap, assocs, fromList, insert, (!))
import Data.List (foldl')

main :: IO ()
main = interact $ show . solve . map (map read . words) . lines

solve :: [[Int]] -> Int
solve ([n, _] : rest) = snd $ until ((== shiftL 1 n) . fst) (step constraints) (0, 0)
  where
    constraints = foldl' mkConstraint (empty n) rest

step :: IntMap Int -> (Int, Int) -> (Int, Int)
step m (pat, cnt) =
  if any (invalid pat) (assocs m)
    then (pat + 1, cnt)
    else (pat + 1, cnt + 1)
  where
    invalid pat (k, v)
      | pat `testBit` k = pat .&. v /= 0
      | otherwise = False

mkConstraint :: IntMap Int -> [Int] -> IntMap Int
mkConstraint m [a, b] =
  let a' = a - 1
      b' = b - 1
   in insert b' (m ! b' `setBit` a') (insert a' (m ! a' `setBit` b') m)

empty :: Int -> IntMap Int
empty max = fromList $ map (,0) [0 .. max - 1]
