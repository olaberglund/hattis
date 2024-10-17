{-# LANGUAGE TupleSections #-}

module Main where

import Control.Arrow ((>>>))
import Data.List (sort, unfoldr)

main :: IO ()
main =
  interact $
    lines
      >>> head
      >>> words
      >>> map read
      >>> goran
      >>> filter (not . null)
      >>> map (unwords . map show)
      >>> unlines

goran :: [Int] -> [[Int]]
goran = unfoldr step . ([],)
 where
  step :: ([Int], [Int]) -> Maybe ([Int], ([Int], [Int]))
  step xs = do
    (prev, x : y : rest) <- Just xs
    [l, g] <- Just $ sort [x, y]
    pure
      ( if [l, g] == [x, y]
          then []
          else prev <> [l, g] <> rest
      , (prev <> [l], g : rest)
      )
