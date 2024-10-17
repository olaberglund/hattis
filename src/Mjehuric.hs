{-# LANGUAGE TupleSections #-}

module Mjehuric where

import Control.Arrow ((>>>))
import Data.List (sort)

main :: IO ()
main =
  interact $
    lines
      >>> head
      >>> words
      >>> map read
      >>> ([],)
      >>> iterate bubbleStep
      >>> takeWhile (not . null . snd)
      >>> tail
      >>> map (unwords . map show . unpair)
      >>> unlines

bubbleStep :: ([Int], [Int]) -> ([Int], [Int])
bubbleStep xs | sort (unpair xs) == unpair xs = (unpair xs, [])
bubbleStep (prev, x : y : rest)
  | x > y = (prev <> [y], x : rest)
  | otherwise = bubbleStep (prev <> [x], y : rest)
bubbleStep (prev, xs) = bubbleStep ([], prev <> xs)

unpair :: ([a], [a]) -> [a]
unpair = uncurry (<>)
