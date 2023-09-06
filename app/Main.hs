{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Char (digitToInt)
import Data.Maybe (mapMaybe)

-- espresso: x water
-- latte: x + 1 water
-- water tank: s capacity
--
-- first line: n of students
-- second line: s
--
data Order = Espresso Int | Latte Int

main :: IO ()
main = interact (show . refills . lines)

refills :: [String] -> Int
refills strs =
  let s = capacity (head strs)
   in sum (map consumption (mapMaybe mkOrder strs)) `div` s

capacity :: String -> Int
capacity = read . last . words

mkOrder :: String -> Maybe Order
mkOrder = \case
  [n] -> Just $ Espresso (digitToInt n)
  [n, 'L'] -> Just $ Latte (digitToInt n)
  _ -> Nothing

consumption :: Order -> Int
consumption = \case
  Espresso n -> n
  Latte n -> n + 1
