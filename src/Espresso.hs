{-# LANGUAGE LambdaCase #-}

module Espresso where

import Data.Char (digitToInt)
import Data.Foldable (foldl')

data Order = Espresso Int | Latte Int

main :: IO ()
main = interact (show . refills . lines)

refills :: [String] -> Int
refills (setup : orders) = fst $ foldl' serve (0, c) (map mkOrder orders)
  where
    c = capacity setup
    serve (rs, waterLevel) o
      | waterLevel - consumption o < 0 = (rs + 1, c - consumption o)
      | otherwise = (rs, waterLevel - consumption o)

capacity :: String -> Int
capacity = read . last . words

mkOrder :: String -> Order
mkOrder = \case
  [n] -> Espresso (digitToInt n)
  [n, 'L'] -> Latte (digitToInt n)

consumption :: Order -> Int
consumption = \case
  Espresso n -> n
  Latte n -> n + 1
