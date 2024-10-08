{-# LANGUAGE ScopedTypeVariables #-}

module Kutevi where

import           Control.Arrow ((>>>))
import           Data.List     (foldl')

main :: IO ()
main =
    interact $
        lines
            >>> tail
            >>> map (map read . words)
            >>> (\[possible, selected] -> solve possible selected)
            >>> unwords

solve :: [Int] -> [Int] -> [String]
solve possible selected =
    let gcd' = foldl' gcd 360 possible
     in map
            ( \x ->
                if x `mod` gcd' == 0
                    then "YES"
                    else "NO"
            )
            selected
