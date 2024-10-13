module ReverseRot where

import Control.Arrow ((>>>))
import Data.Map (Map, (!))
import qualified Data.Map as Map

main :: IO ()
main =
  interact $
    lines
      >>> map (solve . words)
      >>> unlines

solve :: [String] -> String
solve [n, s] = map (rotate (read n) !) (reverse s)
solve _ = ""

rotate :: Int -> Map Char Char
rotate n =
  Map.fromList
    . zip alphabet
    . take (length alphabet)
    . drop n
    . cycle
    $ alphabet
 where
  alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ_."
