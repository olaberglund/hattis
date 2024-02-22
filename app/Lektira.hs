module Lektira where

main :: IO ()
main = interact (minimum . map (concatMap reverse) . splits . head . lines)

splits :: String -> [[String]]
splits s = do
  i1 <- [1 .. length s - 2]
  let (s1, r1) = splitAt i1 s
  i2 <- [1 .. length r1 - 1]
  let (s2, r2) = splitAt i2 r1
  return [s1, s2, r2]
