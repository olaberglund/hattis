module Julemost where

import System.IO

main :: IO ()
main = getLine >>= mainWithDay 1 0 . read

mainWithDay :: Int -> Int -> Int -> IO ()
mainWithDay day guess maxBottles = do
  print (day * guess)
  hFlush stdout
  res <- getLine
  case res of
    "less" -> mainWithDay (day + 1) (guess - 1) maxBottles
    "more" -> mainWithDay (day + 1) (guess + 1) maxBottles
    "exact" -> return ()
    _ -> mainWithDay day guess maxBottles
