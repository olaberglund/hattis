module PervasiveHeartMonitor where

import Control.Arrow ((>>>))
import Data.Char (isAlpha)
import Data.List (partition)

main :: IO ()
main =
  interact $
    lines
      >>> map
        (processLine >>> (\(v, n) -> show v <> " " <> n))
      >>> unlines

processLine :: String -> (Double, String)
processLine line =
  let tokens = words line
      (xs, ys) = partition (isAlpha . head) tokens
   in (average $ map read ys, unwords xs)

average :: [Double] -> Double
average xs = sum xs / fromIntegral (length xs)
