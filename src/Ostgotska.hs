module Ostgotska where

import Control.Arrow ((>>>))

main :: IO ()
main =
  interact $
    lines
      >>> head
      >>> words
      >>> aeFreq
      >>> \x ->
        if x >= 0.4
          then "dae ae ju traeligt va"
          else "haer talar vi rikssvenska"

aeFreq :: [String] -> Float
aeFreq ws = fromIntegral (length (filter containsAe ws)) / fromIntegral (length ws)
 where
  containsAe :: String -> Bool
  containsAe s = ('a', 'e') `elem` zip (init s) (tail s)
