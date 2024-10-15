module PizzaCrust where

import Control.Arrow ((>>>))

main :: IO ()
main =
  interact $
    lines
      >>> map (map read . words)
      >>> (\[[r, c]] -> (* 100) $ 1 - (circleArea r - circleArea (r - c)) / circleArea r)
      >>> show

circleArea :: Double -> Double
circleArea = (* pi) . (** 2)
