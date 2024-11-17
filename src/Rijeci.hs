module Rijeci where

import Control.Arrow ((>>>))

main :: IO ()
main =
  interact $
    read
      >>> (iterate press [1, 0] !!)
      >>> map show
      >>> unwords

press :: [Int] -> [Int]
press [as, bs] = [bs, bs + as]
