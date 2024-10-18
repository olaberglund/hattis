module Main where

import Control.Arrow ((>>>))

main :: IO ()
main =
  interact $
    lines >>> undefined
