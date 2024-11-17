module Soylent where

import Control.Arrow ((>>>))

main :: IO ()
main =
  interact $
    lines
      >>> tail
      >>> map (show . ceiling . flip (/) 400 . read)
      >>> unlines
