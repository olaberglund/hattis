module SecretMessage where

import Control.Arrow ((>>>))
import Data.Foldable (toList)
import Data.List (transpose)
import Data.Sequence (chunksOf, fromList, reverse)
import Prelude hiding (reverse)

main :: IO ()
main =
  interact $
    lines
      >>> tail
      >>> map
        ( \w ->
            let n = ceiling . sqrt . fromIntegral . length $ w
             in concatMap tail
                  . transpose
                  . toList
                  . fmap toList
                  . reverse
                  . chunksOf n
                  . fromList
                  $ (w <> replicate n '*')
        )
      >>> unlines
