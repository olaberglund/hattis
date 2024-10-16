module Mult where

import           Control.Arrow ((>>>))
import           Data.List     (unfoldr)

main :: IO ()
main =
    interact $
        lines
            >>> tail
            >>> map read
            >>> unfoldr mult
            >>> map show
            >>> unlines

mult :: [Int] -> Maybe (Int, [Int])
mult (n : ns) = do
    (h : rest) <- Just $ dropWhile ((/= 0) . flip mod n) ns
    pure (h, rest)
