{-# LANGUAGE TupleSections #-}

module Main where

import           Control.Arrow ((>>>))

main :: IO ()
main =
    interact $
        lines
            >>> head
            >>> words
            >>> map read
            >>> ([],)
            >>> iterate bubbleStep
            >>> take 6
            >>> map (uncurry (<>))
            >>> map (unwords . map show)
            >>> unlines

-- bubbleStep :: ([Int], [Int]) -> ([Int], [Int])
bubbleStep :: ([Int], [Int]) -> ([Int], [Int])
bubbleStep (prev, x : y : rest)
    | x > y = (prev <> [y], x : rest)
    | otherwise = bubbleStep (prev <> [x], y : rest)
bubbleStep (prev, xs) = ([], prev <> xs)
