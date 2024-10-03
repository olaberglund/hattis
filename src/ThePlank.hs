module ThePlank where

import           Control.Arrow ((>>>))
import           Data.Tree     (Tree)
import qualified Data.Tree     as Tree

main :: IO ()
main =
    interact $
        lines
            >>> head
            >>> read
            >>> mkTree
            >>> Tree.foldTree (\_ xs -> if null xs then 1 :: Int else sum xs)
            >>> show

planks :: [Int]
planks = [1, 2, 3]

mkTree :: Int -> Tree Int
mkTree n = Tree.unfoldTree mkComb 0
  where
    mkComb :: Int -> (Int, [Int])
    mkComb accLen =
        ( accLen
        , map
            (accLen +)
            (filter (\l -> l + accLen <= n) planks)
        )
