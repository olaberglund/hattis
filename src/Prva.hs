{-# LANGUAGE OverloadedStrings #-}

module Prva where

import Control.Arrow ((>>>))
import Data.List (sort)
import qualified Data.Text as T

main :: IO ()
main =
  interact $
    T.pack
      >>> T.lines
      >>> tail
      >>> (\rows -> sort $ mkWords rows <> mkWords (T.transpose rows))
      >>> head
      >>> T.unpack

mkWords :: [T.Text] -> [T.Text]
mkWords = concatMap (filter ((> 1) . T.length) . T.splitOn "#")
