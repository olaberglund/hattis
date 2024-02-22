#!/bin/sh

cat > ./app/Main.hs <<- EOM
module Main where

import Control.Arrow ((>>>))

main :: IO ()
main = interact \$
  lines >>>
EOM
