#!/bin/sh

cat > ./app/Main.hs <<- EOM
module Main where

main :: IO ()
main = interact undefined
EOM
