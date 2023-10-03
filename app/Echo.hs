module Echo where

main :: IO ()
main = interact (unwords . replicate 3 . concat . lines)
