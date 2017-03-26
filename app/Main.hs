module Main where

import Auto

main :: IO ()
main = interact id

data Character = AA | BB deriving (Bounded, Enum, Eq, Show)