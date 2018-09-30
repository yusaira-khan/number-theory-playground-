module Main where

import SparseBinary
import Ops

main :: IO ()
main =
    print $ findSingleDiophantine 4 2 6

