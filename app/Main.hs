module Main where

import SparseBinary

main :: IO ()
main = 
    let 
        a = (toEnum 64)::SparseBinary
        b = succ a
    in print a >> print b >> print (a+b)

