module Ops
    ( gcd', lcm'
    ) where

import Debug.Trace

gcd' :: Int -> Int -> Int
gcd' a b = if a < b 
    then error "could not gcd"
    else let c = a `mod` b 
    in traceStack ("gcding" ++ show (a,b))
    (if c == 0 then b else gcd' b c)
lcm' = id