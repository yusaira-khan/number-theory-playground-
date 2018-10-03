module Ops where

import Debug.Trace


gcd' :: [Int] -> Int
gcd' [b]  =b
gcd' bb@(b2:b:l) = if  (b < b2)
    then error "could not gcd"
    else let c =(b `rem` b2)
    in (if c == 0 then  gcd' (b2:l) else gcd' (c:b2:l))