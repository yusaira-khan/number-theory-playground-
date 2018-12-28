module Ops where

import Debug.Trace


gcd' :: [Int] -> Int
-- ^The "gcd'" function finds the greatest common divisor  of numbers
-- It takes in an array of elements
gcd' [element]  =element
gcd' elements@(firstElement:secondElement:rest) = if  (firstElement > secondElement)
    then error "could not gcd"
    else let remainder =(secondElement `rem` firstElement)
    in (if remainder == 0 then  gcd' (firstElement:l) else gcd' (secondElement:remainder:l))