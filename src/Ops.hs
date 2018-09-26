module Ops
    ( gcd', lcm'
    ) where

import Debug.Trace

divisionString :: (Int, Int)-> String
divisionString (a,b) = stringA ++ " = "++ stringQ ++"(" ++ stringB ++") + " ++ stringR
    where 
        stringA = show a
        stringB = show b
        stringQ = show $ a `quot` b
        stringR = show $ a `mod` b
        

gcd' :: Int -> Int -> Int
gcd' a b = if a < b 
    then error "could not gcd"
    else let c = a `mod` b 
    in traceStack (divisionString (a,b))
    (if c == 0 then b else gcd' b c)
    
lcm' :: Int -> Int -> Int
lcm' a b = (a * b) `quot` gcd' a b

diophantineEquation (a,x) (b,y) c = error