module Ops
    ( gcd', lcm'
    ) where

import Debug.Trace

divisionString :: (Integral a, Show a) => (a, a)-> String
divisionString (a,b) = stringA ++ " = "++ stringQ ++"(" ++ stringB ++") + " ++ stringR
    where
        stringA = show a
        stringB = show b
        stringQ = show $ a `quot` b
        stringR = show $ a `rem` b

--todo, use somesort of printer monad instead of trace
gcd' :: (Integral a, Show a) => a -> a -> a
gcd' a b = if a < b
    then error "could not gcd"
    else let c = a `rem` b
    in traceStack (divisionString (a,b))
    (if c == 0 then b else gcd' b c)

lcm' :: (Integral a, Show a) => a -> a -> a
lcm' a b = (a * b) `quot` gcd' a b

diophantineEquation (a,x) (b,y) c = undefined