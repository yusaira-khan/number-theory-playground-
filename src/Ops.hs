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

data DivInfo = DivInfo{getA::Int, getB::Int, getQ::Int, getR::Int}

--todo, use somesort of printer monad instead of trace
gcd' :: (Integral a, Show a) => a -> a -> a
gcd' a b = if a < b
    then error "could not gcd"
    else let c = a `rem` b
    in (if c == 0 then b else gcd' b c)

stackedGcd ::Int -> Int -> [DivInfo] -> [DivInfo]
stackedGcd a b s = let
        (q,r) = quotRem a b
        d = DivInfo {getA=a, getB=b, getQ=q, getR=r}
        ds = d:s
    in if r == 0 then ds else stackedGcd b r ds

lcm' :: (Integral a, Show a) => a -> a -> a
lcm' a b = (a * b) `quot` gcd' a b

isMultipleOf :: (Integral a, Show a) => a -> a -> Bool
isMultipleOf a b = (b `rem` a) == 0


diophantineEquation :: Int -> Int -> Int -> [(Int,Int)]
diophantineEquation a b c = undefined

hasLinearDiophantineSolution :: Int -> Int -> Int -> Bool
hasLinearDiophantineSolution a b c =
    let g = gcd' a b
    in isMultipleOf g c

unstackDivInfo :: [DivInfo] -> (Int,Int)
unstackDivInfo [] = (0,0)
unstackDivInfo [d] = (1,-(getQ d))
unstackDivInfo (d1:d2:ds) =
    let
        (DivInfo {getA=a1,getB=b1,getQ=q1,getR=r1}) = d1
        (DivInfo {getA=a2,getB=b2,getQ=q2,getR=r2}) = d2
        (m1,m2) = unstackDivInfo (d2:ds)
        (q1',q2') = (m2*q1, m2*q2)
    in (q1',-q2')

findSingleDiophantine :: Int -> Int -> Int -> (Int,Int)
findSingleDiophantine a b c = let
        (a',b',flip) = if a < b then (b,a,True) else (a,b,False)
        divstack = stackedGcd a' b' []
        g = getR $ head divstack
        coeffVar = c `quot` g
        (coeffA', coeffB') = unstackDivInfo $ reverse divstack
        (coeffA'', coeffB'') = (coeffA' * coeffVar, coeffB' * coeffVar)

    in undefined