module Ops where

import Debug.Trace

divisionString :: (Integral a, Show a) => (a, a)-> String
divisionString (a,b) = stringA ++ " = "++ stringQ ++"(" ++ stringB ++") + " ++ stringR
    where
        stringA = show a
        stringB = show b
        stringQ = show $ a `quot` b
        stringR = show $ a `rem` b

data DivInfo = DivInfo{getA::Int, getB::Int, getQ::Int, getR::Int} deriving (Show)

divInfoString :: DivInfo -> String
divInfoString d = stringA ++ " = "++ stringQ ++"(" ++ stringB ++") + " ++ stringR
    where
        stringA = show $ getA d
        stringB = show $ getB d
        stringQ = show $ getQ d
        stringR = show $ getR d

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

diophantineSeries :: (Int,Int)-> (Int,Int)->(Int,Int)
diophantineSeries (coeffA,coeffB) (a,b) =
    ((a+coeffA),(b-coeffB))

diophantineEquation :: Int -> Int -> Int -> [(Int,Int)]
diophantineEquation a b c =  let
        (a',b',flip) = if a < b then (b,a,True) else (a,b,False)
        g = (gcd' a' b')
    in if isMultipleOf g c then let
            (bByG',aByG') = (b'`quot`g,a'`quot`g)
            (initA', initB') = findSingleDiophantine a' b' c
            ((bByG,aByG),(initA, initB)) = if flip then ((aByG',bByG'),(initB',initA')) else ((bByG',aByG'),(initA', initB'))
            diophantineSucc = diophantineSeries (bByG,aByG)
        in iterate diophantineSucc (initA,initB)
        else undefined
diophantineString :: Int -> Int -> Int -> String
diophantineString a b c =  let
        (a',b',flip) = if a < b then (b,a,True) else (a,b,False)
        g = (gcd' a' b')
    in if isMultipleOf g c then let
            (bByG',aByG') = (b'`quot`g,a'`quot`g)
            gcdstack = stackedGcd a' b' []
            gcdstring = map divInfoString gcdstack
            (initA', initB') = findSingleDiophantine a' b' c
            ((bByG,aByG),(initA, initB)) = if flip then ((aByG',bByG'),(initB',initA')) else ((bByG',aByG'),(initA', initB'))
            eqstring = "x="++(show initA)++"+n*"++(show bByG) ++ " | y="++(show initB)++"-n*"++(show aByG)
            str = gcdstring ++[eqstring]
        in unlines str
        else undefined



hasLinearDiophantineSolution :: Int -> Int -> Int -> Bool
hasLinearDiophantineSolution a b c =
    let g = gcd' a b
    in isMultipleOf g c
--substitute
unstackDivInfo :: [DivInfo] -> (Int,Int)
unstackDivInfo [d] = (1,-(getQ d))
unstackDivInfo (d:ds) =
    let
        q2' = -(getQ d)
        q1' = 1
        (q1,q2) = unstackDivInfo (ds)
        (qA,qB) = (q1'*q2, q1+q2'*q2)
    in (qA,qB)

modify g coeff val =
    let
        qg = (quot val g)
        m = qg * coeff
    in m

findSingleDiophantine :: Int -> Int -> Int -> (Int,Int)
findSingleDiophantine a b c = let
        (a',b',flip) = if a < b then (b,a,True) else (a,b,False)
        divstack = stackedGcd a' b' []
        g = getB $ head divstack
        divstack' = tail divstack
        coeffVar = c `quot` g
        (coeffA', coeffB') = if null divstack' then ((getQ $ head divstack),-g) else unstackDivInfo $ reverse divstack'
        divg = if b' == g then g else 1
        (coeffA'', coeffB'') = ((modify divg coeffVar coeffA'), (modify divg coeffVar coeffB'))
        (coeffA, coeffB) = if flip then (coeffB'',coeffA'') else (coeffA'',coeffB'')
    in (coeffA,coeffB)
