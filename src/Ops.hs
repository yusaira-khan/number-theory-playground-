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

gcd' :: (Integral a, Show a) => [a] -> a
gcd' [b]  =b
gcd' (b:b2:l) = if b < b2
    then error "could not gcd"
    else let c = b `rem` b2
    in (if c == 0 then  gcd' b2:l else gcd' c:b2:l)


stackedGcd ::[Int] -> [[DivInfo]] -> [[DivInfo]]
stackedGcd[g] (b:s)= s
stackedGcd (b:a:cs) (s:ss) = let
        (q,r) = quotRem a b
        d = DivInfo {getA=a, getB=b, getQ=q, getR=r}
        ds = (d:s)
    in if r == 0 then stackedGcd (b:cs) ([]:ds:s) else stackedGcd (r:b:cs) (ds:s)

lcm' :: (Integral a, Show a) => [a] -> a
lcm' a =  (foldr (*) 1 a) `quot` (gcd' a)

isMultipleOf :: (Integral a, Show a) => a -> a -> Bool
isMultipleOf a b = (b `rem` a) == 0

diophantineSeries :: [Int]-> [Int]->[Int]
diophantineSeries (c1:[]) (a1:[]) = c1+a1
diophantineSeries (c1:c2:cs) (a1:a2:as) =
    (a1+c1):(a2-c2):(diophantineSeries cs as)

diophantineEquation :: [Int] -> Int -> [[Int]]
diophantineEquation as@[a:b:l] c =  let
        g = gcd' as
    in if isMultipleOf g c then let
            lc = lcm' as
            lcmByAll = map (\x-> lc `quot` x)
            inits = findSingleDiophantine as c
            diophantineSucc = diophantineSeries lcmByAll
        in iterate diophantineSucc inits
        else undefined

-- diophantineString :: Int -> Int -> Int -> String
-- diophantineString a b c =  let
--         (a',b',flip) = if a > b then (b,a,True) else (a,b,False)
--         g = (gcd' a' b')
--     in if isMultipleOf g c then let
--             (bByG',aByG') = (b'`quot`g,a'`quot`g)
--             gcdstack = stackedGcd a' b' []
--             gcdstring = map divInfoString gcdstack
--             (initA', initB') = findSingleDiophantine a' b' c
--             ((bByG,aByG),(initA, initB)) = if flip then ((aByG',bByG'),(initB',initA')) else ((bByG',aByG'),(initA', initB'))
--             eqstring = "x="++(show initA)++"+n*"++(show bByG) ++ " | y="++(show initB)++"-n*"++(show aByG)
--             str = gcdstring ++[eqstring]
--         in unlines str
--         else undefined



hasLinearDiophantineSolution :: Int -> Int -> Int -> Bool
hasLinearDiophantineSolution a b c =
    let g = gcd' a b
    in isMultipleOf g c
--substitute
unstackDivInfo :: [[DivInfo]] -> [Int]
unstackDivInfo [[d]] = [[1,-(getQ d)]]
unstackDivInfo [[d]:dss] =  let j = (head $head (unstackDivInfo dss)) in (map (j*)  [1,-(getQ d)]):(unstackDivInfo dss)
unstackDivInfo ((d:ds):dss) =
    let
        q2' = -(getQ d)
        q1' = 1
        qs = unstackDivInfo (ds:dss)
        q1 = head qs
        q2 = head $ tail qs
        (qA,qB) = (q1'*q2, q1+q2'*q2)
    in qA:qB:qs

modify g coeff val =
    let
        qg = (quot val g)
        m = qg * coeff
    in m

findSingleDiophantine :: [Int] -> Int -> [Int]
findSingleDiophantine [as] c = let
        divstack = stackedGcd as [[]]
        g = getB $ head $ head divstack
        divstack' = map tail divstack
        coeffVar = c `quot` g
        coeffs = unstackDivInfo $ reverse $ map reverse divstack'
        divg = if null (filter (== g) as) then g else 1
        coeffs' = map (modify divg coeffVar) coeffs
    in coeffs'
