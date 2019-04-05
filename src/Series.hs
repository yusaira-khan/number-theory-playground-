module Series
    ( ones,natural,fib,fib',fib'', lukas, interleave, sieve, subsequentPowers, union,iterateMultiply, hammingSeries
    ) where

import Debug.Trace

ones :: (Num a) => [a]
ones = repeat 1

natural :: (Enum a, Num a) => [a]
natural = iterate succ 0

overlappedSum :: (Num a) => [a] -> [a]
overlappedSum (a : t@(b:_)) = (a+b) : overlappedSum t

overlappedProd :: (Num a) => [a] -> [a]
overlappedProd (a : t@(b:_)) = (a*b) : overlappedProd t

--infiniteloops
nextNonOverlappedNum (a :b: t) = (a+b) : nextNonOverlappedNum t
nextNonOverlappedNum (b:t) = b:nextNonOverlappedNum (b:b:t)

fib :: (Num a) => [a]
fib = 1 : 1 : overlappedSum fib

--infinite loops
fibscousin = 1 : 1 : nextNonOverlappedNum fibscousin

    --next (a : t@(b:_)) = ((traceStack (show (a,b))) (a+b)) : next t
lukas :: (Num a) => [a]
lukas = 1 : 3 : overlappedSum lukas
interleave = id
sieve = id
iterateMultiply :: (Num a) => a-> a -> [a]
iterateMultiply x y= y:iterateMultiply x (x*y)
subsequentPowers :: Int-> [Int]
subsequentPowers x =  iterateMultiply x 1

addlazylist :: (Num a) => [a]-> [a]-> [a]
addlazylist a b = ((head  a)+(head  b)):(addlazylist (tail a) (tail b))
fib':: (Num a) => [a]
fib' =0:fib''

fib'':: (Num a) => [a]
fib''=1:(addlazylist fib'  fib'')
-- subsequentPowersCousin :: Int -> Int  -> [Int]
-- subsequentPowersCousin =  iterateMultiply

union:: [Int] -> [Int]-> [Int]
union xl@(x : xs) yl@(y : ys)
        | x < y = x : union xs yl
        | x == y = x : union xs ys
        | otherwise = y : union xl ys
-- hammingWrong =  overlappedProd (union (subsequentPowersCousin 2 1) (subsequentPowersCousin 3 1))

hammingSeries = 1 : (union (ham 2) (union (ham 3)(ham 5)))
ham n = map (n*) hammingSeries