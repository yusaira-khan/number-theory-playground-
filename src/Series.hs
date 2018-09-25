module Series
    ( ones,natural,fib, lukas, hamming, interleave, sieve, subsequentPowers
    ) where

import Debug.Trace

ones :: [Int]
ones = repeat 1

natural :: [Int]
natural = iterate (+1) 0

overlappedSum :: [Int] -> [Int]
overlappedSum (a : t@(b:_)) = (a+b) : overlappedSum t

overlappedProd :: [Int] -> [Int]
overlappedProd (a : t@(b:_)) = (a*b) : overlappedProd t

--infiniteloops
nextNonOverlapped (a :b: t) = (a+b) : nextNonOverlappedNum (t)
nextNonOverlappedNum (b:t) = b:nextNonOverlappedNum (b:b:t)

fib :: [Int]
fib = 1 : 1 : overlappedSum fib

--infinite loops
fibscousin = 1 : 1 : nextNonOverlappedNum fibscousin
    
    --next (a : t@(b:_)) = ((traceStack (show (a,b))) (a+b)) : next t

lukas = 1 : 3 : overlappedSum lukas
interleave = id
sieve = id 

subsequentPowers :: Int-> [Int]
subsequentPowers x =  iterate (x*) 1

subsequentPowersCousin :: Int -> Int  -> [Int]
subsequentPowersCousin x =  iterate (x*) 

merge:: [Int] -> [Int]-> [Int]
merge xl@(x:xs) yl@(y:ys) = if (x < y) then (x:(merge xs yl)) else if (x == y) then (x:(merge xs ys)) else y:(merge xl ys)
hamming =  overlappedProd (merge (subsequentPowersCousin 2 1) (subsequentPowersCousin 3 1))
