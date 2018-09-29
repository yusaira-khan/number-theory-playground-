module Series
    ( ones,natural,fib, lukas, interleave, sieve, subsequentPowers
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

subsequentPowers :: Int-> [Int]
subsequentPowers x =  iterate (x*) 1

subsequentPowersCousin :: Int -> Int  -> [Int]
subsequentPowersCousin x =  iterate (x*)

merge:: [Int] -> [Int]-> [Int]
merge xl@(x : xs) yl@(y : ys)
        | x < y = x : merge xs yl
        | x == y = x : merge xs ys
        | otherwise = y : merge xl ys
hammingWrong =  overlappedProd (merge (subsequentPowersCousin 2 1) (subsequentPowersCousin 3 1))
