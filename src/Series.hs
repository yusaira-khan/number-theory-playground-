module Series
    ( ones,natural,fib, lukas, hamming, interleave, sieve, subsequentPowers
    ) where

import Debug.Trace

ones :: [Int]
ones = repeat 1

natural :: [Int]
natural = iterate (+1) 0

nextOverlappedNum :: [Int] -> [Int]
nextOverlappedNum (a : t@(b:_)) = (a+b) : nextOverlappedNum t

--infiniteloops
nextNonOverlappedNum (a :b: t) = (a+b) : nextNonOverlappedNum (t)
nextNonOverlappedNum (b:t) = b:nextNonOverlappedNum (b:b:t)

fib :: [Int]
fib = 1 : 1 : nextOverlappedNum fib

--infinite loops
fibscousin = 1 : 1 : nextNonOverlappedNum fibscousin
    
    --next (a : t@(b:_)) = ((traceStack (show (a,b))) (a+b)) : next t

lukas = 1 : 3 : nextOverlappedNum lukas
interleave = id
sieve = id 

subsequentPowers :: Int-> [Int]
subsequentPowers x =  iterate (x*) 1

merge:: [Int] -> [Int]-> [Int]
merge xl@(x:xs) yl@(y:ys) = if (x < y) then (x:(merge xs yl)) else if (x == y) then (x:(merge xs ys)) else y:(merge xl ys)
hamming = merge (subsequentPowers 2) (subsequentPowers 3)
