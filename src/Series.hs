module Series
    ( ones,natural,fib, lukas, hamming, interleave, sieve, multiple
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
hamming = id
interleave = id
sieve = id 
multiple = id 
