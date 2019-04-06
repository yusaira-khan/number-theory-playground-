module Prime
    ( isPrime
    ) where

tenPrimes=[2,3,5,7,11,13,17,23,29]
largestVal=960
listofPrimes = tenPrimes
getSqrtFloor :: Int->Int
getSqrtFloor a = 0

isDivisibleBySinglePrime:: Int->Int->Bool
isDivisibleBySinglePrime candidate primeKnown = ( getSqrtFloor candidate > primeKnown ) && (candidate `mod` primeKnown == 0)
isPrime:: Int -> Bool
isPrime c = c<largestVal && any (isDivisibleBySinglePrime c) tenPrimes

-- sieve::[Int]
-- sieve = (functions)iterate (1+)


--- append appplicatives?
-- filtersieve p (x : xs)=  if (x%p ==0)then rest else x:rest where rest = filtersieve p xs

-- sieve' = 2:filtersieve 2 sieve''
-- sieve'' =3:
