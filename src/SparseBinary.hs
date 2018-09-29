--{-# LANGUAGE InstanceSigs #-}
module SparseBinary(SparseBinary(SparseBinary)) where
import Debug.Trace

newtype SparseBinary = SparseBinary { getSparseBinary :: [Word] } 
 --todo, make the constructor smart
--todo, figure out difference between foldl and foldr
-- document functionality in a shell like environment with prints
instance Bounded SparseBinary where
    --minBound :: SparseBinary
    minBound = SparseBinary [ ]
    --maxBound :: SparseBinary
    maxBound = SparseBinary [1,2,4,8,16,32,64,128]

--todo, usefoldl
fromEnumHelper :: [Word] -> Word -> Int
fromEnumHelper sl init = fromIntegral (foldl (+) init sl)

toEnumHelper :: Int -> Word -> [Word] -> [Word]
toEnumHelper 0 _ sAcc = sAcc
toEnumHelper i iPow sAcc = let 
        sAccNew = if (i `mod` 2) == 1 
            then sAcc++[iPow]
            else sAcc
        in toEnumHelper (i `quot` 2) (iPow * 2) sAccNew

addNonConsecutivePowerOf2 :: [Word]->Word -> [Word]
addNonConsecutivePowerOf2 [] p = [p] 
addNonConsecutivePowerOf2 wl@(w:ws) p = if w == p then addNonConsecutivePowerOf2 ws (p*2) else p:wl

powersOf2SmallerThan :: Word -> [Word]
powersOf2SmallerThan 1 = []
powersOf2SmallerThan n = 
    powersOf2SmallerThan halfN ++ [halfN] 
    where halfN = quot n 2

addHelper :: [Word] -> Word -> [Word]
addHelper [] powerOf2 = [powerOf2]
addHelper fullNums@(currentNum:rest) powerOf2 = 
    case (compare powerOf2 currentNum) of 
        LT -> powerOf2:fullNums
        EQ -> addHelper rest (powerOf2*2)
        GT -> currentNum :(addHelper rest powerOf2)

compareHelper :: [Word] -> [Word] -> Ordering
compareHelper [] [] = EQ
compareHelper [] _ = LT
compareHelper _ [] = GT
compareHelper al@(a:as) bl@(b:bs) = let res =  compare a b in case res of
        EQ -> compare as bs
        _ -> res

subHelper :: [Word] -> Word -> [Word]
subHelper [] o = []
subHelper fullNums@(currentNum:rest) powerOf2 = 
    case (compare powerOf2 currentNum) of 
        EQ -> rest 
        LT -> (dropWhile (<powerOf2) $ powersOf2SmallerThan currentNum) ++ rest
        GT -> currentNum :(subHelper rest powerOf2)

instance Enum SparseBinary where
    -- fromEnum :: SparseBinary -> Int
    fromEnum s = fromEnumHelper (getSparseBinary s) 0 
    -- Int -> SparseBinary
    toEnum i = 
        if (i <  fromEnum (minBound :: SparseBinary)) || (i > fromEnum (maxBound :: SparseBinary)) 
            then undefined
            else SparseBinary (toEnumHelper i 1 (getSparseBinary minBound))
    --succ[1,4] == [2,4] == pred [1,2,4]
    --succ[2,4] == [1,2,4]== pred [8]
    --succ :: SparseBinary -> SparseBinary
    succ a = if a == (maxBound :: SparseBinary) 
        then undefined 
        else SparseBinary $ addNonConsecutivePowerOf2  (getSparseBinary a) 1
    --pred :: SparseBinary -> SparseBinary
    pred a = if a == (maxBound :: SparseBinary) 
        then undefined 
        else let b:bs = getSparseBinary a
        in SparseBinary $ powersOf2SmallerThan b ++ bs

instance Show SparseBinary where
    --show :: SparseBinary -> String
    show a = "(SB=" ++ show (getSparseBinary a) ++ "|D=" ++ show (fromEnum a) ++ ")"

instance Eq SparseBinary where
    (==) a b = getSparseBinary a == getSparseBinary b

instance Ord SparseBinary where
    --compare :: SparseBinary -> SparseBinary -> Ordering
    compare a b = 
        let 
            aW = getSparseBinary a
            aWR = reverse aW
            bW = getSparseBinary b
            bWR = reverse bW
        in compareHelper aWR bWR

instance Num SparseBinary where
    (+) a b = 
        let 
            aW = getSparseBinary a
            bW = getSparseBinary b
        in SparseBinary $ foldl addHelper  bW aW 
    (*) a b = 
        let 
            aW = getSparseBinary a
            bW = getSparseBinary b
        in foldl (+) (SparseBinary []) (map (\b -> SparseBinary$ map (b*) aW ) bW)
    abs a = a
    -- signum :: SparseBinary -> SparseBinary
    signum a = if a == toEnum 0 then toEnum 0 else toEnum 1
    -- Integer -> SparseBinary
    fromInteger a = toEnum (fromIntegral a)
    (-) a b = case compare a b of
        EQ -> SparseBinary []
        LT -> b - a
        GT -> let 
                aW = getSparseBinary a
                bW = getSparseBinary b
            in SparseBinary $ foldl subHelper aW bW

instance Real SparseBinary where
    toRational a  = fromIntegral $ fromEnum a

quotHelper :: ([Word],SparseBinary) -> ([Word],[Word]) ->  Word -> ([Word],[Word])
quotHelper (n,sd) (q,r) p = 
    case compare p 1 of
        LT -> (q,r)
        _ ->
            let 
                r' = map (2*) r
                (ne,ns) = if n == [] then (0,[]) else (head n,tail n)
                (n',r'') = if (ne == p) then (ns,1:r') else (n,r')
                sr'' = SparseBinary r''
                (q',r''') = case compare sr'' sd of 
                                LT ->(q,r'')
                                _ -> let    wa = compare sr'' sd
                                            pq = (p:q) 
                                            srsd = (sr'' - sd)
                                        in ((p:q), getSparseBinary (sr''-sd))
            in (quotHelper (n',sd) (q',r''') (p`quot`2))


instance Integral SparseBinary where
    toInteger a = fromIntegral $ fromEnum a
    quotRem a b =   if b == toEnum 0 then undefined else if a == toEnum 0 then (a,a) else let
                aW = getSparseBinary a
                aWR = reverse aW
                largetsPowerOf2 = head aWR
                (qW,rW) = quotHelper (aWR,b) ([],[]) largetsPowerOf2
            in (SparseBinary qW, SparseBinary rW)
    quot  a b= fst $ quotRem a b
    rem a b = snd $ quotRem a b
    divMod = quotRem
    div = quot
    mod = rem