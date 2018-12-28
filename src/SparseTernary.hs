--{-# LANGUAGE InstanceSigs #-}
module SparseTernary(SparseTernary(SparseTernary)) where
import Debug.Trace

newtype SparseTernary = SparseTernary {getSparseTernary:: [Word]}
 --todo, make the constructor smart
--todo, figure out difference between foldl and foldr
-- document functionality in a shell like environment with prints
instance Bounded SparseTernary where
    --minBound :: SparseBinary
    minBound = SparseTernary [ ]
    --maxBound :: SparseBinary
    maxBound = SparseTernary [2,6,18,54,162]

--todo, usefoldl
fromEnumHelper :: [Word] -> Word -> Int
fromEnumHelper sl init = fromIntegral (foldl (+) init sl)

toEnumHelper :: Int -> Word -> [Word] -> [Word]
toEnumHelper 0 _ sAcc = sAcc
toEnumHelper i iPow sAcc = let
        mod3val = (i `mod` 3)
        in let sAccNew = if mod3val == 0
            then sAcc
            else (sAcc*mod3val)++[iPow]
        in toEnumHelper (i `quot` 3) (iPow * 3) sAccNew

addNonConsecutivePowerOf3 :: [Word] -> Word -> [Word]
addNonConsecutivePowerOf3 [] p = [p]
addNonConsecutivePowerOf3 wl@(w:ws) p = if w == p then addNonConsecutivePowerOf3 ws (p*3) else p:wl

powersOf3SmallerThan :: Word -> [Word]
powersOf3SmallerThan 1 = []
powersOf3SmallerThan n =
    powersOf3SmallerThan thirdN ++ [thirdN*2]
    where thirdN = quot n 3

addHelper :: [Word] -> Word -> [Word]
addHelper [] powerOf3 = [powerOf3]
addHelper fullNums@(currentNum:rest) powerOf3 =
    case compare powerOf3 currentNum of
        LT -> powerOf3:fullNums
        EQ -> addHelper rest (powerOf3*3)
        GT -> currentNum :addHelper rest powerOf3

compareHelper :: [Word] -> [Word] -> Ordering
compareHelper [] [] = EQ
compareHelper [] _ = LT
compareHelper _ [] = GT
compareHelper al@(a:as) bl@(b:bs) = let res = compare a b in case res of
        EQ -> compare as bs
        _ -> res

subHelper :: [Word] -> Word -> [Word]
subHelper [] o = []
subHelper fullNums@(currentNum:rest) powerOf3 =
    case compare powerOf3 currentNum of
        EQ -> rest
        LT -> dropWhile (<powerOf3) (powersOf3SmallerThan currentNum) ++ rest
        GT -> currentNum : subHelper rest powerOf3

instance Enum SparseTernary where
    -- fromEnum :: SparseBinary -> Int
    fromEnum s = fromEnumHelper (getSparseBinary s) 0
    -- Int -> SparseBinary
    toEnum i =
        if (i < fromEnum (minBound :: SparseBinary)) || (i > fromEnum (maxBound :: SparseBinary))
            then undefined
            else SparseBinary (toEnumHelper i 1 [])
    --succ[1,4] == [2,4] == pred [1,2,4]
    --succ[2,4] == [1,2,4]== pred [8]
    --succ :: SparseBinary -> SparseBinary
    succ a = if a == (maxBound :: SparseTernary)
        then undefined
        else SparseTernary $ addNonConsecutivePowerOf2 (getSparseBinary a) 1
    --pred :: SparseBinary -> SparseBinary
    pred a = if a == (maxBound :: SparseTernary)
        then undefined
        else let b:bs = getSparseBinary a
        in SparseTernary $ powersOf2SmallerThan b ++ bs

instance Show SparseTernary where
    --show :: SparseBinary -> String
    show a = "(SB=" ++ show (SparseTernary a) ++ "|D=" ++ show (fromEnum a) ++ ")"

instance Eq SparseTernary where
    (==) a b = getSparseTernary a == getSparseTernary b

instance Ord SparseTernary where
    --compare :: SparseBinary -> SparseBinary -> Ordering
    compare a b =
        let
            aW = getSparseTernary a
            aWR = reverse aW
            bW = getSparseTernary b
            bWR = reverse bW
        in compareHelper aWR bWR

instance Num SparseTernary where
    (+) a b =
        let
            aW = getSparseTernary a
            bW = getSparseTernary b
        in SparseTernary $ foldl addHelper bW aW
    (*) a b =
        let
            aW = getSparseTernary a
            bW = getSparseTernary b
        in foldl (+) (SparseTernary []) (map (\b -> SparseTernary$ map (b*) aW ) bW)
    abs a = a
    -- signum :: SparseTernary -> SparseTernary
    signum a = if a == toEnum 0 then toEnum 0 else toEnum 1
    -- Integer -> SparseTernary
    fromInteger a = toEnum (fromIntegral a)
    (-) a b = case compare a b of
        EQ -> SparseTernary []
        LT -> b - a
        GT -> let
                aW = getSparseTernary a
                bW = getSparseTernary b
            in SparseTernary $ foldl subHelper aW bW

instance Real SparseTernary where
    toRational a = fromIntegral $ fromEnum a

quotHelper :: ([Word],SparseTernary) -> ([Word],[Word]) -> Word -> ([Word],[Word])
quotHelper (n,sd) (q,r) p =
    case compare p 1 of
        LT -> (q,r)
        _ ->
            let
                r' = map (3*) r
                (ne,ns) = if null n then (0,[]) else (head n,tail n)
                (n',r'') = if ne == p then (ns,1:r') else (n,r')
                sr'' = SparseTernary r''
                (q',r''') = case compare sr'' sd of
                                LT -> (q,r'')
                                _ -> (p:q, getSparseTernary (sr''-sd))
            in quotHelper (n',sd) (q',r''') (p`quot`2)

instance Integral SparseTernary where
    toInteger a = fromIntegral $ fromEnum a
    quotRem a b
         | b == toEnum 0 = undefined
         | a == toEnum 0 = (a,a)
         | otherwise = let
                aW = getSparseTernary a
                aWR = reverse aW
                largetsPowerOf2 = head aWR
                (qW,rW) = quotHelper (aWR,b) ([],[]) largetsPowerOf2
            in (SparseTernary qW, SparseTernary rW)
    quot a b = fst $ quotRem a b
    rem a b = snd $ quotRem a b
    divMod = quotRem
    div = quot
    mod = rem