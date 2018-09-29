--{-# LANGUAGE InstanceSigs #-}
module SparseBinary where
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
addHelper [] powerOf2 = trace ("adding to empty "++show powerOf2) [powerOf2]
addHelper fullNums@(currentNum:rest) powerOf2 = 
    case (compare powerOf2 currentNum) of 
      LT -> trace ("LT adding " ++ (show powerOf2) ++ " to " ++ (show fullNums) ++ "to give " ++ show(powerOf2:fullNums)) powerOf2:fullNums
      EQ -> trace ("EQ adding " ++ (show powerOf2) ++ " to " ++ (show fullNums) ++ "to give ") addHelper rest (powerOf2*2)
      GT -> trace ("GT adding " ++ (show [powerOf2]) ++ " to " ++ (show fullNums) ++ "to give ") currentNum :(addHelper rest powerOf2)

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
     compare a b =  undefined

instance Num SparseBinary where
    (+) a b = 
        let 
            aW = getSparseBinary a
            bW = getSparseBinary b
        in SparseBinary $ foldl addHelper  bW aW 
    (*) a b = undefined
    --     let 
    --         aW = getSparseBinary a
    --         bW = getSparseBinary b
    --     in SparseBinary $ foldl (\aW' b -> foldl (*) aW' b) aW bW
    abs a = a
    signum a = if a == toEnum 0 then toEnum 0 else toEnum 1
    -- Integer -> SparseBinary
    fromInteger a = toEnum (fromIntegral a)
    (-) a b = undefined

instance Real SparseBinary where
    toRational a  = undefined

instance Integral SparseBinary where
    toInteger a = undefined
    quotRem a b = undefined
