--{-# LANGUAGE InstanceSigs #-}
module SparseBinary where

newtype SparseBinary = SparseBinary { getSparseBinary :: [Word] } 
 --todo, make the constructor smart

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
    succ a = if a == (maxBound :: SparseBinary) then undefined else SparseBinary $ addNonConsecutivePowerOf2  (getSparseBinary a) 1
    --pred :: SparseBinary -> SparseBinary
    pred a = undefined

instance Show SparseBinary where
    --show :: SparseBinary -> String
    show a = "(SB=" ++ show (getSparseBinary a) ++ "|D=" ++ show (fromEnum a) ++ ")"

instance Eq SparseBinary where
    (==) a b = (getSparseBinary a) == (getSparseBinary b)

instance Ord SparseBinary where
     compare a b =  undefined

instance Num SparseBinary where
    (+) a b = undefined 
    (*) a b = undefined
    abs a = undefined
    signum a = undefined
    -- Integer -> SparseBinary
    fromInteger a = toEnum (fromIntegral a)
    (-) a b = undefined

instance Real SparseBinary where
    toRational a  = undefined

instance Integral SparseBinary where
    toInteger a = undefined
    quotRem a b = undefined
