--{-# LANGUAGE InstanceSigs #-}
module SparseBinary where

newtype SparseBinary = SparseBinary { getSparseBinary :: [Word] } 
 --todo, make the constructor smart

instance Bounded SparseBinary where
    --minBound :: SparseBinary
    minBound = SparseBinary [ ]
    --maxBound :: SparseBinary
    maxBound = SparseBinary [128,64,32,16,8,4,2,1]

--todo, usefoldl
fromEnumHelper :: [Word] -> Word -> Int
fromEnumHelper sl init = fromIntegral (foldl (+) init sl)

toEnumHelper :: Int -> Word -> [Word] -> [Word]
toEnumHelper 0 _ sAcc = sAcc
toEnumHelper i iPow sAcc = let 
        sAccNew = if (i `mod` 2) == 1 
            then iPow:sAcc
            else sAcc
        in toEnumHelper (i `quot` 2) (iPow * 2) sAccNew

instance Enum SparseBinary where
    -- fromEnum :: SparseBinary -> Int
    fromEnum s = fromEnumHelper (getSparseBinary s) 0 
    toEnum i = 
        if (i <  fromEnum (minBound :: SparseBinary)) || (i > fromEnum (maxBound :: SparseBinary)) 
            then undefined
            else SparseBinary (toEnumHelper i 1 (getSparseBinary minBound))
    succ a = undefined
    pred a = undefined

instance Show SparseBinary where
    --show :: SparseBinary -> String
    show a = show $ getSparseBinary a

instance Eq SparseBinary where
    (==) a b = undefined

instance Ord SparseBinary where
     compare a b =  undefined

instance Num SparseBinary where
    (+) a b = undefined 
    (*) a b = undefined
    abs a = undefined
    signum a = undefined
    fromInteger a = undefined
    (-) a b = undefined

instance Real SparseBinary where
    toRational a  = undefined

instance Integral SparseBinary where
    toInteger a = undefined
    quotRem a b = undefined
