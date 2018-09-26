module SparseBinary where

newtype SparseBinary = SparseBinary { getSparseBinary :: [Word] } 
 --todo, make the constructor smart

instance Bounded SparseBinary where
    minBound = SparseBinary [ ]
    maxBound = SparseBinary [128,64,32,16,8,4,2,1]

instance Enum SparseBinary where
    fromEnum a = undefined
    toEnum a = undefined
    succ a = undefined
    pred a = undefined

instance Show SparseBinary where
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
