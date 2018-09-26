module SparseBinary where

newtype SparseBinary = SparseBinary { getSparseBinary :: [Word] } deriving (Show)


instance Bounded SparseBinary where
    minBound = undefined
    maxBound = undefined

instance Enum SparseBinary where
    fromEnum a = undefined
    toEnum a = undefined
    succ a = undefined
    pred a = undefined

-- instance Show SparseBinary where
--     show a = undefined

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


