# number-theory-playground
## Series
```haskell
take 10 fib
--[1,1,2,3,5,8,13,21,34,55]
take 10 lukas
--[1,3,4,7,11,18,29,47,76,123]
take 10 ones
--[1,1,1,1,1,1,1,1,1,1]
take 10 natural
--[0,1,2,3,4,5,6,7,8,9]
gcd' 133 85
--1
gcd' 36 24
--12
lcm' 36 24
--72

```
## Sparse Binary
```haskell
minBound::SparseBinary
--(SB=[]|D=0)
maxBound::SparseBinary
--(SB=[1,2,4,8,16,32,64,128]|D=255)
a = toEnum 5 :: SparseBinary
a
--(SB=[1,4]|D=5)
succ a
--(SB=[2,4]|D=6)
succ . succ $ a
--(SB=[1,2,4]|D=7)
pred a
--(SB=[4]|D=4)
pred . pred $ a
--(SB=[1,2]|D=3)
b = a + (succ a)
b
--(SB=[1,2,8]|D=11)
quotRem b a
--((SB=[2]|D=2),(SB=[1]|D=1))
a * b
-- (SB=[1,2,4,16,32]|D=55)
```

## Sparse Binary Series
```haskell
 take 10 natural :: [SparseBinary]
--[(SB=[]|D=0),(SB=[1]|D=1),(SB=[2]|D=2),(SB=[1,2]|D=3),(SB=[4]|D=4),(SB=[1,4]|D=5),(SB=[2,4]|D=6),(SB=[1,2,4]|D=7),(SB=[8]|D=8),(SB=[1,8]|D=9)]
take 10 ones :: [SparseBinary]
--[(SB=[1]|D=1),(SB=[1]|D=1),(SB=[1]|D=1),(SB=[1]|D=1),(SB=[1]|D=1),(SB=[1]|D=1),(SB=[1]|D=1),(SB=[1]|D=1),(SB=[1]|D=1),(SB=[1]|D=1)]
take 10 fib :: [SparseBinary]
--[(SB=[1]|D=1),(SB=[1]|D=1),(SB=[2]|D=2),(SB=[1,2]|D=3),(SB=[1,4]|D=5),(SB=[8]|D=8),(SB=[1,4,8]|D=13),(SB=[1,4,16]|D=21),(SB=[2,32]|D=34),(SB=[1,2,4,16,32]|D=55)]
```
