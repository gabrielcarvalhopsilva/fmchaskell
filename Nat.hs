{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Eta reduce" #-}
module Nat where

import Prelude
    hiding ((+), (-), (*), (^), pred, double, fact, fib, min, max, quot, rem, length, elem, sum, product, (++), reverse,
    allEven, anyEven, allOdd, anyOdd, allZero, anyZero, addNat, mulNat, expNat, enumFromTo, take, drop)

data Nat = O | S Nat
    deriving ( Eq , Show )

data ListNat = Empty | Cons Nat ListNat
    deriving ( Eq, Show )

if_then_else :: Bool -> a -> a -> a
if_then_else True n m = n
if_then_else False n m = m

(+) :: Nat -> Nat -> Nat
n + O = n
n + (S m) = S(n + m)

(-) :: Nat -> Nat -> Nat
n - O = n
O - n = O
(S n) - (S m) = n - m

(*) :: Nat -> Nat -> Nat
n * O = O
n * (S m) = n + n * m

(^) :: Nat -> Nat -> Nat
n ^ O = S O
n ^ (S m) = n * (n ^ m)

pred :: Nat -> Nat
pred O = O
pred (S n) = n

double :: Nat -> Nat
double O = O
double (S n) = S (S (double n))

fact :: Nat -> Nat
fact O = S O
fact (S n) = S n * fact n

fib :: Nat -> Nat
fib O = O
fib (S O) =  S O
fib (S(S n)) = fib(S n) + fib n

min :: Nat -> Nat -> Nat
min O n = O
min n O = O
min (S n) (S m) = S (min n m)

max :: Nat -> Nat -> Nat
max O n = n
max n O = n
max (S n) (S m) = S (max n m)

quot :: Nat -> Nat -> Nat
quot O n = O
quot n m = S(quot (n-m) n)

rem :: Nat -> Nat -> Nat
rem O n = O
rem n m = n - (quot n m * m)

leq :: Nat -> Nat -> Bool
leq O n = True
leq n O = False
leq (S n) (S m) = leq n m

ev :: Nat -> Bool
ev O = True
ev (S O) = False
ev (S(S n)) = ev n

od :: Nat -> Bool
od O = False
od (S O) = True
od (S(S n )) = od n

isMul₃ :: Nat -> Bool
isMul₃ O = True
isMul₃ (S O) = False
isMul₃ (S (S O)) = False
isMul₃ (S(S (S n))) = isMul₃ n

divides :: Nat -> Nat -> Bool
divides n m = if_then_else (rem n m == O) True False

isZero :: Nat -> Bool
isZero O = True
isZero (S n) = False

length :: ListNat -> Nat
length Empty = O
length (Cons x xs) = S(length xs)

elem :: Nat -> ListNat -> Bool
elem n Empty = False
elem n (Cons x xs) = if_then_else (n == x) True (elem n xs)

sum :: ListNat -> Nat
sum Empty = O
sum (Cons x xs) = x + sum xs

product :: ListNat -> Nat
product Empty = S O
product (Cons x xs) = x * product xs

(++) :: ListNat -> ListNat -> ListNat
Empty ++ xs = xs
xs ++ Empty = xs
(Cons x xs) ++ ys = Cons x (xs ++ ys)

reverse :: ListNat -> ListNat
reverse Empty = Empty
reverse (Cons x xs) = xs ++ Cons x Empty

allEven :: ListNat -> Bool
allEven Empty = True
allEven (Cons x xs) = if_then_else (ev x) (allEven xs) False

anyEven :: ListNat -> Bool
anyEven Empty = False
anyEven (Cons x xs) = if_then_else (ev x) True (anyEven xs)

allOdd :: ListNat -> Bool
allOdd Empty = True
allOdd (Cons x xs) = if_then_else (od x) (allOdd xs) False

anyOdd :: ListNat -> Bool
anyOdd Empty = False
anyOdd (Cons x xs) = if_then_else (od x) True (anyOdd xs)

allZero :: ListNat -> Bool
allZero Empty = True
allZero (Cons x xs) = if_then_else (isZero x) (allZero xs) False

anyZero :: ListNat -> Bool
anyZero Empty = False
anyZero (Cons x xs) = if_then_else (isZero x) True (anyZero xs)

addNat :: Nat -> ListNat -> ListNat
addNat n Empty = Empty
addNat n (Cons m ms) = Cons (n + m) (addNat n ms)

mulNat :: Nat -> ListNat -> ListNat
mulNat n Empty = Empty
mulNat n (Cons m ms) = Cons (n * m) (mulNat n ms)

expNat :: Nat -> ListNat -> ListNat
expNat n Empty = Empty
expNat n (Cons m ms) = Cons (n ^ m) (expNat n ms)

enumFromTo :: Nat -> Nat -> ListNat
enumFromTo n n = Cons n Empty
enumFromTo n m = Cons n (enumFromTo (S n) m)

enumTo :: Nat -> ListNat
enumTo n = enumFromTo O n

take :: Nat -> ListNat -> ListNat
take n Empty = Empty
take (S n) (Cons x xs) = Cons x (take n xs)

drop :: Nat -> ListNat -> ListNat
drop O l = l
drop (S n) (Cons x xs) = drop n xs