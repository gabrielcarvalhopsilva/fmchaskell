module Nat where

import Prelude
    hiding ((+), (-), (*), (^), pred, double, fact, fib, min, max, quot, rem)

data Nat = O | S Nat
    deriving ( Eq , Show )

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