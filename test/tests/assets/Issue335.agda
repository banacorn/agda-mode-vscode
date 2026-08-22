module Issue335 where

data Nat : Set where
  zero : Nat
  suc  : Nat -> Nat

_+_ : Nat -> Nat -> Nat
zero  + n = n
suc m + n = suc (m + n)

data _==_ {A : Set} (x : A) : A -> Set where
  refl : x == x

+-assoc : (m n p : Nat) -> ((m + n) + p) == (m + (n + p))
+-assoc m n p = {!   !}
