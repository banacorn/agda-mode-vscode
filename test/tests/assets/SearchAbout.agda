module SearchAbout where

data Bool : Set where
  true  : Bool
  false : Bool

data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

_+_ : ℕ → ℕ → ℕ
zero    + n = n
(suc m) + n = suc (m + n)

if_then_else_ : {A : Set} → Bool → A → A → A
if_then_else_ true  t _ = t
if_then_else_ false _ f = f

-- Some functions to search about
isZero : ℕ → Bool
isZero zero    = true
isZero (suc _) = false

not : Bool → Bool
not true  = false
not false = true

-- Test data for searching
testNat : ℕ
testNat = suc (suc zero)

testBool : Bool
testBool = true