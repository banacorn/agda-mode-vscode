module Issue335 where

data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

infixl 6 _+_
_+_ : ℕ → ℕ → ℕ
zero + n = n
suc m + n = suc (m + n)

infix 4 _≡_
data _≡_ {A : Set} (x : A) : A → Set where
  refl : x ≡ x

cong : {A B : Set} (f : A → B) {x y : A} → x ≡ y → f x ≡ f y
cong f refl = refl

+-assoc' : (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
+-assoc' m n p = {!   !}
