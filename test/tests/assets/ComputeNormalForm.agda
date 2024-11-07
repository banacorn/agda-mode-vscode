module ComputeNormalForm where

data ℕ : Set where
  Z : ℕ
  S : ℕ → ℕ

_+_ : ℕ → ℕ → ℕ
Z + n = n
(S n) + m = S (n + m)