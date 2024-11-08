module Auto where

data ℕ : Set where
  Z : ℕ
  S : ℕ → ℕ

_+_ : ℕ → ℕ → ℕ
Z + n = {!   !}
(S n) + m = {!   !}