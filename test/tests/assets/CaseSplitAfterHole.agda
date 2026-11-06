module CaseSplitAfterHole where

open import Agda.Builtin.String

data ℕ : Set where
  Z : ℕ
  S : ℕ → ℕ

_+_ : ℕ → ℕ → ℕ
a + b = a

_++_ : String → ℕ → ℕ
s ++ n = n

a--b : ℕ
a--b = Z

whereish : ℕ
whereish = Z

𝐵 : ℕ
𝐵 = Z

app : (ℕ → ℕ) → ℕ
app f = f Z

h : {n : ℕ} → ℕ
h {n} = n

record R : Set where
  field
    a : ℕ
    c : ℕ

data Box : Set where
  box : ℕ → Box
  nox : Box

split : Box → ℕ
split b = {! !}
