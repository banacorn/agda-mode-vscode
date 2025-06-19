module CaseSplit where

data ℕ : Set where
  Z : ℕ
  S : ℕ → ℕ

-- MakeCaseType::Function
_+_ : ℕ → ℕ → ℕ
x + y = {!   !}

-- MakeCaseType::ExtendedLambda
f0 : ℕ → ℕ 
f0 = λ { x → {!   !} }

f1 : ℕ → ℕ 
f1 = λ 
  { x → {!   !}
  }

g0 : ℕ → ℕ 
g0 = λ where x → {!   !}

g1 : ℕ → ℕ 
g1 = λ where 
  x → {!   !}

issue16 : ℕ → ℕ
issue16 = λ { x → {!   !} }