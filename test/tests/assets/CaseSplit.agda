module CaseSplit where

data ℕ : Set where
  Z : ℕ
  S : ℕ → ℕ

f0 : ℕ → ℕ 
f0 = λ { x → {!   !} ; y → {!   !} }

f1 : ℕ → ℕ 
f1 = λ 
  { x → {!   !} 
  ; y → {!   !}
  }

g0 : ℕ → ℕ 
g0 = λ where x → {!   !}
             y → {!   !}

g1 : ℕ → ℕ 
g1 = λ where 
  x → {!   !}
  y → {!   !}
