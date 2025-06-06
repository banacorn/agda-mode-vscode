module Issue204 where

data Nat : Set where
  zero : Nat
  suc : (n : Nat) → Nat

{-# BUILTIN NATURAL Nat #-}

data Fin : Nat → Set where
  zero : {n : Nat} → Fin (suc n)
  suc  : {n : Nat} (i : Fin n) → Fin (suc n)

three1 : Fin 5
three1 = suc {n =  4 !}} (suc (suc zero))

three2 : Fin 5
three2 = suc {n = {!  !}} (suc (suc zero))