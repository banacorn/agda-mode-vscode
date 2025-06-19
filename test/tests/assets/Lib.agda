module Lib where

data Bool : Set where
  true  : Bool
  false : Bool

data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

if_then_else_ : {A : Set} → Bool → A → A → A
if_then_else_ true  t _ = t
if_then_else_ false _ f = f