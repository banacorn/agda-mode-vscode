module DownloadedALS where

data Nat : Set where
  zero : Nat
  suc : Nat → Nat

one : Nat
one = suc zero
