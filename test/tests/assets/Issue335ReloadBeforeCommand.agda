module Issue335ReloadBeforeCommand where

open import Agda.Builtin.Nat

double : Nat → Nat
double m = {!   !}
