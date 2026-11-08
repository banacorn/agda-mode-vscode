module Issue264Overlap where
open import Agda.Builtin.Nat
open import Agda.Builtin.Sigma

test : Σ Nat (λ n → Σ Nat (λ m → Nat)) → Nat
test = ?
