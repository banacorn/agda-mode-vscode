{-# OPTIONS --allow-exec #-}

module Issue152 where

open import Agda.Builtin.List
open import Agda.Builtin.Nat
open import Agda.Builtin.Reflection
open import Agda.Builtin.Reflection.External
open import Agda.Builtin.Sigma
open import Agda.Builtin.String
open import Agda.Builtin.Unit

handle : Term → Σ Nat (λ _ → Σ String (λ _ → String)) → TC ⊤
handle hole (zero  , (stdout , stderr)) = unify hole (lit (string stdout))
handle hole (suc _ , (stdout , stderr)) = typeError (strErr stderr ∷ [])

macro
  runTouch : Term → TC ⊤
  runTouch hole = bindTC
    (execTC "touch" ("issue-152-created-by-reflection" ∷ []) "")
    (handle hole)

test : String
test = runTouch
