{-# OPTIONS --cubical #-}
module Issue335Boundary where

open import Agda.Primitive.Cubical
open import Agda.Builtin.Cubical.Path

refl' : {A : Set} (x : A) → x ≡ x
refl' x = {!   !}
