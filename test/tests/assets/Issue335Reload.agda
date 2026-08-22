{-# OPTIONS --cubical #-}
module Issue335Reload where

open import Agda.Primitive.Cubical
open import Agda.Builtin.Cubical.Path

refl' : {A : Set} (x : A) → x ≡ x
refl' x = ?
