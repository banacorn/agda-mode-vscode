-- https://github.com/banacorn/agda-mode-vscode/issues/158
module Refine where

postulate
  A : Set

record Foo : Set where
  field
    very-long-field-name-1 : A
    very-long-field-name-2 : A
    very-long-field-name-3 : A

foo : Foo
foo = {!   !}

open import Lib

fst : Bool → Bool → Bool
fst a b = a

bar : Bool
bar = fst ? ?