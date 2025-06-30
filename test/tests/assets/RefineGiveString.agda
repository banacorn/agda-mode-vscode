-- https://github.com/banacorn/agda-mode-vscode/issues/158
module RefineGiveString where

postulate
  A : Set

record Foo : Set where
  field
    very-long-field-name-1 : A
    very-long-field-name-2 : A
    very-long-field-name-3 : A

foo : Foo
foo = {!   !}

bar : Foo
bar = {!   !}