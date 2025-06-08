-- from issue #158
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

bar : Foo
bar = {!   !}