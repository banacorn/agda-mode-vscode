module Issue77Records where

record Long : Set₁ where
  field
    very-long-field-name-1 : Set
    very-long-field-name-2 : Set
    very-long-field-name-3 : Set

record Same : Set₁ where
  field
    aaaa : Set
    bbbb : Set
    cccc : Set
    dddd : Set
    eeee : Set
    ffff : Set

record One : Set₁ where
  field
    a : Set
    b : Set

record Big : Set₂ where
  field
    foo : Set₁
    bar : Set₁

long : Long
long = {!   !}

module M where
  long' : Long
  long' = {!   !}

same : Same
same = {!   !}

one : One
one = {!   !}

big : Big
big = {! record { foo = Set ; bar = Set } !}

big' : Big
big' = {! record
          { foo = Set
          ; bar = Set
          } !}
