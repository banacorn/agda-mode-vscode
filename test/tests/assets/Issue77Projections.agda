module Issue77Projections where

record R : Set₁ where
  field
    foo : Set

record S : Set₁ where
  field
    foo : Set

open R
open S

prefix : R → Set
prefix r = {! foo r !}

postfix : R → Set
postfix r = {! r .foo !}

qualified : R → Set
qualified r = {! R.foo r !}
