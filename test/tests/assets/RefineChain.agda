module RefineChain where

postulate A : Set

record R : Set where
  constructor node
  field
    f1 f2 f3 f4 f5 : A

foo : A → R
foo i = ?
