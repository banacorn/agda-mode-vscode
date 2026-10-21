module Issue77Coinductive where

record Stream : Set where
  coinductive
  constructor cons
  field
    tl : Stream

record Box : Set where
  coinductive
  constructor cons
  field
    un : Box

f : Stream → Stream
f s = {! cons s !}
