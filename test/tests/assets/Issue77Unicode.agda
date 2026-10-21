module Issue77Unicode where

record R : Set₁ where
  field
    𝕗𝕠𝕠 : Set
    𝕓𝕒𝕣 : Set

r : R
r = {!   !}
