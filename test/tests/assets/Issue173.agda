module Issue173 where

_ : {A : Set} → A → A
_ = λ 𝒶 → {! 𝒶 !}