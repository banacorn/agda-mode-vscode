module Issue77Operators where

data A : Set where
  ⟨_⟩ : A → A
  _,_ : A → A → A
  nil : A

data B : Set where
  ⟨_⟩ : B → B
  _,_ : B → B → B
  nil : B

mixfix : A
mixfix = {! ⟨ ⟨ nil ⟩ ⟩ !}

pair : A
pair = {! nil , (nil , nil) !}
