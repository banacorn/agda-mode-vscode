module EditDuringLoad where

data ℕ : Set where
  zero : ℕ
  suc : ℕ → ℕ

-- The astral character on the line below has to stay within one padding
-- length of the meta, or the test stops telling the two offset tables apart.
-- Agda counts it as one code point, VSCode as two UTF-16 units. A table built
-- from the text Agda read puts it before the meta; a table built from the
-- padded text puts it after, and the meta lands one unit short.
m : ℕ
-- 𝕏
m = _
