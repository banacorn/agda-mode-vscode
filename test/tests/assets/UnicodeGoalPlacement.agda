module UnicodeGoalPlacement where

-- Test cases for Unicode hole placement issue #250
-- Mathematical bold letters (outside BMP, represented as UTF-16 surrogate pairs)

data ℕ : Set where
  zero : ℕ
  suc : ℕ → ℕ

-- Case 1: Single mathematical bold character before a goal
𝐱 : ℕ
𝐱 = {!   !}

-- Case 2: Multiple mathematical bold characters before a goal
𝐱𝐲𝐳 : ℕ
𝐱𝐲𝐳 = {!   !}

-- Case 3: Mixed Unicode characters with regular ASCII
𝐍ormal-text-then-𝐱 : ℕ
𝐍ormal-text-then-𝐱 = {!   !}

-- Case 4: Mathematical bold characters with multiple goals on same line
𝐚 : ℕ → ℕ → ℕ
𝐚 𝐱 𝐲 = {!   !} {!   !}

-- Case 5: Question mark goals (should expand to {!   !})
𝐛 : ℕ
𝐛 = ?

-- Case 6: Multiple Unicode chars of different types
𝐀𝔅ℂ𝐝 : ℕ  -- Mathematical Bold A, Fraktur B, Double-struck C, Bold d
𝐀𝔅ℂ𝐝 = {!   !}