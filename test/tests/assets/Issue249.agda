-- https://github.com/banacorn/agda-mode-vscode/issues/249
module Issue249 where

open import Agda.Builtin.Sigma
open import Agda.Builtin.Unit

bug : Σ ⊤ λ _ → ⊤
bug = ?

{- This is a comment. -}