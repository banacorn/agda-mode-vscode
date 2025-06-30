-- https://github.com/banacorn/agda-mode-vscode/issues/236
module RefineGiveNoParen where

open import Lib

foo : Bool
foo = {!   !}