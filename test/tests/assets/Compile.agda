module Compile where

open import Agda.Builtin.IO
open import Agda.Builtin.Unit

postulate
  main : IO ⊤
{-# COMPILE GHC main = return () #-}
