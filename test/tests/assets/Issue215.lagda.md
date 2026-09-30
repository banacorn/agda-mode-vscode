# Issue 215 reproduction

```agda
module Issue215 where

data Carrier : Set where
  first-long-carrier-element-name : Carrier
  second-long-carrier-element-name : Carrier

data Target : Set where
  make-target-from-two-long-carrier-elements : Carrier → Carrier → Target

pair-of-targets : Target → Target → Target
pair-of-targets t _ = t

result : Target
result = pair-of-targets {!   !} {!   !}
```
