module Issue77Constructors where

data ℕ : Set where
  zero : ℕ
  suc : ℕ → ℕ

data A : Set where
  con : A → A → A
  nil : A

data B : Set where
  con : B
  nil : B

g : A → A
g z = z

refined : A
refined = {! con !}

nested : A
nested = {! con (con nil nil) nil !}

qualified : A
qualified = {! A.con A.nil A.nil !}

nospace : A
nospace = {!con nil nil!}

multiline : A
multiline = {! con
                nil
                nil !}

vars : A → A → A
vars x y = {! con x y !}

fun : A
fun = {! con (g nil) nil !}

number : ℕ
number = {! suc !}
