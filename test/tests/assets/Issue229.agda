-- https://github.com/banacorn/agda-mode-vscode/issues/229
module Issue229 where

    data Bool : Set where
        false : Bool
        true : Bool

    and : Bool → Bool → Bool
    and false r = false 
    and true r = r

    _ : Bool
    _ = {! and !}

    x?y : Bool
    x?y = true

    _ : Bool
    _ = {!   !}