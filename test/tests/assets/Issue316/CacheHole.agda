module CacheHole where

id : Set -> Set
id A = {! A !}

const : Set -> Set -> Set
const A B = {! A !}
