-- !!! Odd pattern binding (legal Haskell, used to be rejected by Hugs)
module M where

f x = let [] = x in x
