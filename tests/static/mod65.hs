-- !!! Malformed lhs (legal Haskell, rejected by Hugs)
module M where

f x = let [] = x in x
