-- !!! (n+k) constant pattern test
-- (broke desugarer in Nov2002 release, as reported by 
-- Thomas Hallgren.)
module M where

v = let (x+10) = 12 in x
