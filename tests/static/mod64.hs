-- !!! Constant pattern bindings (used to be illegal in Hugs).
module M where
x = let ['a'] = "a" in 'a'
