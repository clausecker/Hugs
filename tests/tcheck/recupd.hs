-- !!! Testing type of updated record
module Bug where

data Pair a b = Pair { left :: a, right :: b } 

test = Pair { left = True, right = "foo" } { right = 'b' }
