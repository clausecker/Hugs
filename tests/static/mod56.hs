-- !!! Illegal deriving Ix
module M where
data T = K1 Int | K2 deriving (Eq,Ord,Ix)
