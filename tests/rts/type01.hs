-- !!! super-class weirdness (a bug in Feb2001 and Dec2001)
module Mod103 where

import Monad

class Monad m => C1 m x
class (C1 m x) => C2 m x
  where
    c2 :: x -> m x

instance Monad m => C1 m Bool
instance C2 Maybe Bool
  where
    c2 = return


test :: Maybe Bool
test = c2 True

