module System.StringTy where

import qualified DotNet ( Object )
import System.ObjectTy

data String_ a
type StringTy a = Object (String_ a)
