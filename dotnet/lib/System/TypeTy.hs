module System.TypeTy where

import qualified DotNet ( Object )
import System.ObjectTy

data Type_ a
type Type a = Object (Type_ a)
