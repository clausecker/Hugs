module System.ObjectTy where

import qualified DotNet ( Object )

data Object_ a
type Object a = DotNet.Object (Object_ a)




