--
-- The IEnumerator interface
-- 
module System.Collections.IEnumerator where

import DotNet
import qualified System.Object

data IEnumerator_ a
type IEnumerator a = Object (IEnumerator_ a)

-- ToDo: make this type-safe.

current :: System.Object.Object a -> IO (System.Object.Object b)
current = invoke "get_Current" ()

moveNext :: System.Object.Object a -> IO Bool
moveNext = invoke "MoveNext" ()

reset :: System.Object.Object a -> IO ()
reset = invoke "Reset" ()

