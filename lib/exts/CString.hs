module CString(module CString) where

import CTypes
import MarshalArray
import MarshalAlloc
import Ptr
import Exception(bracket)

type CString = Ptr CChar

type CStringLen = (Ptr CChar, Int)

peekCString :: CString -> IO String
peekCString = peekArray0 '\0' 

peekCStringLen :: CStringLen -> IO String
peekCStringLen (s,l) = peekArray l s

newCString :: String -> IO CString
newCString s = newArray0 '\0' s
  
newCStringLen :: String -> IO CStringLen
newCStringLen s = do
  let l = length s
  s' <- mallocArray l
  pokeArray s' s
  return (s',l)
  
freeCString    :: CString -> IO ()
freeCStringLen :: CStringLen -> IO ()
freeCString = free
freeCStringLen (s,_) = free s

withCString :: String -> (CString -> IO a) -> IO a
withCString s = bracket (newCString s) freeCString

withCStringLen :: String -> (CStringLen -> IO a) -> IO a
withCStringLen s = bracket (newCStringLen s) freeCStringLen


