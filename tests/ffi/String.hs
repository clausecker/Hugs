-- !!! Testing marshalling of strings

import Foreign
import Foreign.C
import Control.Exception

tests = do

  putStrLn "\nTesting puts (and withCString)"
  withCString "Test successful" puts

  putStrLn "\nTesting peekArray0 (and withString0)"
  s <- withString0 "Test successful" (peekArray0 '\0')
  putStr s


withString0 s = bracket (newArray0 '\0' s) free

foreign import ccall puts :: CString -> IO Int
