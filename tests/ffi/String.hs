-- !!! Testing marshalling of strings

import Foreign
import Exception

tests = do

  putStrLn "\nTesting puts (and withString0)"
  withString0 "Test successful" puts

  putStrLn "\nTesting peekArray0"
  s <- withString0 "Test successful" (peekArray0 '\0')
  putStr s


withString0 s = bracket (newArray0 '\0' s) free

foreign import ccall puts :: Ptr Char -> IO Int
