-- !!! Testing system calls

import Foreign
import Exception
import Prelude hiding (read)

tests = do
  putStrLn "\nTesting open, read and close"
  s <- testRead "testScript" 200
  putStrLn s

  putStrLn "\nTesting open, write and close"
  testWrite "/tmp/test_write" "Test successful"

foreign import ccall safe "static stdlib.h &errno" errno :: Ptr Int
        
withString0 s = bracket (newArray0 '\0' s) free
withBuffer sz m = do
  b <- mallocArray sz
  sz' <- m b
  s <- peekArray sz' b
  free b
  return s

foreign import ccall puts :: Ptr Char -> IO Int


foreign import ccall "open" open'  :: Ptr Char -> Int -> IO Int
foreign import ccall "open" open2' :: Ptr Char -> Int -> Int -> IO Int
foreign import ccall "creat" creat' :: Ptr Char -> Int -> IO Int
foreign import ccall        close :: Int -> IO Int
foreign import ccall "read" read' :: Int -> Ptr Char -> Int -> IO Int
foreign import ccall "write" write' :: Int -> Ptr Char -> Int -> IO Int

creat s m   = withString0 s $ \s' -> unix "creat" $ creat' s' m
open s m    = withString0 s $ \s' -> unix "open"  $ open' s' m
open2 s m n = withString0 s $ \s' -> unix "open2" $ open2' s' m n
write fd s  = withString0 s $ \s' -> unix "write" $ write' fd s' (length s)
read  fd sz = withBuffer sz $ \s' -> unix "read"  $ read' fd s' sz

unix s m = do
  x <- m
  if x < 0 
   then do
     err <- peek errno
     ioError $ userError $ s ++ ": " ++ show (x,err)
   else return x

testRead fn sz = bracket (open fn 0) close (flip read sz)
testWrite fn s = bracket (open2 fn (512+64+1) 511) close (flip write s)

