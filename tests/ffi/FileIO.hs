-- !!! Testing system calls

import Foreign
import Control.Exception
import Prelude hiding (read)
import Foreign.C.Types
import Foreign.C.String

tests = do
  putStrLn "\nTesting open, read and close"
  s <- testRead "ffi/FileIO.hs" 200
  putStrLn (map castCCharToChar s)

  putStrLn "\nTesting open, write and close"
  testWrite "/tmp/test_write" "Test successful"

-- Not permitted by a strict interpretation of the FFI, as errno may be
-- a macro.
-- foreign import ccall safe "static errno.h &errno" errno :: Ptr Int

foreign import ccall "FileIO_aux.h" getErrno :: IO Int

withString0 s = bracket (newArray0 '\0' s) free
withBuffer sz m = do
  b <- mallocArray sz
  sz' <- m b
  s <- peekArray sz' b
  free b
  return s

foreign import ccall puts :: Ptr CChar -> IO Int


foreign import ccall "fcntl.h  open"  open'  :: Ptr CChar -> Int -> IO Int
foreign import ccall "fcntl.h  open_for_read"  open2' :: Ptr CChar -> IO Int
foreign import ccall "fcntl.h  creat" creat' :: Ptr CChar -> Int -> IO Int
foreign import ccall "FileIO_aux.h"       close  :: Int -> IO Int
foreign import ccall "FileIO_aux.h read"  read'  :: Int -> Ptr CChar -> Int -> IO Int
foreign import ccall "FileIO_aux.h write" write' :: Int -> Ptr CChar -> Int -> IO Int

creat s m   = withCString s $ \s' -> unix "creat" $ creat' s' m
open s m    = withCString s $ \s' -> unix "open"  $ open' s' m
open2 s     = withCString s $ \s' -> unix "open2" $ open2' s' 
write fd s  = withCString s $ \s' -> unix "write" $ write' fd s' (length s)
read  fd sz = withBuffer sz $ \s' -> unix "read"  $ read' fd s' sz

unix s m = do
  x <- m
  if x < 0 
   then do
     err <- getErrno
     ioError $ userError $ s ++ ": " ++ show (x,err)
   else return x

testRead fn sz = bracket (open fn 0) close (flip read sz)
testWrite fn s = bracket (open2 fn)  close (flip write s)

