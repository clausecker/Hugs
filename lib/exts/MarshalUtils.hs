module MarshalUtils (
  with,          -- :: Storable a => a -> (Ptr a -> IO b) -> IO b
  new,           -- :: Storable a => a -> IO (Ptr a)
  fromBool,      -- :: Num a => Bool -> a
  toBool,	 -- :: Num a => a -> Bool
  maybeNew,      -- :: (      a -> IO (Ptr a))
		 -- -> (Maybe a -> IO (Ptr a))
  maybeWith,     -- :: (      a -> (Ptr b -> IO c) -> IO c)
		 -- -> (Maybe a -> (Ptr b -> IO c) -> IO c)
  maybePeek,     -- :: (Ptr a -> IO        b )
		 -- -> (Ptr a -> IO (Maybe b))
  withMany,      -- :: (a -> (b -> res) -> res) -> [a] -> ([b] -> res) -> res
  copyBytes,     -- :: Ptr a -> Ptr a -> Int -> IO ()
  moveBytes,     -- :: Ptr a -> Ptr a -> Int -> IO ()
  withObject     -- :: Storable a => a -> (Ptr a -> IO b) -> IO b
) where

import Maybe
import Storable
import CTypes
import MarshalAlloc
import Ptr

new     :: Storable a => a -> IO (Ptr a)
new val  = 
  do 
    ptr <- malloc
    poke ptr val
    return ptr

with       :: Storable a => a -> (Ptr a -> IO b) -> IO b
with val f  =
  alloca $ \ptr -> do
    poke ptr val
    res <- f ptr
    return res

withObject :: Storable a => a -> (Ptr a -> IO b) -> IO b
withObject  = with

fromBool       :: Num a => Bool -> a
fromBool False  = 0
fromBool True   = 1

toBool :: Num a => a -> Bool
toBool  = (/= 0)

maybeNew :: (      a -> IO (Ptr a))
	 -> (Maybe a -> IO (Ptr a))
maybeNew  = maybe (return nullPtr)

maybeWith :: (      a -> (Ptr b -> IO c) -> IO c) 
	  -> (Maybe a -> (Ptr b -> IO c) -> IO c)
maybeWith  = maybe ($ nullPtr)

maybePeek                           :: (Ptr a -> IO b) -> Ptr a -> IO (Maybe b)
maybePeek peek ptr | ptr == nullPtr  = return Nothing
		   | otherwise       = do a <- peek ptr; return (Just a)

withMany :: (a -> (b -> res) -> res)  -- withXXX combinator for one object
	 -> [a]			      -- storable objects
	 -> ([b] -> res)	      -- action on list of marshalled obj.s
	 -> res
withMany _       []     f = f []
withMany withFoo (x:xs) f = withFoo x $ \x' ->
			      withMany withFoo xs (\xs' -> f (x':xs'))

copyBytes               :: Ptr a -> Ptr a -> Int -> IO ()
copyBytes dest src size  = memcpy dest src (fromIntegral size)

moveBytes               :: Ptr a -> Ptr a -> Int -> IO ()
moveBytes dest src size  = memmove dest src (fromIntegral size)

foreign import ccall unsafe "string.h" memcpy  :: Ptr a -> Ptr a -> CSize -> IO ()
foreign import ccall unsafe "string.h" memmove :: Ptr a -> Ptr a -> CSize -> IO ()
