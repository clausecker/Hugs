module MarshalArray (
  mallocArray,    -- :: Storable a => Int -> IO (Ptr a)
  mallocArray0,   -- :: Storable a => Int -> IO (Ptr a)
  allocaArray,    -- :: Storable a => Int -> (Ptr a -> IO b) -> IO b
  allocaArray0,   -- :: Storable a => Int -> (Ptr a -> IO b) -> IO b
  reallocArray,   -- :: Storable a => Ptr a -> Int -> IO (Ptr a)
  reallocArray0,  -- :: Storable a => Ptr a -> Int -> IO (Ptr a)
  peekArray,      -- :: Storable a =>         Int -> Ptr a -> IO [a]
  peekArray0,     -- :: (Storable a, Eq a) => a   -> Ptr a -> IO [a]
  pokeArray,      -- :: Storable a =>      Ptr a -> [a] -> IO ()
  pokeArray0,     -- :: Storable a => a -> Ptr a -> [a] -> IO ()
  newArray,       -- :: Storable a =>      [a] -> IO (Ptr a)
  newArray0,      -- :: Storable a => a -> [a] -> IO (Ptr a)
  withArray,      -- :: Storable a =>      [a] -> (Ptr a -> IO b) -> IO b
  withArray0,     -- :: Storable a => a -> [a] -> (Ptr a -> IO b) -> IO b
  copyArray,      -- :: Storable a => Ptr a -> Ptr a -> Int -> IO ()
  moveArray,      -- :: Storable a => Ptr a -> Ptr a -> Int -> IO ()
  lengthArray0,   -- :: (Storable a, Eq a) => a -> Ptr a -> IO Int
  advancePtr,     -- :: Storable a => Ptr a -> Int -> Ptr a
) where

import Storable
import MarshalAlloc
import MarshalUtils
import Ptr
import Monad

mallocArray :: Storable a => Int -> IO (Ptr a)
mallocArray  = doMalloc undefined
  where
    doMalloc            :: Storable a => a -> Int -> IO (Ptr a)
    doMalloc dummy size  = mallocBytes (size * sizeOf dummy)

mallocArray0      :: Storable a => Int -> IO (Ptr a)
mallocArray0 size  = mallocArray (size + 1)

allocaArray :: Storable a => Int -> (Ptr a -> IO b) -> IO b
allocaArray  = doAlloca undefined
  where
    doAlloca            :: Storable a => a -> Int -> (Ptr a -> IO b) -> IO b
    doAlloca dummy size  = allocaBytes (size * sizeOf dummy)

allocaArray0      :: Storable a => Int -> (Ptr a -> IO b) -> IO b
allocaArray0 size  = allocaArray (size + 1)

reallocArray :: Storable a => Ptr a -> Int -> IO (Ptr a)
reallocArray  = doRealloc undefined
  where
    doRealloc                :: Storable a => a -> Ptr a -> Int -> IO (Ptr a)
    doRealloc dummy ptr size  = reallocBytes ptr (size * sizeOf dummy)

reallocArray0          :: Storable a => Ptr a -> Int -> IO (Ptr a)
reallocArray0 ptr size  = reallocArray ptr (size + 1)


peekArray          :: Storable a => Int -> Ptr a -> IO [a]
peekArray size ptr | size <= 0 = return []
                 | otherwise = f (size-1) []
  where
    f 0 acc = do e <- peekElemOff ptr 0; return (e:acc)
    f n acc = do e <- peekElemOff ptr n; f (n-1) (e:acc)
  
peekArray0            :: (Storable a, Eq a) => a -> Ptr a -> IO [a]
peekArray0 marker ptr  = loop 0
  where
    loop i = do
        val <- peekElemOff ptr i
        if val == marker then return [] else do
            rest <- loop (i+1)
            return (val:rest)

pokeArray          :: Storable a => Ptr a -> [a] -> IO ()
pokeArray ptr vals  = zipWithM_ (pokeElemOff ptr) [0..] vals

pokeArray0		   :: Storable a => a -> Ptr a -> [a] -> IO ()
pokeArray0 marker ptr vals  = do
  pokeArray ptr vals
  pokeElemOff ptr (length vals) marker


newArray      :: Storable a => [a] -> IO (Ptr a)
newArray vals  = do
  ptr <- mallocArray (length vals)
  pokeArray ptr vals
  return ptr

newArray0             :: Storable a => a -> [a] -> IO (Ptr a)
newArray0 marker vals  = do
  ptr <- mallocArray0 (length vals)
  pokeArray0 marker ptr vals
  return ptr

withArray        :: Storable a => [a] -> (Ptr a -> IO b) -> IO b
withArray vals f  =
  allocaArray len $ \ptr -> do
      pokeArray ptr vals
      res <- f ptr
      return res
  where
    len = length vals

withArray0               :: Storable a => a -> [a] -> (Ptr a -> IO b) -> IO b
withArray0 marker vals f  =
  allocaArray0 len $ \ptr -> do
      pokeArray0 marker ptr vals
      res <- f ptr
      return res
  where
    len = length vals


copyArray :: Storable a => Ptr a -> Ptr a -> Int -> IO ()
copyArray  = doCopy undefined
  where
    doCopy                     :: Storable a => a -> Ptr a -> Ptr a -> Int -> IO ()
    doCopy dummy dest src size  = copyBytes dest src (size * sizeOf dummy)

moveArray :: Storable a => Ptr a -> Ptr a -> Int -> IO ()
moveArray  = doMove undefined
  where
    doMove                     :: Storable a => a -> Ptr a -> Ptr a -> Int -> IO ()
    doMove dummy dest src size  = moveBytes dest src (size * sizeOf dummy)


lengthArray0            :: (Storable a, Eq a) => a -> Ptr a -> IO Int
lengthArray0 marker ptr  = loop 0
  where
    loop i = do
        val <- peekElemOff ptr i
        if val == marker then return i else loop (i+1)


advancePtr :: Storable a => Ptr a -> Int -> Ptr a
advancePtr  = doAdvance undefined
  where
    doAdvance             :: Storable a => a -> Ptr a -> Int -> Ptr a
    doAdvance dummy ptr i  = ptr `plusPtr` (i * sizeOf dummy)
