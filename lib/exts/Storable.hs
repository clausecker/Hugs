-- Based on GHC.Storable
-- This is just a temporary placeholder until we merge properly
-- with the new libraries so I've kept it as barebones as possible. - ADR

module Storable
	( Storable(
	     sizeOf,         -- :: a -> Int
	     alignment,      -- :: a -> Int
	     peekElemOff,    -- :: Ptr a -> Int      -> IO a
	     pokeElemOff,    -- :: Ptr a -> Int -> a -> IO ()
	     peekByteOff,    -- :: Ptr b -> Int      -> IO a
	     pokeByteOff,    -- :: Ptr b -> Int -> a -> IO ()
	     peek,           -- :: Ptr a             -> IO a
	     poke)           -- :: Ptr a        -> a -> IO ()
        ) where

import Ptr

class Storable a where
   sizeOf      :: a -> Int
   alignment   :: a -> Int
   peekElemOff :: Ptr a -> Int      -> IO a
   pokeElemOff :: Ptr a -> Int -> a -> IO ()
   peekByteOff :: Ptr b -> Int      -> IO a
   pokeByteOff :: Ptr b -> Int -> a -> IO ()
   peek        :: Ptr a      -> IO a
   poke        :: Ptr a -> a -> IO ()

--    peekElemOff = peekElemOff_ undefined
--       where peekElemOff_ :: a -> Ptr a -> Int -> IO a
--             peekElemOff_ undef ptr off = peekByteOff ptr (off * sizeOf undef)
   pokeElemOff ptr off val = pokeByteOff ptr (off * sizeOf val) val
   peekByteOff ptr off = peek (ptr `plusPtr` off)
   pokeByteOff ptr off = poke (ptr `plusPtr` off)
   peek ptr = peekElemOff ptr 0
   poke ptr = pokeElemOff ptr 0

instance Storable Int where {
    sizeOf    _ = 4;		
    alignment _ = 1;	
    peekElemOff = readIntOffPtr;		
    pokeElemOff = writeIntOffPtr }

foreign import ccall unsafe readIntOffPtr  :: Ptr Int -> Int        -> IO Int
foreign import ccall unsafe writeIntOffPtr :: Ptr Int -> Int -> Int -> IO ()

instance Storable Char where {
    sizeOf    _ = 1;		
    alignment _ = 1;	
    peekElemOff = readCharOffPtr;		
    pokeElemOff = writeCharOffPtr }

foreign import ccall unsafe "[Storable_aux.o]" readCharOffPtr  :: Ptr Char -> Int        -> IO Char
foreign import ccall unsafe "[Storable_aux.o]" writeCharOffPtr :: Ptr Char -> Int -> Char -> IO ()

-- readWideCharOffPtr  :: Ptr Char          -> Int -> IO Char
-- readWordOffPtr      :: Ptr Word          -> Int -> IO Word
-- readPtrOffPtr       :: Ptr (Ptr a)       -> Int -> IO (Ptr a)
-- readFunPtrOffPtr    :: Ptr (FunPtr a)    -> Int -> IO (FunPtr a)
-- readFloatOffPtr     :: Ptr Float         -> Int -> IO Float
-- readDoubleOffPtr    :: Ptr Double        -> Int -> IO Double
-- readStablePtrOffPtr :: Ptr (StablePtr a) -> Int -> IO (StablePtr a)
-- readInt8OffPtr      :: Ptr Int8          -> Int -> IO Int8
-- readInt16OffPtr     :: Ptr Int16         -> Int -> IO Int16
-- readInt32OffPtr     :: Ptr Int32         -> Int -> IO Int32
-- readInt64OffPtr     :: Ptr Int64         -> Int -> IO Int64
-- readWord8OffPtr     :: Ptr Word8         -> Int -> IO Word8
-- readWord16OffPtr    :: Ptr Word16        -> Int -> IO Word16
-- readWord32OffPtr    :: Ptr Word32        -> Int -> IO Word32
-- readWord64OffPtr    :: Ptr Word64        -> Int -> IO Word64

-- writeWideCharOffPtr  :: Ptr Char          -> Int -> Char        -> IO ()
-- writeIntOffPtr       :: Ptr Int           -> Int -> Int         -> IO ()
-- writeWordOffPtr      :: Ptr Word          -> Int -> Word        -> IO ()
-- writePtrOffPtr       :: Ptr (Ptr a)       -> Int -> Ptr a       -> IO ()
-- writeFunPtrOffPtr    :: Ptr (FunPtr a)    -> Int -> FunPtr a    -> IO ()
-- writeFloatOffPtr     :: Ptr Float         -> Int -> Float       -> IO ()
-- writeDoubleOffPtr    :: Ptr Double        -> Int -> Double      -> IO ()
-- writeStablePtrOffPtr :: Ptr (StablePtr a) -> Int -> StablePtr a -> IO ()
-- writeInt8OffPtr      :: Ptr Int8          -> Int -> Int8        -> IO ()
-- writeInt16OffPtr     :: Ptr Int16         -> Int -> Int16       -> IO ()
-- writeInt32OffPtr     :: Ptr Int32         -> Int -> Int32       -> IO ()
-- writeInt64OffPtr     :: Ptr Int64         -> Int -> Int64       -> IO ()
-- writeWord8OffPtr     :: Ptr Word8         -> Int -> Word8       -> IO ()
-- writeWord16OffPtr    :: Ptr Word16        -> Int -> Word16      -> IO ()
-- writeWord32OffPtr    :: Ptr Word32        -> Int -> Word32      -> IO ()
-- writeWord64OffPtr    :: Ptr Word64        -> Int -> Word64      -> IO ()

