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

instance Storable Int           where { sizeOf _ = szInt      ; alignment _ = 1; peekElemOff = rdInt      ; pokeElemOff = wrInt       }
instance Storable Char          where { sizeOf _ = szChar     ; alignment _ = 1; peekElemOff = rdChar     ; pokeElemOff = wrChar      }
-- instance Storable WideChar   where { sizeOf _ = szWideChar ; alignment _ = 1; peekElemOff = rdWideChar ; pokeElemOff = wrWideChar  }
-- instance Storable Word       where { sizeOf _ = szWord     ; alignment _ = 1; peekElemOff = rdWord     ; pokeElemOff = wrWord      }
instance Storable (Ptr a)       where { sizeOf _ = szPtr      ; alignment _ = 1; peekElemOff = rdPtr      ; pokeElemOff = wrPtr       }
instance Storable (FunPtr a)    where { sizeOf _ = szFunPtr   ; alignment _ = 1; peekElemOff = rdFunPtr   ; pokeElemOff = wrFunPtr    }
instance Storable Float         where { sizeOf _ = szFloat    ; alignment _ = 1; peekElemOff = rdFloat    ; pokeElemOff = wrFloat     }
instance Storable Double        where { sizeOf _ = szDouble   ; alignment _ = 1; peekElemOff = rdDouble   ; pokeElemOff = wrDouble    }
instance Storable (StablePtr a) where { sizeOf _ = szStablePtr; alignment _ = 1; peekElemOff = rdStablePtr; pokeElemOff = wrStablePtr }
instance Storable Int8          where { sizeOf _ = szInt8     ; alignment _ = 1; peekElemOff = rdInt8     ; pokeElemOff = wrInt8      }
instance Storable Int16         where { sizeOf _ = szInt16    ; alignment _ = 1; peekElemOff = rdInt16    ; pokeElemOff = wrInt16     }
instance Storable Int32         where { sizeOf _ = szInt32    ; alignment _ = 1; peekElemOff = rdInt32    ; pokeElemOff = wrInt32     }
instance Storable Int64         where { sizeOf _ = szInt64    ; alignment _ = 1; peekElemOff = rdInt64    ; pokeElemOff = wrInt64     }
instance Storable Word8         where { sizeOf _ = szWord8    ; alignment _ = 1; peekElemOff = rdWord8    ; pokeElemOff = wrWord8     }
instance Storable Word16        where { sizeOf _ = szWord16   ; alignment _ = 1; peekElemOff = rdWord16   ; pokeElemOff = wrWord16    }
instance Storable Word32        where { sizeOf _ = szWord32   ; alignment _ = 1; peekElemOff = rdWord32   ; pokeElemOff = wrWord32    }
instance Storable Word64        where { sizeOf _ = szWord64   ; alignment _ = 1; peekElemOff = rdWord64   ; pokeElemOff = wrWord64    }

foreign import ccall unsafe "[Storable_aux.o]" szInt       :: Int
foreign import ccall unsafe "[Storable_aux.o]" szChar      :: Int
-- foreign import ccall unsafe "[Storable_aux.o]" szWideChar  :: Int
-- foreign import ccall unsafe "[Storable_aux.o]" szWord      :: Int
foreign import ccall unsafe "[Storable_aux.o]" szPtr       :: Int
foreign import ccall unsafe "[Storable_aux.o]" szFunPtr    :: Int
foreign import ccall unsafe "[Storable_aux.o]" szFloat     :: Int
foreign import ccall unsafe "[Storable_aux.o]" szDouble    :: Int
foreign import ccall unsafe "[Storable_aux.o]" szStablePtr :: Int
foreign import ccall unsafe "[Storable_aux.o]" szInt8      :: Int
foreign import ccall unsafe "[Storable_aux.o]" szInt16     :: Int
foreign import ccall unsafe "[Storable_aux.o]" szInt32     :: Int
foreign import ccall unsafe "[Storable_aux.o]" szInt64     :: Int
foreign import ccall unsafe "[Storable_aux.o]" szWord8     :: Int
foreign import ccall unsafe "[Storable_aux.o]" szWord16    :: Int
foreign import ccall unsafe "[Storable_aux.o]" szWord32    :: Int
foreign import ccall unsafe "[Storable_aux.o]" szWord64    :: Int

foreign import ccall unsafe "[Storable_aux.o]" rdInt       :: Ptr Int           -> Int -> IO Int
foreign import ccall unsafe "[Storable_aux.o]" rdChar      :: Ptr Char          -> Int -> IO Char
-- foreign import ccall unsafe "[Storable_aux.o]" rdWideChar  :: Ptr Char          -> Int -> IO Char
-- foreign import ccall unsafe "[Storable_aux.o]" rdWord      :: Ptr Word          -> Int -> IO Word
foreign import ccall unsafe "[Storable_aux.o]" rdPtr       :: Ptr (Ptr a)       -> Int -> IO (Ptr a)
foreign import ccall unsafe "[Storable_aux.o]" rdFunPtr    :: Ptr (FunPtr a)    -> Int -> IO (FunPtr a)
foreign import ccall unsafe "[Storable_aux.o]" rdFloat     :: Ptr Float         -> Int -> IO Float
foreign import ccall unsafe "[Storable_aux.o]" rdDouble    :: Ptr Double        -> Int -> IO Double
foreign import ccall unsafe "[Storable_aux.o]" rdStablePtr :: Ptr (StablePtr a) -> Int -> IO (StablePtr a)
foreign import ccall unsafe "[Storable_aux.o]" rdInt8      :: Ptr Int8          -> Int -> IO Int8
foreign import ccall unsafe "[Storable_aux.o]" rdInt16     :: Ptr Int16         -> Int -> IO Int16
foreign import ccall unsafe "[Storable_aux.o]" rdInt32     :: Ptr Int32         -> Int -> IO Int32
foreign import ccall unsafe "[Storable_aux.o]" rdInt64     :: Ptr Int64         -> Int -> IO Int64
foreign import ccall unsafe "[Storable_aux.o]" rdWord8     :: Ptr Word8         -> Int -> IO Word8
foreign import ccall unsafe "[Storable_aux.o]" rdWord16    :: Ptr Word16        -> Int -> IO Word16
foreign import ccall unsafe "[Storable_aux.o]" rdWord32    :: Ptr Word32        -> Int -> IO Word32
foreign import ccall unsafe "[Storable_aux.o]" rdWord64    :: Ptr Word64        -> Int -> IO Word64

foreign import ccall unsafe "[Storable_aux.o]" wrInt       :: Ptr Int           -> Int -> Int         -> IO ()
foreign import ccall unsafe "[Storable_aux.o]" wrChar      :: Ptr Char          -> Int -> Char        -> IO ()
-- foreign import ccall unsafe "[Storable_aux.o]" wrWideChar  :: Ptr Char          -> Int -> Char        -> IO ()
-- foreign import ccall unsafe "[Storable_aux.o]" wrWord      :: Ptr Word          -> Int -> Word        -> IO ()
foreign import ccall unsafe "[Storable_aux.o]" wrPtr       :: Ptr (Ptr a)       -> Int -> Ptr a       -> IO ()
foreign import ccall unsafe "[Storable_aux.o]" wrFunPtr    :: Ptr (FunPtr a)    -> Int -> FunPtr a    -> IO ()
foreign import ccall unsafe "[Storable_aux.o]" wrFloat     :: Ptr Float         -> Int -> Float       -> IO ()
foreign import ccall unsafe "[Storable_aux.o]" wrDouble    :: Ptr Double        -> Int -> Double      -> IO ()
foreign import ccall unsafe "[Storable_aux.o]" wrStablePtr :: Ptr (StablePtr a) -> Int -> StablePtr a -> IO ()
foreign import ccall unsafe "[Storable_aux.o]" wrInt8      :: Ptr Int8          -> Int -> Int8        -> IO ()
foreign import ccall unsafe "[Storable_aux.o]" wrInt16     :: Ptr Int16         -> Int -> Int16       -> IO ()
foreign import ccall unsafe "[Storable_aux.o]" wrInt32     :: Ptr Int32         -> Int -> Int32       -> IO ()
foreign import ccall unsafe "[Storable_aux.o]" wrInt64     :: Ptr Int64         -> Int -> Int64       -> IO ()
foreign import ccall unsafe "[Storable_aux.o]" wrWord8     :: Ptr Word8         -> Int -> Word8       -> IO ()
foreign import ccall unsafe "[Storable_aux.o]" wrWord16    :: Ptr Word16        -> Int -> Word16      -> IO ()
foreign import ccall unsafe "[Storable_aux.o]" wrWord32    :: Ptr Word32        -> Int -> Word32      -> IO ()
foreign import ccall unsafe "[Storable_aux.o]" wrWord64    :: Ptr Word64        -> Int -> Word64      -> IO ()

