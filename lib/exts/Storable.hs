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

import Prelude
import Ptr
import HugsStorable

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

instance Storable Int           where { sizeOf _ = szInt      ; alignment _ = 1; peekElemOff = readIntOffPtr      ; pokeElemOff = writeIntOffPtr       }
instance Storable Char          where { sizeOf _ = szChar     ; alignment _ = 1; peekElemOff = readCharOffPtr     ; pokeElemOff = writeCharOffPtr      }
-- instance Storable WideChar   where { sizeOf _ = szWideChar ; alignment _ = 1; peekElemOff = readWideCharOffPtr ; pokeElemOff = writeWideCharOffPtr  }
-- instance Storable Word       where { sizeOf _ = szWord     ; alignment _ = 1; peekElemOff = readWordOffPtr     ; pokeElemOff = writeWordOffPtr      }
instance Storable (Ptr a)       where { sizeOf _ = szPtr      ; alignment _ = 1; peekElemOff = readPtrOffPtr      ; pokeElemOff = writePtrOffPtr       }
instance Storable (FunPtr a)    where { sizeOf _ = szFunPtr   ; alignment _ = 1; peekElemOff = readFunPtrOffPtr   ; pokeElemOff = writeFunPtrOffPtr    }
instance Storable Float         where { sizeOf _ = szFloat    ; alignment _ = 1; peekElemOff = readFloatOffPtr    ; pokeElemOff = writeFloatOffPtr     }
instance Storable Double        where { sizeOf _ = szDouble   ; alignment _ = 1; peekElemOff = readDoubleOffPtr   ; pokeElemOff = writeDoubleOffPtr    }
instance Storable (StablePtr a) where { sizeOf _ = szStablePtr; alignment _ = 1; peekElemOff = readStablePtrOffPtr; pokeElemOff = writeStablePtrOffPtr }
instance Storable Int8          where { sizeOf _ = szInt8     ; alignment _ = 1; peekElemOff = readInt8OffPtr     ; pokeElemOff = writeInt8OffPtr      }
instance Storable Int16         where { sizeOf _ = szInt16    ; alignment _ = 1; peekElemOff = readInt16OffPtr    ; pokeElemOff = writeInt16OffPtr     }
instance Storable Int32         where { sizeOf _ = szInt32    ; alignment _ = 1; peekElemOff = readInt32OffPtr    ; pokeElemOff = writeInt32OffPtr     }
instance Storable Int64         where { sizeOf _ = szInt64    ; alignment _ = 1; peekElemOff = readInt64OffPtr    ; pokeElemOff = writeInt64OffPtr     }
instance Storable Word8         where { sizeOf _ = szWord8    ; alignment _ = 1; peekElemOff = readWord8OffPtr    ; pokeElemOff = writeWord8OffPtr     }
instance Storable Word16        where { sizeOf _ = szWord16   ; alignment _ = 1; peekElemOff = readWord16OffPtr   ; pokeElemOff = writeWord16OffPtr    }
instance Storable Word32        where { sizeOf _ = szWord32   ; alignment _ = 1; peekElemOff = readWord32OffPtr   ; pokeElemOff = writeWord32OffPtr    }
instance Storable Word64        where { sizeOf _ = szWord64   ; alignment _ = 1; peekElemOff = readWord64OffPtr   ; pokeElemOff = writeWord64OffPtr    }

