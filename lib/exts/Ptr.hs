-----------------------------------------------------------------------------
-- Machine Addresses:
-- Suitable for use with Hugs 98 on 32 bit machines.
-----------------------------------------------------------------------------
module Ptr
	( Ptr
	, nullPtr          -- :: Ptr a
 	, plusPtr          -- :: Ptr a -> Int -> Ptr b
        , castPtr          -- :: Ptr a -> Ptr b
	, ptrToInt         -- :: Ptr a -> Int
	-- instance Eq   (Ptr a)
	-- instance Show (Ptr a)

        , FunPtr
	, nullFunPtr        -- :: FunPtr a
        , freeHaskellFunPtr -- :: FunPtr a -> IO ()
	-- instance Eq   (FunPtr a)
	-- instance Show (FunPtr a)
	) where

import Prelude

-- data Ptr a -- in Prelude

instance Eq   (Ptr a) where (==)      = primEqPtr
instance Show (Ptr a) where showsPrec = primShowsPtr

primitive nullPtr      "nullAddr"      :: Ptr a
primitive plusPtr      "plusAddr"      :: Ptr a -> Int -> Ptr b
primitive castPtr      "primUnsafeCoerce" :: Ptr a -> Ptr b
primitive primShowsPtr "primShowsAddr" :: Int -> Ptr a -> ShowS
primitive primEqPtr    "primEqAddr"    :: Ptr a -> Ptr a -> Bool
primitive ptrToInt     "addrToInt"     :: Ptr a -> Int

-- data FunPtr a -- in Prelude

instance Eq   (FunPtr a) where (==)      = primEqFPtr
instance Show (FunPtr a) where showsPrec = primShowsFPtr

primitive nullFunPtr   "nullAddr"      :: FunPtr a
primitive freeHaskellFunPtr :: FunPtr a -> IO ()
primitive primShowsFPtr "primShowsAddr" :: Int -> FunPtr a -> ShowS
primitive primEqFPtr    "primEqAddr"    :: FunPtr a -> FunPtr a -> Bool


-----------------------------------------------------------------------------
