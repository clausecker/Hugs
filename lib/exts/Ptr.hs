-----------------------------------------------------------------------------
-- Machine Addresses:
-- Suitable for use with Hugs 98 on 32 bit machines.
-----------------------------------------------------------------------------
module Ptr
	( Ptr
	, nullPtr  -- :: Ptr a
 	, plusPtr  -- :: Ptr a -> Int -> Ptr b
	, ptrToInt -- :: Ptr a -> Int
	-- instance Eq   (Ptr a)
	-- instance Show (Ptr a)
	) where

-- data Ptr a -- in Prelude

instance Eq   (Ptr a) where (==)      = primEqPtr
instance Show (Ptr a) where showsPrec = primShowsPtr

primitive nullPtr      "nullAddr"      :: Ptr a
primitive plusPtr      "plusAddr"      :: Ptr a -> Int -> Ptr b
primitive primShowsPtr "primShowsAddr" :: Int -> Ptr a -> ShowS
primitive primEqPtr    "primEqAddr"    :: Ptr a -> Ptr a -> Bool
primitive ptrToInt     "addrToInt"     :: Ptr a -> Int


-----------------------------------------------------------------------------
