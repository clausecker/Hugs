-----------------------------------------------------------------------------
-- Machine Addresses:
-- Suitable for use with Hugs 98 on 32 bit machines.
-----------------------------------------------------------------------------
module Addr
	( Addr
	, nullAddr -- :: Addr
 	, plusAddr -- :: Addr -> Int -> Addr
	, addrToInt -- :: Addr -> Int
	-- instance Eq   Addr
	-- instance Show Addr
	, ptrToAddr        -- :: Ptr a -> Addr
	, addrToPtr        -- :: Addr -> Ptr a
	) where

-- data Addr -- in Prelude

instance Eq   Addr where (==)      = primEqAddr
instance Show Addr where showsPrec = primShowsAddr

primitive nullAddr      :: Addr
primitive plusAddr      :: Addr -> Int -> Addr
primitive primShowsAddr :: Int -> Addr -> ShowS
primitive primEqAddr    :: Addr -> Addr -> Bool
primitive addrToInt     :: Addr -> Int

primitive ptrToAddr    "primUnsafeCoerce" :: Ptr a -> Addr
primitive addrToPtr    "primUnsafeCoerce" :: Addr -> Ptr a
primitive funPtrToAddr "primUnsafeCoerce" :: FunPtr a -> Addr
primitive addrToFunPtr "primUnsafeCoerce" :: Addr -> FunPtr a


-----------------------------------------------------------------------------
