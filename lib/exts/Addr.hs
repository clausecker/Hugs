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
	) where

-- data Addr -- in Prelude

instance Eq   Addr where (==)      = primEqAddr
instance Show Addr where showsPrec = primShowsAddr

primitive nullAddr      :: Addr
primitive plusAddr      :: Addr -> Int -> Addr
primitive primShowsAddr :: Int -> Addr -> ShowS
primitive primEqAddr    :: Addr -> Addr -> Bool
primitive addrToInt     :: Addr -> Int


-----------------------------------------------------------------------------
