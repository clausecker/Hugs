module Foreign 
	( StablePtr, ForeignObj
	, makeStablePtr, deRefStablePtr, freeStablePtr
	, makeForeignObj, writeForeignObj
	) where

import Addr( Addr )

-- data StablePtr a -- in Prelude

primitive makeStablePtr   :: a -> IO (StablePtr a)
primitive deRefStablePtr  :: StablePtr a -> IO a
primitive freeStablePtr   :: StablePtr a -> IO ()

-- data ForeignObj -- in Prelude

primitive makeForeignObj  :: Addr{-x-} -> Addr{-free-} -> IO ForeignObj
primitive writeForeignObj :: ForeignObj -> Addr -> IO ()
primitive eqForeignObj    :: ForeignObj -> ForeignObj -> Bool

instance Eq ForeignObj where (==) = eqForeignObj
