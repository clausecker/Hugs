module Hugs.ForeignPtr
        ( 
	  ForeignPtr             -- abstract, instance of: Eq
	, FinalizerPtr
        , newForeignPtr_         -- :: Ptr a -> IO (ForeignPtr a)
        , addForeignPtrFinalizer -- :: FinalizerPtr a -> ForeignPtr a -> IO ()
	, unsafeForeignPtrToPtr	 -- :: ForeignPtr a -> Ptr a
	, touchForeignPtr        -- :: ForeignPtr a -> IO ()
	, castForeignPtr	 -- :: ForeignPtr a -> ForeignPtr b
        ) 
	where

import Hugs.Prelude		( ForeignPtr )
import Foreign.Ptr		( Ptr, FunPtr )

-- data ForeignPtr a -- defined in Prelude.hs

type FinalizerPtr a = FunPtr (Ptr a -> IO ())
primitive newForeignPtr_ :: Ptr a -> IO (ForeignPtr a)
primitive addForeignPtrFinalizer :: FinalizerPtr a -> ForeignPtr a -> IO ()
primitive touchForeignPtr :: ForeignPtr a -> IO ()
primitive unsafeForeignPtrToPtr :: ForeignPtr a -> Ptr a
primitive castForeignPtr "primUnsafeCoerce" :: ForeignPtr a -> ForeignPtr b
