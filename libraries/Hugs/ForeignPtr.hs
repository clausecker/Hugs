module Hugs.ForeignPtr
        ( 
	  ForeignPtr             -- abstract, instance of: Eq
	, FinalizerPtr
        , newForeignPtr          -- :: Ptr a -> FinalizerPtr a -> IO (ForeignPtr a)
        , addForeignPtrFinalizer -- :: ForeignPtr a -> FinalizerPtr a -> IO ()
	, foreignPtrToPtr	 -- :: ForeignPtr a -> Ptr a
	, touchForeignPtr        -- :: ForeignPtr a -> IO ()
	, castForeignPtr	 -- :: ForeignPtr a -> ForeignPtr b
        ) 
	where

import Hugs.Prelude		( ForeignPtr )
import Foreign.Ptr		( Ptr, FunPtr )

-- data ForeignPtr a -- defined in Prelude.hs

type FinalizerPtr a = FunPtr (Ptr a -> IO ())
primitive newForeignPtr :: Ptr a -> FinalizerPtr a -> IO (ForeignPtr a)
primitive addForeignPtrFinalizer :: ForeignPtr a -> FinalizerPtr a -> IO ()
primitive touchForeignPtr :: ForeignPtr a -> IO ()
primitive foreignPtrToPtr :: ForeignPtr a -> Ptr a
primitive castForeignPtr "primUnsafeCoerce" :: ForeignPtr a -> ForeignPtr b
