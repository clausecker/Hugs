module Hugs.ForeignPtr
        ( 
	  ForeignPtr             -- abstract, instance of: Eq
        , newForeignPtr          -- :: Ptr a -> FunPtr (Ptr a -> IO ()) -> IO (ForeignPtr a)
        , addForeignPtrFinalizer -- :: ForeignPtr a -> FunPtr (Ptr a -> IO ()) -> IO ()
	, foreignPtrToPtr	 -- :: ForeignPtr a -> Ptr a
	, touchForeignPtr        -- :: ForeignPtr a -> IO ()
	, castForeignPtr	 -- :: ForeignPtr a -> ForeignPtr b
        ) 
	where

import Hugs.Prelude		( ForeignPtr )
import Foreign.Ptr		( Ptr, FunPtr )

-- data ForeignPtr a -- defined in Prelude.hs

primitive newForeignPtr :: Ptr a -> FunPtr (Ptr a -> IO ()) -> IO (ForeignPtr a)
primitive addForeignPtrFinalizer :: ForeignPtr a -> FunPtr (Ptr a -> IO ()) -> IO ()
primitive touchForeignPtr :: ForeignPtr a -> IO ()
primitive foreignPtrToPtr :: ForeignPtr a -> Ptr a
primitive castForeignPtr "primUnsafeCoerce" :: ForeignPtr a -> ForeignPtr b
