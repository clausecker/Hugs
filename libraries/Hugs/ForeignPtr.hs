module Hugs.ForeignPtr
        ( 
	  ForeignPtr             -- abstract, instance of: Eq
        , newForeignPtr          -- :: Ptr a -> FunPtr (Ptr a -> IO ()) -> IO (ForeignPtr a)
        , addForeignPtrFinalizer -- :: ForeignPtr a -> FunPtr (Ptr a -> IO ()) -> IO ()
        , mallocForeignPtr       -- :: Storable a => IO (ForeignPtr a) 
        , mallocForeignPtrBytes  -- :: Int        -> IO (ForeignPtr a)
	, withForeignPtr         -- :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
	, foreignPtrToPtr	 -- :: ForeignPtr a -> Ptr a
	, touchForeignPtr        -- :: ForeignPtr a -> IO ()
	, castForeignPtr	 -- :: ForeignPtr a -> ForeignPtr b
        ) 
	where

import Hugs.Prelude
import Foreign.Ptr
import Foreign.Storable		( Storable )
import Foreign.Marshal.Alloc	( malloc, mallocBytes, finalizerFree )

-- data ForeignPtr a -- defined in Prelude.hs

primitive eqForeignPtr  :: ForeignPtr a -> ForeignPtr a -> Bool

instance Eq (ForeignPtr a) where 
    p == q = eqForeignPtr p q
    p /= q = not (eqForeignPtr p q)

primitive newForeignPtr :: Ptr a -> FunPtr (Ptr a -> IO ()) -> IO (ForeignPtr a)

primitive addForeignPtrFinalizer :: ForeignPtr a -> FunPtr (Ptr a -> IO ()) -> IO ()

primitive touchForeignPtr :: ForeignPtr a -> IO ()

mallocForeignPtr :: Storable a => IO (ForeignPtr a)
mallocForeignPtr = do
  r <- malloc
  newForeignPtr r finalizerFree

mallocForeignPtrBytes :: Int -> IO (ForeignPtr a)
mallocForeignPtrBytes n = do
  r <- mallocBytes n
  newForeignPtr r finalizerFree

withForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
withForeignPtr fo io
  = do r <- io (foreignPtrToPtr fo)
       touchForeignPtr fo
       return r

primitive foreignPtrToPtr :: ForeignPtr a -> Ptr a
primitive castForeignPtr "primUnsafeCoerce" :: ForeignPtr a -> ForeignPtr b
