module ForeignPtr
        ( 
	  ForeignPtr             -- abstract, instance of: Eq
        , makeForeignPtr          -- :: Ptr a -> FunPtr (Ptr a -> IO ()) -> IO (ForeignPtr a)
--        , addForeignPtrFinalizer -- :: ForeignPtr a -> FunPtr (Ptr a -> IO ()) -> IO ()
	, withForeignPtr         -- :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
	, foreignPtrToPtr	 -- :: ForeignPtr a -> Ptr a
	, touchForeignPtr        -- :: ForeignPtr a -> IO ()
	, castForeignPtr	 -- :: ForeignPtr a -> ForeignPtr b
        ) 
	where

import Ptr
import Dynamic

-- #include "Dynamic.h"
-- INSTANCE_TYPEABLE1(ForeignPtr,foreignPtrTc,"ForeignPtr")

-- data ForeignPtr a -- defined in Prelude.hs

primitive eqForeignPtr  :: ForeignPtr a -> ForeignPtr a -> Bool

instance Eq (ForeignPtr a) where 
    p == q = eqForeignPtr p q
    p /= q = not (eqForeignPtr p q)

primitive makeForeignPtr :: Ptr a -> FunPtr (Ptr a -> IO ()) -> IO ForeignObj

-- not implemented
-- addForeignPtrFinalizer :: ForeignPtr a -> FunPtr (Ptr a -> IO ()) -> IO ()

primitive touchForeignPtr :: ForeignPtr a -> IO ()

withForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
withForeignPtr fo io
  = do r <- io (foreignPtrToPtr fo)
       touchForeignPtr fo
       return r

primitive foreignPtrToPtr :: ForeignPtr a -> Ptr a
primitive castForeignPtr "primUnsafeCoerce" :: ForeignPtr a -> ForeignPtr b

