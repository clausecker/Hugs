-- !!! Testing marshalling of all the basic types


foreign import ccall "types_aux.h" iInt       :: Int         -> IO Int       
foreign import ccall "types_aux.h" iChar      :: Char        -> IO Char      
foreign import ccall "types_aux.h" iAddr      :: Addr        -> IO Addr      
foreign import ccall "types_aux.h" iPtr       :: Ptr a       -> IO (Ptr a)  
foreign import ccall "types_aux.h" iFunPtr    :: FunPtr a    -> IO (FunPtr a)
foreign import ccall "types_aux.h" iFloat     :: Float       -> IO Float     
foreign import ccall "types_aux.h" iDouble    :: Double      -> IO Double    
foreign import ccall "types_aux.h" iStablePtr :: StablePtr a -> IO (StablePtr a)
foreign import ccall "types_aux.h" iInt8      :: Int8        -> IO Int8      
foreign import ccall "types_aux.h" iInt16     :: Int16       -> IO Int16     
foreign import ccall "types_aux.h" iInt32     :: Int32       -> IO Int32     
foreign import ccall "types_aux.h" iInt64     :: Int64       -> IO Int64     
foreign import ccall "types_aux.h" iWord8     :: Word8       -> IO Word8     
foreign import ccall "types_aux.h" iWord16    :: Word16      -> IO Word16    
foreign import ccall "types_aux.h" iWord32    :: Word32      -> IO Word32    
foreign import ccall "types_aux.h" iWord64    :: Word64      -> IO Word64    

