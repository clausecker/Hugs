-- !!! Testing marshalling of all the basic types


foreign import ccall "[types_aux.c] foo" iInt       :: Int         -> IO Int       
foreign import ccall "[types_aux.c] foo" iChar      :: Char        -> IO Char      
foreign import ccall "[types_aux.c] foo" iAddr      :: Addr        -> IO Addr      
foreign import ccall "[types_aux.c] foo" iPtr       :: Ptr a       -> IO (Ptr a)  
foreign import ccall "[types_aux.c] foo" iFunPtr    :: FunPtr a    -> IO (FunPtr a)
foreign import ccall "[types_aux.c] foo" iFloat     :: Float       -> IO Float     
foreign import ccall "[types_aux.c] foo" iDouble    :: Double      -> IO Double    
foreign import ccall "[types_aux.c] foo" iStablePtr :: StablePtr a -> IO (StablePtr a)
foreign import ccall "[types_aux.c] foo" iInt8      :: Int8        -> IO Int8      
foreign import ccall "[types_aux.c] foo" iInt16     :: Int16       -> IO Int16     
foreign import ccall "[types_aux.c] foo" iInt32     :: Int32       -> IO Int32     
foreign import ccall "[types_aux.c] foo" iInt64     :: Int64       -> IO Int64     
foreign import ccall "[types_aux.c] foo" iWord8     :: Word8       -> IO Word8     
foreign import ccall "[types_aux.c] foo" iWord16    :: Word16      -> IO Word16    
foreign import ccall "[types_aux.c] foo" iWord32    :: Word32      -> IO Word32    
foreign import ccall "[types_aux.c] foo" iWord64    :: Word64      -> IO Word64    

