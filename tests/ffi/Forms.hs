-- !!! Testing all the different forms of foreign import


-- Several variants on static import
foreign import ccall "static forms_aux.h foo" si1 :: Int -> IO Int       
foreign import ccall "       forms_aux.h foo" si2 :: Int -> IO Int       
foreign import ccall "static             foo" si3 :: Int -> IO Int       
foreign import ccall "                   foo" si4 :: Int -> IO Int       

-- Foreign import label
foreign import ccall "forms_aux.h &x" xp :: Ptr Int

-- Foreign import dynamic
foreign import ccall "dynamic" d :: FunPtr (Int -> IO Int) -> (Int -> IO Int)

-- Foreign import wrapper
foreign import ccall "wrapper" w :: (Int -> IO Int) -> IO (FunPtr (Int -> IO Int))


-- And again without the IO monad:

-- Several variants on static import
foreign import ccall "static forms_aux.h foo" psi1 :: Int -> Int       
foreign import ccall "       forms_aux.h foo" psi2 :: Int -> Int       
foreign import ccall "static             foo" psi3 :: Int -> Int       
foreign import ccall "                   foo" psi4 :: Int -> Int       

-- Foreign import dynamic
foreign import ccall "dynamic" pd :: FunPtr (Int -> Int) -> (Int -> Int)

-- Foreign import wrapper
foreign import ccall "wrapper" pw :: (Int -> Int) -> IO (FunPtr (Int -> Int))



