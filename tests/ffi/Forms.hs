-- !!! Testing all the different forms of foreign import


-- Several variants on static import
foreign import ccall "static forms_aux.h [forms_aux.c] foo" si1 :: Int -> IO Int       
foreign import ccall "       forms_aux.h [forms_aux.c] foo" si2 :: Int -> IO Int       
foreign import ccall "static             [forms_aux.c] foo" si3 :: Int -> IO Int       
foreign import ccall "                   [forms_aux.c] foo" si4 :: Int -> IO Int       
foreign import ccall "static forms_aux.h               foo" si5 :: Int -> IO Int       
foreign import ccall "       forms_aux.h               foo" si6 :: Int -> IO Int       
foreign import ccall "static                           foo" si7 :: Int -> IO Int       
foreign import ccall "                                 foo" si8 :: Int -> IO Int       

-- Foreign import label
foreign import ccall "&x" xp :: Ptr Int

-- Foreign import dynamic
foreign import ccall "dynamic" d :: FunPtr (Int -> IO Int) -> (Int -> IO Int)

-- Foreign import wrapper
foreign import ccall "wrapper" w :: (Int -> IO Int) -> IO (FunPtr (Int -> IO Int))


-- And again without the IO monad:

-- Several variants on static import
foreign import ccall "static forms_aux.h [forms_aux.c] foo" psi1 :: Int -> Int       
foreign import ccall "       forms_aux.h [forms_aux.c] foo" psi2 :: Int -> Int       
foreign import ccall "static             [forms_aux.c] foo" psi3 :: Int -> Int       
foreign import ccall "                   [forms_aux.c] foo" psi4 :: Int -> Int       
foreign import ccall "static forms_aux.h           foo" psi5 :: Int -> Int       
foreign import ccall "       forms_aux.h           foo" psi6 :: Int -> Int       
foreign import ccall "static                       foo" psi7 :: Int -> Int       
foreign import ccall "                             foo" psi8 :: Int -> Int       

-- Foreign import dynamic
foreign import ccall "dynamic" pd :: FunPtr (Int -> Int) -> (Int -> Int)

-- Foreign import wrapper
foreign import ccall "wrapper" pw :: (Int -> Int) -> IO (FunPtr (Int -> Int))



