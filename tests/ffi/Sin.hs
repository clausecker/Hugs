-- !!! Testing static, dynamic and wrapped import of trig function

-- Imports kept minimal to avoid pulling in Storable and other
-- things which use even more ffi.
import System.IO.Unsafe( unsafePerformIO )
import Foreign.Ptr( FunPtr, freeHaskellFunPtr )

tests = do

  putStrLn "\nTesting sin==mysin (should return lots of Trues)"
  print (testSin sin mysin)

  putStrLn "\nTesting sin==dynamic_sin (should return lots of Trues)"
  print (testSin sin (dyn_sin sin_addr))

  putStrLn "\nTesting sin==IO wrapped_sin (should return lots of Trues)"
  sin_addr2 <- wrapIO (return . sin)
  print (testSin sin (unsafePerformIO . (dyn_sinIO sin_addr2)))
  freeHaskellFunPtr sin_addr2

  putStrLn "\nTesting sin==Id wrapped_sin (should return lots of Trues)"
  sin_addr3 <- wrapId sin
  print (testSin sin (dyn_sin sin_addr3))
  freeHaskellFunPtr sin_addr3

testSin f g = [ (f x == g x) | x <- [0,0.01 .. 1] ]

foreign import ccall "math.h sin" mysin :: Double -> Double
foreign import ccall "dynamic" dyn_sin :: FunPtr (Double -> Double) -> (Double -> Double)
foreign import ccall "dynamic" dyn_sinIO :: FunPtr (Double -> IO Double) -> (Double -> IO Double)
foreign import ccall "math.h &sin" sin_addr :: FunPtr (Double -> Double)
foreign import ccall "wrapper" wrapId :: (Double -> Double) -> IO (FunPtr (Double -> Double))
foreign import ccall "wrapper" wrapIO :: (Double -> IO Double) -> IO (FunPtr (Double -> IO Double))


