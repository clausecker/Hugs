-- !!! Testing dynamic import and wrappers (esp. for Sparcs)

-- Test contributed by Sven Panne
module Main where

import Foreign
import Random

-------------------------------------------------------------------------------

foreign import ccall "dynamic" callFun5I :: FunPtr (Int -> Int -> Int -> Int -> Int -> Int) -> (Int -> Int -> Int -> Int -> Int -> Int)
foreign import ccall "wrapper" mkFun5I   :: (Int -> Int -> Int -> Int -> Int -> Int) -> IO (FunPtr (Int -> Int -> Int -> Int -> Int -> Int))

manyArgs5I :: (Int -> Int -> Int -> Int -> Int -> Int)
manyArgs5I a1 a2 a3 a4 a5 = (((a1 * 31 + a2) * 31 + a3) * 31 + a4) * 31 + a5

test5I :: IO ()
test5I = do
  a1 <- randomIO
  a2 <- randomIO
  a3 <- randomIO
  a4 <- randomIO
  a5 <- randomIO
  funAddr <- mkFun5I manyArgs5I
  print (callFun5I funAddr a1 a2 a3 a4 a5 ==
         manyArgs5I        a1 a2 a3 a4 a5)
  freeHaskellFunPtr funAddr

-------------------------------------------------------------------------------

foreign import ccall "dynamic" callFun6D :: FunPtr (Double -> Double -> Double -> Double -> Double -> Double -> Double) -> (Double -> Double -> Double -> Double -> Double -> Double -> Double)
foreign import ccall "wrapper" mkFun6D   :: (Double -> Double -> Double -> Double -> Double -> Double -> Double) -> IO (FunPtr (Double -> Double -> Double -> Double -> Double -> Double -> Double))

manyArgs6D :: Double -> Double -> Double -> Double -> Double -> Double -> Double
manyArgs6D a1 a2 a3 a4 a5 a6 =
   ((((a1 * 31 + a2) * 31 + a3) * 31 + a4) * 31 + a5) * 31 + a6

test6D :: IO ()
test6D = do
  a1 <- randomIO
  a2 <- randomIO
  a3 <- randomIO
  a4 <- randomIO
  a5 <- randomIO
  a6 <- randomIO
  funAddr <- mkFun6D manyArgs6D
  print (callFun6D funAddr a1 a2 a3 a4 a5 a6 ==
         manyArgs6D        a1 a2 a3 a4 a5 a6)
  freeHaskellFunPtr funAddr

-------------------------------------------------------------------------------

foreign import ccall "dynamic" callFun11M :: FunPtr (Int -> Double -> Float -> Char -> Int -> Int -> Float -> Int -> Char -> Double -> Int -> Double) -> (Int -> Double -> Float -> Char -> Int -> Int -> Float -> Int -> Char -> Double -> Int -> Double)
foreign import ccall "wrapper" mkFun11M   :: (Int -> Double -> Float -> Char -> Int -> Int -> Float -> Int -> Char -> Double -> Int -> Double) -> IO (FunPtr (Int -> Double -> Float -> Char -> Int -> Int -> Float -> Int -> Char -> Double -> Int -> Double))

manyArgs11M :: Int -> Double -> Float -> Char -> Int -> Int -> Float -> Int -> Char -> Double -> Int -> Double
manyArgs11M a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 =
   (((((((((fromIntegral           a1   * 31 +                         a2)  * 31 +
            realToFrac             a3)  * 31 + fromIntegral (fromEnum  a4)) * 31 +
            fromIntegral           a5)  * 31 + fromIntegral            a6)  * 31 +
            realToFrac             a7)  * 31 + fromIntegral            a8)  * 31 +
            fromIntegral (fromEnum a9)) * 31 +                        a10)  * 31 +
            fromIntegral a11

test11M :: IO ()
test11M = do
  a1  <- randomIO
  a2  <- randomIO
  a3  <- randomIO
  a4  <- randomIO
  a5  <- randomIO
  a6  <- randomIO
  a7  <- randomIO
  a8  <- randomIO
  a9  <- randomIO
  a10 <- randomIO
  a11 <- randomIO
  funAddr <- mkFun11M manyArgs11M
  print (callFun11M funAddr a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 ==
         manyArgs11M        a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11)
  freeHaskellFunPtr funAddr

-------------------------------------------------------------------------------

rep :: String -> IO () -> IO ()
rep msg tst = do
   putStrLn ("Testing " ++ msg ++ "...")
   sequence_ (replicate 10 tst)

main :: IO ()
main = do
  setStdGen (mkStdGen 4711)
  rep "5 Int arguments" test5I
  rep "6 Double arguments" test6D
  rep "11 mixed arguments" test11M


