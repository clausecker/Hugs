-- !!! Testing Haskell callbacks (adapted from HOpenGL stuff)

module Main where

import Foreign

-------------------------------------------------------------------------------

type DisplayCallback = IO ()

foreign import ccall "wrapper" makeDisplayCallback ::
   DisplayCallback -> IO (FunPtr DisplayCallback)

foreign import ccall unsafe "callback.h registerDisplayCB" registerDisplayCB ::
   FunPtr DisplayCallback -> IO ()

foreign import ccall safe "callback.h invokeDisplayCB" invokeDisplayCB ::
   IO ()

display :: DisplayCallback
display = putStrLn "Hello, I'm the display callback!"

-------------------------------------------------------------------------------

main :: IO ()
main = do
   displayFunPtr <- makeDisplayCallback display
   registerDisplayCB displayFunPtr
   invokeDisplayCB
   invokeDisplayCB
   freeHaskellFunPtr displayFunPtr
