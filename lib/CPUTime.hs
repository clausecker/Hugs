--
-- Implementation of Haskell 98's CPUTime module
-- 
-- Hugs98 specific
--
module CPUTime
	( getCPUTime		-- :: IO Integer
	, cpuTimePrecision      -- :: Integer
	) where

import Ratio((%))

cpuTimePrecision :: Integer
cpuTimePrecision = round (picoSec % fromIntegral clockTicks)

picoSec :: Integer
picoSec = 1000000000000 -- 10^12

getCPUTime :: IO Integer
getCPUTime = do
   (usec, unsec, ssec, snsec) <- getCPUUsage
   return (picoSec * fromIntegral usec  +
   	   1000    * fromIntegral unsec + 
	   picoSec * fromIntegral ssec  + 
	   1000    * fromIntegral snsec)
	   
primitive getCPUUsage   :: IO (Int,Int,Int,Int)
primitive clockTicks   :: Int
