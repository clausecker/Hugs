-----------------------------------------------------------------------------
-- Trace primitive: import this library as a simple way to access the
-- impure trace primitive.  This is sometimes useful for debugging,
-- although understanding the output that it produces can sometimes be
-- a major challenge unless you are familiar with the intimate details
-- of how programs are executed.
--
-- Suitable for use with Hugs 98
-----------------------------------------------------------------------------

module Trace( trace, traceShow ) where

primitive trace :: String -> a -> a

traceShow :: Show a => String -> a -> a
traceShow msg x = trace (msg ++ show x) x

-----------------------------------------------------------------------------
