module Hugs.StablePtr( StablePtr, module Hugs.StablePtr ) where

-- data StablePtr a -- in Prelude

-- recently renamed
newStablePtr = makeStablePtr 

primitive makeStablePtr      :: a -> IO (StablePtr a)
primitive deRefStablePtr     :: StablePtr a -> IO a
primitive freeStablePtr      :: StablePtr a -> IO ()
primitive castStablePtrToPtr :: StablePtr a -> Ptr ()
primitive castPtrToStablePtr :: Ptr () -> StablePtr a

