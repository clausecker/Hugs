-----------------------------------------------------------------------------
-- Weak.hs:	Weak Pointers
--
-- This library provides support for weak pointers.
--
-- Suitable for use with Hugs 98
-----------------------------------------------------------------------------

module Weak(Weak, makeWeakPtr, derefWeakPtr) where

data Weak a

primitive makeWeakPtr  :: a -> IO (Weak a)
primitive derefWeakPtr :: Weak a -> IO (Maybe a)

{-
-- for testing purposes 
primitive gc "primGC" :: IO ()

-- not a CAF!
test z = do
  { let x = [z]  -- use a list so we're sure it's heap allocated
  ; print x -- this makes sure x is in whnf
  ; w <- makeWeakPtr x
  ; showWeakPtr w
  ; gc
  ; print x -- this makes sure x is still alive after the GC
  ; showWeakPtr w  -- so it's probably still alive here
  ; gc
  ; showWeakPtr w  -- but ought to be dead by here
  }

showWeakPtr :: Show a => Weak a -> IO ()
showWeakPtr w = do
  { x <- derefWeakPtr w
  ; print x
  }
-}

-----------------------------------------------------------------------------
