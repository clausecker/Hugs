-----------------------------------------------------------------------------
-- Lazy State Thread module
-- 
-- This library provides support for both lazy and strict state threads,
-- as described in the PLDI '94 paper by John Launchbury and Simon Peyton
-- Jones.  In addition to the monad ST, it also provides mutable variables
-- STRef and mutable arrays STArray.  It is identical to the ST module
-- except that the ST instance is lazy.
--
-- Suitable for use with Hugs 98.
-----------------------------------------------------------------------------

module LazyST 
	( ST
	, runST
	, thenLazyST, thenStrictST, returnST
	, unsafeInterleaveST
	, fixST 
	, stToIO
	, unsafeIOtoST

	, STRef
	  -- instance Eq (STRef s a)
	, newSTRef
	, readSTRef
	, writeSTRef 

        , STArray
          -- instance Eq (STArray s ix elt)
        , newSTArray
        , boundsSTArray
        , readSTArray
        , writeSTArray
        , thawSTArray
        , freezeSTArray
        , unsafeFreezeSTArray
        , Ix
	) where

import Array(Array,Ix(index),bounds,assocs)
import IOExts(unsafePerformIO)
import Monad   

-----------------------------------------------------------------------------

data ST s a      -- implemented as an internal primitive

primitive runST                        :: (forall s. ST s a) -> a
primitive returnST     "STReturn"      :: a -> ST s a
primitive thenLazyST   "STLazyBind"    :: ST s a -> (a -> ST s b) -> ST s b
primitive thenStrictST "STStrictBind"  :: ST s a -> (a -> ST s b) -> ST s b
primitive unsafeInterleaveST "STInter" :: ST s a -> ST s a
primitive fixST        "STFix"         :: (a -> ST s a) -> ST s a

primitive stToIO	"primSTtoIO"   :: ST s a -> IO a

unsafeIOtoST        :: IO a -> ST s a
unsafeIOtoST         = returnST . unsafePerformIO

instance Functor (ST s) where
    fmap = liftM

instance Monad (ST s) where
    (>>=)  = thenLazyST
    return = returnST

-----------------------------------------------------------------------------

data STRef s a   -- implemented as an internal primitive

primitive newSTRef   "STNew"      :: a -> ST s (STRef s a)
primitive readSTRef  "STDeref"    :: STRef s a -> ST s a
primitive writeSTRef "STAssign"   :: STRef s a -> a -> ST s ()
primitive eqSTRef    "STMutVarEq" :: STRef s a -> STRef s a -> Bool

instance Eq (STRef s a) where (==) = eqSTRef

-----------------------------------------------------------------------------

data STArray s ix elt -- implemented as an internal primitive

newSTArray          :: Ix ix => (ix,ix) -> elt -> ST s (STArray s ix elt)
boundsSTArray       :: Ix ix => STArray s ix elt -> (ix, ix)
readSTArray         :: Ix ix => STArray s ix elt -> ix -> ST s elt
writeSTArray        :: Ix ix => STArray s ix elt -> ix -> elt -> ST s ()
thawSTArray         :: Ix ix => Array ix elt -> ST s (STArray s ix elt)
freezeSTArray       :: Ix ix => STArray s ix elt -> ST s (Array ix elt)
unsafeFreezeSTArray :: Ix ix => STArray s ix elt -> ST s (Array ix elt)

newSTArray bs e      = primNewArr bs (rangeSize bs) e
boundsSTArray a      = primBounds a
readSTArray a i      = primReadArr index a i
writeSTArray a i e   = primWriteArr index a i e
thawSTArray arr      = newSTArray (bounds arr) err `thenStrictST` \ stArr ->
		       let 
                         fillin [] = returnST stArr
                         fillin ((ix,v):ixvs) = writeSTArray stArr ix v
                          `thenStrictST` \ _ -> fillin ixvs
		       in fillin (assocs arr)
 where
  err = error "thawArray: element not overwritten" -- shouldnae happen
freezeSTArray a      = primFreeze a
unsafeFreezeSTArray  = freezeSTArray  -- not as fast as GHC

instance Eq (STArray s ix elt) where
  (==) = eqSTArray

primitive primNewArr   "STNewArr"
          :: (a,a) -> Int -> b -> ST s (STArray s a b)
primitive primReadArr  "STReadArr"
          :: ((a,a) -> a -> Int) -> STArray s a b -> a -> ST s b
primitive primWriteArr "STWriteArr"
          :: ((a,a) -> a -> Int) -> STArray s a b -> a -> b -> ST s ()
primitive primFreeze   "STFreeze"
          :: STArray s a b -> ST s (Array a b)
primitive primBounds   "STBounds"
          :: STArray s a b -> (a,a)
primitive eqSTArray    "STArrEq"
          :: STArray s a b -> STArray s a b -> Bool

-----------------------------------------------------------------------------
