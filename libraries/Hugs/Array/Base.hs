-- A replacement for Data.Array.Base, for use with Hugs
-- (Haskell 98 + multi-parameter type classes)

module Hugs.Array.Base
	( HasBounds(bounds)
	, indices

	, IArray(..)

	, listArray	-- :: (IArray a e, Ix i) => (i,i) -> [e] -> a i e
	, ixmap		-- :: (IArray a e, Ix i, Ix j) => (i,i) -> (i -> j) -> a j e -> a i e
	, elems		-- :: (IArray a e, Ix i) => a i e -> [e]
	, assocs	-- :: (IArray a e, Ix i) => a i e -> [(i, e)]
	, amap		-- :: (IArray a e', IArray a e, Ix i) => (e' -> e) -> a i e' -> a i e

	, MArray(..)

	, newListArray	-- :: (MArray a e m, Ix i) => (i,i) -> [e] -> m (a i e)
	, getElems	-- :: (MArray a e m, Ix i) => a i e -> m [e]
	, getAssocs	-- :: (MArray a e m, Ix i) => a i e -> m [(i, e)]
	, mapArray	-- :: (MArray a e' m, MArray a e m, Ix i) => (e' -> e) -> a i e' -> m (a i e)
	, mapIndices	-- :: (MArray a e m, Ix i, Ix j) => (i,i) -> (i -> j) -> a j e -> m (a i e)

	, freeze	-- :: (Ix i, MArray a e m, IArray b e) => a i e -> m (b i e)
	, unsafeFreeze	-- :: (Ix i, MArray a e m, IArray b e) => a i e -> m (b i e)
	, thaw		-- :: (Ix i, IArray a e, MArray b e m) => a i e -> m (b i e)
	, unsafeThaw	-- :: (Ix i, IArray a e, MArray b e m) => a i e -> m (b i e)
	) where

import Data.Ix
import Hugs.Array (Array)
import qualified Hugs.Array as Array
import Hugs.IOArray

class HasBounds a where
	bounds :: Ix i => a i e -> (i,i)

indices :: (HasBounds a, Ix i) => a i e -> [i]
indices = range . bounds

-- Immutable arrays

class HasBounds a => IArray a e where
	array	   :: Ix i => (i,i) -> [(i, e)] -> a i e
	(!)	   :: Ix i => a i e -> i -> e
	(//)	   :: Ix i => a i e -> [(i, e)] -> a i e
	accum	   :: Ix i => (e -> e' -> e) -> a i e -> [(i, e')] -> a i e
	accumArray :: Ix i => (e -> e' -> e) -> e -> (i,i) -> [(i, e')] -> a i e

-- Array instances
-- (has to be here, so that Data.Array is Haskell 98)

instance HasBounds Array where
	bounds = Array.bounds

instance IArray Array e where
	array = Array.array
	(!) = (Array.!)
	(//) = (Array.//)
	accum = Array.accum
	accumArray = Array.accumArray

-- These definitions from the Prelude generalize to IArray

listArray :: (IArray a e, Ix i) => (i,i) -> [e] -> a i e
listArray bnds vs = array bnds (zip (range bnds) vs)

ixmap :: (IArray a e, Ix i, Ix j) => (i,i) -> (i -> j) -> a j e -> a i e
ixmap bnds f arr = array bnds [ (i, arr ! f i) | i <- range bnds ]

elems :: (IArray a e, Ix i) => a i e -> [e]
elems arr = [arr!i | i <- indices arr]

assocs :: (IArray a e, Ix i) => a i e -> [(i, e)]
assocs arr = [(i, arr!i) | i <- indices arr]

-- Inefficient, but general

amap :: (IArray a e', IArray a e, Ix i) => (e' -> e) -> a i e' -> a i e
amap f arr = array (bounds arr) [(i, f (arr!i)) | i <- indices arr]

-- Mutable arrays

class (HasBounds a, Monad m) => MArray a e m where
	newArray :: Ix i => (i,i) -> e -> m (a i e)

	newArray_ :: Ix i => (i,i) -> m (a i e)
	newArray_ (l,u) = newArray (l,u) arrEleBottom

	readArray :: Ix i => a i e -> i -> m e
	writeArray :: Ix i => a i e -> i -> e -> m ()

-- IOArray instances
-- (has to be here, so that the IOExts compatability stub is Haskell 98)

instance HasBounds IOArray where
  bounds = boundsIOArray

instance MArray IOArray e IO where
  newArray = newIOArray
  readArray = readIOArray
  writeArray = writeIOArray

-- Miscellaneous functions

newListArray :: (MArray a e m, Ix i) => (i,i) -> [e] -> m (a i e)
newListArray bnds vs = do
    marr <- newArray_ bnds
    sequence_ [writeArray marr i v | (i,v) <- zip (range bnds) vs]
    return marr

-- Converting between mutable and immutable arrays (freezing and thawing)
-- Hugs has built-in implementations of freeze from IO or ST to Array,
-- but there's no way to use them with the new classes.
-- Hugs doesn't have the unsafe versions.

freeze :: (Ix i, MArray a e m, IArray b e) => a i e -> m (b i e)
freeze marr = do
    ies <- getAssocs marr
    return (array (bounds marr) ies)

unsafeFreeze :: (Ix i, MArray a e m, IArray b e) => a i e -> m (b i e)
unsafeFreeze = freeze

thaw :: (Ix i, IArray a e, MArray b e m) => a i e -> m (b i e)
thaw iarr = newListArray (bounds iarr) (elems iarr)

unsafeThaw :: (Ix i, IArray a e, MArray b e m) => a i e -> m (b i e)
unsafeThaw = thaw

-----------------------------------------------------------------------------
-- General stuff (stolen from Data.Array.Base)

arrEleBottom :: a
arrEleBottom = error "MArray: undefined array element"

-- | Return a list of all the elements of a mutable array
getElems :: (MArray a e m, Ix i) => a i e -> m [e]
getElems marr =
    sequence [readArray marr i | i <- indices marr]

-- | Return a list of all the associations of a mutable array, in
-- index order.
getAssocs :: (MArray a e m, Ix i) => a i e -> m [(i, e)]
getAssocs marr =
    sequence [do {e <- readArray marr i; return (i,e)} | i <- indices marr]

-- | Constructs a new array derived from the original array by applying a
-- function to each of the elements.
mapArray :: (MArray a e' m, MArray a e m, Ix i) => (e' -> e) -> a i e' -> m (a i e)
mapArray f marr = do
    let (l,u) = bounds marr
    marr' <- newArray_ (l,u)
    sequence_ [do
        e <- readArray marr i
        writeArray marr' i (f e)
        | i <- range (l,u)]
    return marr'

-- | Constructs a new array derived from the original array by applying a
-- function to each of the indices.
mapIndices :: (MArray a e m, Ix i, Ix j) => (i,i) -> (i -> j) -> a j e -> m (a i e)
mapIndices (l,u) f marr = do
    marr' <- newArray_ (l,u)
    sequence_ [do
        e <- readArray marr (f i)
        writeArray marr' i e
        | i <- range (l,u)]
    return marr'
