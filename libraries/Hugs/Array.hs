-----------------------------------------------------------------------------
-- Standard Library: Array operations
--
-- Suitable for use with Hugs 98
-----------------------------------------------------------------------------

module Hugs.Array ( 
    module Data.Ix,  -- export all of Ix 
    unsafeIndex, unsafeRangeSize,

    Array, array, listArray, (!), bounds, indices, elems, assocs, 
    accumArray, (//), accum, ixmap,
    unsafeArray, unsafeAt, unsafeReplace, unsafeAccum, unsafeAccumArray
    ) where

import Data.Ix
import Data.List( (\\) )
import Hugs.Prelude( unsafeIndex, unsafeRangeSize )

infixl 9  !, //

data Array a b -- Arrays are implemented as a primitive type

array          :: Ix a => (a,a) -> [(a,b)] -> Array a b
listArray      :: Ix a => (a,a) -> [b] -> Array a b
(!)	       :: Ix a => Array a b -> a -> b
bounds         :: Ix a => Array a b -> (a,a)
indices        :: Ix a => Array a b -> [a]
elems          :: Ix a => Array a b -> [b]
assocs	       :: Ix a => Array a b -> [(a,b)]
(//)           :: Ix a => Array a b -> [(a,b)] -> Array a b
accum          :: Ix a => (b -> c -> b) -> Array a b -> [(a,c)] -> Array a b
accumArray     :: Ix a => (b -> c -> b) -> b -> (a,a) -> [(a,c)] -> Array a b
ixmap	       :: (Ix a, Ix b) => (a,a) -> (a -> b) -> Array b c -> Array a c

primitive primArray  :: (i,i) -> Int -> (a -> Int) -> [(a,b)] -> Array i b
primitive primUpdate :: [(a,b)] -> Array i b -> (a -> Int) -> Array i b
primitive primAccum
    :: [(a,c)] -> Array i b -> (b -> c -> b) -> (a -> Int) -> Array i b
primitive primAccumArray
    :: (i,i) -> Int -> (b -> c -> b) -> b -> (a -> Int) -> [(a,c)] -> Array i b
primitive primSubscript
    :: ((i,i) -> a -> Int) -> Array i b -> a -> b

primitive primBounds :: Array a b -> (a,a)
primitive primElems  :: Array a b -> [b]
primitive primAmap   :: (b -> c) -> Array a b -> Array a c

unsafeArray :: Ix i => (i,i) -> [(Int, e)] -> Array i e
unsafeArray bnds	= primArray bnds (rangeSize bnds) id

unsafeAt :: Ix i => Array i e -> Int -> e
unsafeAt		= primSubscript (const id)

unsafeReplace :: Ix i => Array i e -> [(Int, e)] -> Array i e
unsafeReplace iarr ies	= primUpdate ies iarr id

unsafeAccum :: Ix i => (e -> a -> e) -> Array i e -> [(Int, a)] -> Array i e
unsafeAccum f iarr ies	= primAccum ies iarr f id

unsafeAccumArray :: Ix i => (e -> a -> e) -> e -> (i,i) -> [(Int, a)] -> Array i e
unsafeAccumArray f z bnds = primAccumArray bnds (rangeSize bnds) f z id

array bnds ies      = unsafeArray bnds [(index bnds i,e) | (i,e) <- ies]
listArray bnds vs   = unsafeArray bnds (zip [0..] vs)
arr!i               = unsafeAt arr (index (bounds arr) i)
bounds              = primBounds
indices	            = range . bounds
elems               = primElems
assocs a            = zip (indices a) (elems a)
accumArray f z bnds = primAccumArray bnds (rangeSize bnds) f z (index bnds)
a // as             = unsafeReplace a [(index bnds i,e) | (i,e) <- as]
		      where bnds = bounds a
accum f a as        = unsafeAccum f a [(index bnds i,e) | (i,e) <- as]
		      where bnds = bounds a
ixmap bnds f a      = array bnds [ (i, a ! f i) | i <- range bnds ]

instance (Ix a) => Functor (Array a) where
    fmap = primAmap

instance (Ix a, Eq b) => Eq (Array a b) where
    a == a'   =   assocs a == assocs a'

instance (Ix a, Ord b) => Ord (Array a b) where
    a <= a'   =   assocs a <= assocs a'

instance  (Ix a, Show a, Show b) => Show (Array a b)  where
    showsPrec p a = showParen (p > 9) (
		    showString "array " .
		    shows (bounds a) . showChar ' ' .
		    shows (assocs a)                  )

instance  (Ix a, Read a, Read b) => Read (Array a b)  where
    readsPrec p = readParen (p > 9)
	     (\r -> [(array b as, u) | ("array",s) <- lex r,
				       (b,t)       <- reads s,
				       (as,u)      <- reads t   ])

-----------------------------------------------------------------------------
