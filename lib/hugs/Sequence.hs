module Sequence(
	Sequence( fromList, toList ),
	-- instances for [], Maybe and List
	List -- same instances as for []
	) where

import Maybe(maybeToList, listToMaybe)
import Monad

class (Functor m, MonadPlus m) => Sequence m where
  fromList :: [a] -> m a
  toList   :: m a -> [a]

  fromList = msum . fmap return

----------------------------------------------------------------
-- []: fast access to head but slow append
----------------------------------------------------------------

instance Sequence [] where
  fromList = id
  toList   = id

----------------------------------------------------------------
-- Maybe: single element lists - sort of
----------------------------------------------------------------

instance Sequence Maybe where
  toList   = maybeToList
  fromList = listToMaybe

----------------------------------------------------------------
-- List: lists with fast append (but slower indexing!)
----------------------------------------------------------------

-- Instead of providing Cons as a constructor, we provide Append.

data List a = Empty
             | Singleton a
             | Append (List a) (List a)

-- We define all the same instances that are defined for [].

-- The following definitions are independent of the choice of
-- representation since there's very little benefit in writing
-- representation-dependent versions.

instance Eq a => Eq (List a) where
  xs == ys = toList xs == toList ys

instance Ord a => Ord (List a) where
  compare xs ys = compare (toList xs) (toList ys)

instance Read a => Read (List a) where
  readsPrec p s = [ (fromList xs, r)      | (xs,  r) <- readsPrec p s ]
  readList    s = [ (map fromList xss, r) | (xss, r) <- readList    s ] 

instance Show a => Show (List a) where
  showsPrec p xs  = showsPrec p (toList xs)
  showList    xss = showList    (map toList xss)

-- The following operations are representation dependent and ought
-- to go much faster than any alternative way of writing them.
--
-- For example, the monadic operators preserve the structure of their
-- input.

instance Functor List where
  fmap f Empty          = Empty
  fmap f (Singleton x)  = Singleton (f x)
  fmap f (Append xs ys) = Append (fmap f xs) (fmap f ys)

instance Monad List where
  Empty        >>= k = Empty
  Singleton x  >>= k = k x
  Append xs ys >>= k = Append (xs >>= k) (ys >>= k)

  return = Singleton

instance MonadPlus List where
  mzero = Empty
  mplus = Append

instance Sequence List where
  fromList = foldr (\ x xs -> Singleton x `Append` xs) Empty

  toList xs = flatten xs []
   where
    -- flatten uses the standard technique of a "work list" yss
    -- flatten xs yss = xs ++ concatMap toList yss
    -- flatten :: List a -> [List a] -> [a]
    flatten Empty          []       = []
    flatten Empty          (ys:yss) = flatten ys yss
    flatten (Singleton x)  []       = [x]
    flatten (Singleton x)  (ys:yss) = x : flatten ys yss

    -- special cases for extra speed
    flatten (Append (Singleton x) ys) yss = x:flatten ys yss
    flatten (Append xs Empty) yss   = flatten xs yss
    flatten (Append Empty ys) yss   = flatten ys yss

    flatten (Append xs ys) yss      = flatten xs (ys:yss)
