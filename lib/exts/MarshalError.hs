module MarshalError (
  throwIf,       -- :: (a -> Bool) -> (a -> String) -> IO a       -> IO a
  throwIf_,      -- :: (a -> Bool) -> (a -> String) -> IO a       -> IO ()
  throwIfNeg,    -- :: (Ord a, Num a) 
	         -- =>                (a -> String) -> IO a       -> IO a
  throwIfNeg_,   -- :: (Ord a, Num a)
	         -- =>                (a -> String) -> IO a       -> IO ()
  throwIfNull,   -- ::                String        -> IO (Ptr a) -> IO (Ptr a)
  void           -- IO a -> IO ()
) where

import Ptr

throwIf                 :: (a -> Bool) -> (a -> String) -> IO a -> IO a
throwIf pred msgfct act  = 
  do
    res <- act
    (if pred res then ioError . userError . msgfct else return) res

throwIf_                 :: (a -> Bool) -> (a -> String) -> IO a -> IO ()
throwIf_ pred msgfct act  = void $ throwIf pred msgfct act

throwIfNeg :: (Ord a, Num a) => (a -> String) -> IO a -> IO a
throwIfNeg  = throwIf (< 0)

throwIfNeg_ :: (Ord a, Num a) => (a -> String) -> IO a -> IO ()
throwIfNeg_  = throwIf_ (< 0)

throwIfNull :: String -> IO (Ptr a) -> IO (Ptr a)
throwIfNull  = throwIf (== nullPtr) . const

void     :: IO a -> IO ()
void act  = act >> return ()
