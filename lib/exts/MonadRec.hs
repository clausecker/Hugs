-----------------------------------------------------------------------------
-- MonadRec Library
--
-- Suitable for use with the mdo extension of Hugs 98
-----------------------------------------------------------------------------

module MonadRec (MonadRec(mfix)) where
    
import qualified LazyST
import qualified ST
import IOExts

fix :: (a -> a) -> a
fix f = let a = f a in a

-- The MonadRec class definition

class Monad m => MonadRec m where
    mfix    :: (a -> m a) -> m a 

-- Instances of MonadRec

-- Maybe:
instance MonadRec Maybe where
    mfix f = let a = f (unJust a) in a
             where unJust (Just x) = x

-- List:
instance MonadRec [] where
    mfix f = case fix (f . head) of
               []    -> []
               (x:_) -> x : mfix (tail . f)

-- IO:
instance MonadRec IO where
    mfix = fixIO 

-- Lazy State:
instance MonadRec (LazyST.ST s) where
    mfix = LazyST.fixST
    
-- Strict State:
instance MonadRec (ST.ST s) where
    mfix = ST.fixST
