module ForeignObj( ForeignObj, module ForeignObj ) where

-- data ForeignObj -- in Prelude

-- recently renamed
newForeignObj = makeForeignObj

primitive makeForeignObj  :: Addr{-x-} -> Addr{-free-} -> IO ForeignObj
primitive writeForeignObj :: ForeignObj -> Addr -> IO ()
primitive eqForeignObj    :: ForeignObj -> ForeignObj -> Bool

instance Eq ForeignObj where (==) = eqForeignObj
