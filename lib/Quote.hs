module Quote where

class Quote a where
    quote :: a -> String

instance Quote String where
    quote = id

instance Quote Bool where
    quote = show

instance Show a => Quote (Maybe a) where
    quote = show

instance Quote Int where
    quote = show

instance Quote Integer where
    quote = show

instance Quote Float where
    quote = show

instance Quote Double where
    quote = show
