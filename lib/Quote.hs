module Quote where

class Quote a where
    quote :: a -> String

instance Quote String where
    quote = id
