--!!! Testing monad comprehensions
module MonadTest where
import Monad

-- Old uses of list comprehensions
as :: [Bool]
as = [ odd x | x <- [1..10] ]

-- Use in monad comprehensions
mmap :: Monad m => (a -> b) -> (m a -> m b)
mmap f xs = [ f x | x <- xs ]

-- use ","
bind :: Monad m => m a -> (a -> m b) -> m b
bind m k = [ b | a <- m, b <- k a ]

bind2 :: MonadPlus m => m Int -> (Int -> m b) -> m b
bind2 m k = [ b | a <- m, odd a, b <- k a ]

-- use local binding
bind3 :: Monad m => m a -> (a -> b) -> (b -> m c) -> m c
bind3 m f k = [ c | a <- m, let b = f a, c <- k b ]
