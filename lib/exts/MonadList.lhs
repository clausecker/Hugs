\h1{MonadList}

\begin{code}
module MonadList (
	ListT(..),
	runListT,
	mapListT,
	module Monad,
	module MonadTrans,
	) where
\end{code}

\begin{code}
import Monad
import MonadTrans
import MonadReader
import MonadState
import MonadCont
import MonadError
\end{code}

\h2{Our parameterizable list monad, with an inner monad}

\begin{code}
newtype ListT m a = ListT { runListT :: m [a] }
\end{code}

\begin{code}
instance (Monad m) => Functor (ListT m) where
	fmap f m = ListT $ do
		a <- runListT m
		return (map f a)

instance (Monad m) => Monad (ListT m) where
	return a = ListT $ return [a]
	m >>= k  = ListT $ do
		a <- runListT m
		b <- mapM (runListT . k) a
		return (concat b)
	fail _ = ListT $ return []

instance (Monad m) => MonadPlus (ListT m) where
	mzero       = ListT $ return []
	m `mplus` n = ListT $ do
		a <- runListT m
		b <- runListT n
		return (a ++ b)

instance MonadTrans ListT where
	lift m = ListT $ do
		a <- m
		return [a]

instance (MonadIO m) => MonadIO (ListT m) where
	liftIO = lift . liftIO

instance (MonadReader s m) => MonadReader s (ListT m) where
	ask       = lift ask
	local f m = ListT $ local f (runListT m)

instance (MonadState s m) => MonadState s (ListT m) where
	get = lift get
	put = lift . put

instance (MonadCont m) => MonadCont (ListT m) where
	callCC f = ListT $
		callCC $ \c ->
		runListT (f (\a -> ListT $ c [a]))

instance (MonadError e m) => MonadError e (ListT m) where
	throwError       = lift . throwError
	m `catchError` h = ListT $ runListT m
		`catchError` \e -> runListT (h e)
\end{code}

\begin{code}
mapListT :: (m [a] -> n [b]) -> ListT m a -> ListT n b
mapListT f m = ListT $ f (runListT m)
\end{code}
