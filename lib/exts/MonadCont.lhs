\h1{MonadCont}

\begin{code}
module MonadCont (
	MonadCont(..),
	Cont(..),
	runCont,
	mapCont,
	withCont,
	ContT(..),
	runContT,
	mapContT,
	withContT,
	module Monad,
	module MonadTrans,
	) where
\end{code}

\begin{code}
import Monad
import MonadTrans
import MonadReader
import MonadWriter
import MonadState
import MonadRWS
\end{code}

\begin{code}
class (Monad m) => MonadCont m where
	callCC :: ((a -> m b) -> m a) -> m a
\end{code}

\h2{Our parameterizable continuation monad}

\begin{code}
newtype Cont r a = Cont { runCont :: (a -> r) -> r }
\end{code}

\begin{code}
instance Functor (Cont r) where
	fmap f m = Cont $ \c -> runCont m (c . f)

instance Monad (Cont r) where
	return a = Cont ($ a)
	m >>= k  = Cont $ \c -> runCont m $ \a -> runCont (k a) c

instance MonadCont (Cont r) where
	callCC f = Cont $ \c -> runCont (f (\a -> Cont $ \_ -> c a)) c
\end{code}

\begin{code}
mapCont :: (r -> r) -> Cont r a -> Cont r a
mapCont f m = Cont $ f . runCont m

withCont :: ((b -> r) -> (a -> r)) -> Cont r a -> Cont r b
withCont f m = Cont $ runCont m . f
\end{code}

\h2{Our parameterizable continuation monad, with an inner monad}

\begin{code}
newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }
\end{code}

\begin{code}
instance (Monad m) => Functor (ContT r m) where
	fmap f m = ContT $ \c -> runContT m (c . f)

instance (Monad m) => Monad (ContT r m) where
	return a = ContT ($ a)
	m >>= k  = ContT $ \c -> runContT m (\a -> runContT (k a) c)

instance (Monad m) => MonadCont (ContT r m) where
	callCC f = ContT $ \c -> runContT (f (\a -> ContT $ \_ -> c a)) c

instance MonadTrans (ContT r) where
	lift m = ContT (m >>=)

instance (MonadIO m) => MonadIO (ContT r m) where
	liftIO = lift . liftIO

instance (MonadReader r' m) => MonadReader r' (ContT r m) where
	ask       = lift ask
	local f m = ContT $ \c -> do
		r <- ask
		local f (runContT m (local (const r) . c))

instance (MonadState s m) => MonadState s (ContT r m) where
	get = lift get
	put = lift . put
\end{code}

\h2{MonadCont instances for other monad transformers}

\begin{code}
instance (MonadCont m) => MonadCont (ReaderT r m) where
	callCC f = ReaderT $ \r ->
		callCC $ \c ->
		runReaderT (f (\a -> ReaderT $ \_ -> c a)) r

instance (MonadCont m) => MonadCont (StateT s m) where
	callCC f = StateT $ \s ->
		callCC $ \c ->
		runStateT (f (\a -> StateT $ \s' -> c (a, s'))) s

instance (Monoid w, MonadCont m) => MonadCont (WriterT w m) where
	callCC f = WriterT $
		callCC $ \c ->
		runWriterT (f (\a -> WriterT $ c (a, mempty)))

instance (Monoid w, MonadCont m) => MonadCont (RWST r w s m) where
	callCC f = RWST $ \r s ->
		callCC $ \c ->
		runRWST (f (\a -> RWST $ \_ s' -> c (a, s', mempty))) r s
\end{code}

\begin{code}
mapContT :: (m r -> m r) -> ContT r m a -> ContT r m a
mapContT f m = ContT $ f . runContT m

withContT :: ((b -> m r) -> (a -> m r)) -> ContT r m a -> ContT r m b
withContT f m = ContT $ runContT m . f
\end{code}
