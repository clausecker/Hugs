\h1{MonadState}
\haskell:module{
  \Name{MonadState}
  \Version{0.2}
  \License{
	The Haskell Monad Template Library is Copyright &copy;
	Andy Gill, and the Oregon Graduate Institute of Science and
	Technology, 1999, All rights reserved, and is distributed as
	free software under the license in the file "License", which
	is included in the distribution.}
  \Author{
	Rendered by \A[HREF="http://www.cse.ogi.edu/~andy"]{Andy Gill},
	inspired by the paper
	\em{Functional Programming with Overloading and
	    Higher-Order Polymorphism},
	  \A[HREF="http://www.cse.ogi.edu/~mpj"]{Mark P Jones},
		Advanced School of Functional Programming, 1995.}
  \Restrictions{
	This requires multi parameter classes and fundeps.
	}
  \Tested{Hugs98, GHC 4.03}
}

\begin{code}
module MonadState (
	MonadState(..),
	modify,
	gets,
	State(..),
	runState,
	evalState,
	execState,
	mapState,
	withState,
	StateT(..),
	runStateT,
	evalStateT,
	execStateT,
	mapStateT,
	withStateT,
	module Monad,
	module MonadFix,
	module MonadTrans,
	) where
\end{code}

\begin{code}
import Monad
import MonadFix
import MonadTrans
import MonadReader
import MonadWriter
\end{code}

\haskell:class{
  \Name{MonadState}
  \get{returns the state from the internals of the monad.}
  \put{changes (replaces) the state inside the monad.}
}

\begin{code}
class (Monad m) => MonadState s m | m -> s where
	get :: m s
	put :: s -> m ()
\end{code}

\haskell:function{
  \Purpose{Monadic state transformer.}
  \Description{
      Maps an old state to a new state inside a state monad.
      The old state is thrown away.}
  \Example{
	\haskell:code[bgcolor="#ff88ff"]{
	  Main> :t modify ((+1) :: Int -> Int)
	  modify (...) :: (MonadState Int a) => a ()
	}
	{This says that modify (+1) acts over any
	Monad that is a member of the MonadState class,
	with an \haskell:expr{Int} state.}
  }
}
\begin{code}
modify :: (MonadState s m) => (s -> s) -> m ()
modify f = do
	s <- get
	put (f s)
\end{code}

\haskell:function{
  \Purpose{gets part of the state}
  \Description{
	gets specific component of the state,
	using a projection function supplied.
  }
	
}
\begin{code}
gets :: (MonadState s m) => (s -> a) -> m a
gets f = do
	s <- get
	return (f s)
\end{code}

\h2{Our parameterizable state monad}

\begin{code}
newtype State s a = State { runState :: s -> (a, s) }
\end{code}

The State Monad structure is paramterized over just the state.

\begin{code}
instance Functor (State s) where
	fmap f m = State $ \s -> let
		(a, s') = runState m s
		in (f a, s')

instance Monad (State s) where
	return a = State $ \s -> (a, s)
	m >>= k  = State $ \s -> let
		(a, s') = runState m s
		in runState (k a) s'

instance MonadFix (State s) where
	mfix f = State $ \s -> let (a, s') = runState (f a) s in (a, s')

instance MonadState s (State s) where
	get   = State $ \s -> (s, s)
	put s = State $ \_ -> ((), s)
\end{code}

\begin{code}
evalState :: State s a -> s -> a
evalState m s = fst (runState m s)

execState :: State s a -> s -> s
execState m s = snd (runState m s)

mapState :: ((a, s) -> (b, s)) -> State s a -> State s b
mapState f m = State $ f . runState m

withState :: (s -> s) -> State s a -> State s a
withState f m = State $ runState m . f
\end{code}

\h2{Our parameterizable state monad, with an inner monad}

\begin{code}
newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }
\end{code}

The StateT Monad structure is parameterized over two things:
\ul{
 \li{s - The state.}
 \li{m - The inner monad.}
}

Here are some examples of use:

\haskell:code[bgcolor="#ff88ff"]{
-- (Parser from ParseLib with Hugs)
   type Parser a = StateT String [] a
      ==> StateT (String -> [(a,String)])
-- For example, item can be written as:
	item = do (x:xs) <- get
		  put xs
		  return x

   type BoringState s a = StateT s Indentity a
      ==> StateT (s -> Identity (a,s))

   type StateWithIO s a = StateT s IO a
      ==> StateT (s -> IO (a,s))

   type StateWithErr s a = StateT s Maybe a
      ==> StateT (s -> Maybe (a,s))
}

\begin{code}
instance (Monad m) => Functor (StateT s m) where
	fmap f m = StateT $ \s -> do
		(x, s') <- runStateT m s
		return (f x, s')

instance (Monad m) => Monad (StateT s m) where
	return a = StateT $ \s -> return (a, s)
	m >>= k  = StateT $ \s -> do
		(a, s') <- runStateT m s
		runStateT (k a) s'
	fail str = StateT $ \_ -> fail str

instance (MonadPlus m) => MonadPlus (StateT s m) where
	mzero       = StateT $ \_ -> mzero
	m `mplus` n = StateT $ \s -> runStateT m s `mplus` runStateT n s

instance (MonadFix m) => MonadFix (StateT s m) where
	mfix f = StateT $ \s -> mfix $ \ ~(a, _) -> runStateT (f a) s

instance (Monad m) => MonadState s (StateT s m) where
	get   = StateT $ \s -> return (s, s)
	put s = StateT $ \_ -> return ((), s)

instance MonadTrans (StateT s) where
	lift m = StateT $ \s -> do
		a <- m
		return (a, s)

instance (MonadIO m) => MonadIO (StateT s m) where
	liftIO = lift . liftIO

instance (MonadReader r m) => MonadReader r (StateT s m) where
	ask       = lift ask
	local f m = StateT $ \s -> local f (runStateT m s)

instance (MonadWriter w m) => MonadWriter w (StateT s m) where
	tell     = lift . tell
	listen m = StateT $ \s -> do
		((a, s'), w) <- listen (runStateT m s)
		return ((a, w), s')
	pass   m = StateT $ \s -> pass $ do
		((a, f), s') <- runStateT m s
		return ((a, s'), f)
\end{code}

\begin{code}
evalStateT :: (Monad m) => StateT s m a -> s -> m a
evalStateT m s = do
	(a, _) <- runStateT m s
	return a

execStateT :: (Monad m) => StateT s m a -> s -> m s
execStateT m s = do
	(_, s') <- runStateT m s
	return s'

mapStateT :: (m (a, s) -> n (b, s)) -> StateT s m a -> StateT s n b
mapStateT f m = StateT $ f . runStateT m

withStateT :: (s -> s) -> StateT s m a -> StateT s m a
withStateT f m = StateT $ runStateT m . f
\end{code}

\h2{MonadState instances for other monad transformers}

\begin{code}
instance (MonadState s m) => MonadState s (ReaderT r m) where
	get = lift get
	put = lift . put

instance (Monoid w, MonadState s m) => MonadState s (WriterT w m) where
	get = lift get
	put = lift . put
\end{code}
