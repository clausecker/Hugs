\section[IORef]{Mutable Variables}

\begin{code}
module IORef
	( IORef		      -- abstract, instance of: Eq
        , newIORef	      -- :: a -> IO (IORef a)
        , readIORef	      -- :: IORef a -> IO a
        , writeIORef	      -- :: IORef a -> a -> IO ()
	, modifyIORef	      -- :: IORef a -> (a -> a) -> IO ()
	, updateIORef	      -- deprecated, use modifyIORef


	) where


\end{code}

\begin{code}



data IORef a        -- mutable variables containing values of type a

primitive newIORef   "newRef" :: a -> IO (IORef a)
primitive readIORef  "getRef" :: IORef a -> IO a
primitive writeIORef "setRef" :: IORef a -> a -> IO ()
primitive eqIORef    "eqRef"  :: IORef a -> IORef a -> Bool

instance Eq (IORef a) where
    (==) = eqIORef

modifyIORef :: IORef a -> (a -> a) -> IO ()
modifyIORef ref f = writeIORef ref . f =<< readIORef ref

-- deprecated, use modifyIORef
updateIORef :: IORef a -> (a -> a) -> IO ()
updateIORef = modifyIORef

\end{code}
