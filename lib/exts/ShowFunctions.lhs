%
% (c) The GHC Team, 2000
%

\begin{code}
module ShowFunctions where

instance Show (a -> b) where
	showsPrec _ _ = showString "<function>"
\end{code}
