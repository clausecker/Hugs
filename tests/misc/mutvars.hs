--!!! Testing Mutvars

-- Note: equivalent code of the form: show (runST (newVar 'a' ...))
-- won't typecheck under Hugs 1.01.

a1 = show (runST prog)
 where
  prog :: ST s Char
  prog =
	newVar 'a'	>>= \ v ->
	readVar v

a2 = show (runST prog)
 where
  prog :: ST s Char
  prog =
	newVar 'a'		>>= \ v ->
	writeVar v 'b'		>>
	readVar v

a3 = show (runST prog)
 where
  prog :: ST s (Bool,Bool,Bool)
  prog =
	newVar 'a'		>>= \ v1 ->
	newVar 'a'		>>= \ v2 ->
	return (v1 == v1, v1 == v2, v2 == v2)

