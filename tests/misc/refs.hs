--!!! Testing Refs
a1 = 
	newRef 'a'	>>= \ v ->
	derefRef v	>>= \ x ->
	print x

a2 = 
	newRef 'a'		>>= \ v ->
	assignRef v 'b'		>>
	derefRef v		>>= \ x ->
	print x

a3 = 
	newRef 'a'		>>= \ v1 ->
	newRef 'a'		>>= \ v2 ->
	print (v1 == v1, v1 == v2, v2 == v2)


