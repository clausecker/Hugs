/* --------------------------------------------------------------------------
 * Haskell array primitives.
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the Oregon Graduate Institute of Science and
 * Technology, 1994-1999, All rights reserved.  It is distributed as
 * free software under the license in the file "License", which is
 * included in the distribution.
 *
 * $RCSfile: array.c,v $
 * $Revision: 1.3 $
 * $Date: 1999/09/13 11:00:59 $
 * ------------------------------------------------------------------------*/

static Name nameEltUndef;		/* undefined element in array	   */
static Name nameOutBounds;		/* value of of bounds		   */

PROTO_PRIM(primArray);
PROTO_PRIM(primUpdate);
PROTO_PRIM(primAccum);
PROTO_PRIM(primAccumArray);
PROTO_PRIM(primAmap);
PROTO_PRIM(primSubscript);
PROTO_PRIM(primBounds);
PROTO_PRIM(primElems);
#if LAZY_ST
PROTO_PRIM(primSTNewArr);
PROTO_PRIM(primSTReadArr);
PROTO_PRIM(primSTWriteArr);
PROTO_PRIM(primSTFreeze);
PROTO_PRIM(primSTArrEq);
#endif
#if IO_MONAD
PROTO_PRIM(primIONewArr);
PROTO_PRIM(primIOReadArr);
PROTO_PRIM(primIOWriteArr);
PROTO_PRIM(primIOFreeze);
PROTO_PRIM(primIOArrEq);
#endif

static struct primitive arrayPrimTable[] = {
  {"primArray",		4, primArray},
  {"primUpdate",	3, primUpdate},
  {"primAccum",		4, primAccum},
  {"primAccumArray",	6, primAccumArray},
  {"primAmap",		2, primAmap},
  {"primSubscript",	3, primSubscript},
  {"primBounds",	1, primBounds},
  {"primElems",		1, primElems},
  {"eltUndef",		0, primFail},
  {"outBounds",		2, primFail},
#if LAZY_ST
  {"STNewArr",		4, primSTNewArr},
  {"STReadArr",		4, primSTReadArr},
  {"STWriteArr",	5, primSTWriteArr},
  {"STFreeze",		2, primSTFreeze},
  {"STBounds",	        1, primBounds},
  {"STArrEq",	        2, primSTArrEq},
#endif
#if IO_MONAD
  {"IONewArr",		5, primIONewArr},
  {"IOReadArr",		5, primIOReadArr},
  {"IOWriteArr",	6, primIOWriteArr},
  {"IOFreeze",		3, primIOFreeze},
  {"IOBounds",	        1, primBounds},
  {"IOArrEq",	        2, primIOArrEq},
#endif
  {0,			0, 0}
};

/* --------------------------------------------------------------------------
 * Array control:
 * ------------------------------------------------------------------------*/

static Void arrayControl Args((Int));
static Void arrayControl(what)
Int what; {
    switch (what) {
	case INSTALL : 
		       setCurrModule(modulePrelude);
#define pFun(n,s,t)    addPrim(0,n=newName(findText(s),NIL),t,modulePrelude,NIL)
		       pFun(nameEltUndef,  "_undefined_array_element",
							"eltUndef");
		       pFun(nameOutBounds, "_out_of_bounds","outBounds");
#undef pFun
		       break;
    }
}

static struct primInfo arrayPrims = { arrayControl, arrayPrimTable, 0 };

/*-------------------------------------------------------------------------*/

/* The implementation of arrays is heavily parameterized to allow the
 * use of different implementations.  Non-conservative GC is also an
 * important goal, which is also why so much of this was originally done
 * using macros rather than procedures.  As it happens, this probably could
 * have been avoided, but there don't seem to be sufficiently good reasons
 * to warrant changing it.
 *
 * The result, however, is a torture-test for the C preprocessor!
 *
 * A description of the various `parameters' follows:
 *
 * Primitives that build a new array use the macro:
 * declArr;		Allocate slot on stack to hold a freshly created
 *			array that will be seen by the garbage collector.
 *			The value of the array can subsequently be referred
 *			to using the `arr' macro.  The declArr macro also
 *			declares a local Int variable, alen, to hold the
 *			length of the array.
 *
 * There are four methods for creating a new array, all of which return
 * the intermediate array in arr and its length in alen:
 *
 * aNewSet(b,s,v);	Allocate new array with bounds b and size s.
 *			Data elements set to v.
 * aNewNil(b,s);	Equivalent to aNewSet(b,s,NIL), treated separately
 *			because it is possible to use more efficient code
 *			for this special case in some implementations.
 * aNewCopy(a);		Builds an exact copy of array a, which can then be
 *			modified destructively, without changing a.
 *			Note that this forces evaluation of a.
 * aNewLike(a,v);	Builds an array of the same size and bounds as a
 *			with each element initialized to v.
 *			Note that this forces evaluation of a.
 *
 * All four of these methods are implemented using macros; the b, r, a
 * parameters are integers, identifying particular primArg(x) slots.
 * The v parameters should be constants, unmovable by GC, or primArg(x)
 * references that can be safely modified during GC.
 *
 * Other functions are:
 *
 * aEvalModel(a);	Evaluate model array primArg(a), and overwrite it
 *			on stack with an indirection free pointer to the
 *			resulting array.
 * aAssocs(r,as,p);	Move list of assocs -- (index,value) pairs -- from
 *			primArg(as) (which is NIL'd to prevent space leak)
 *			to top of stack and evaluate, in sequence, until all
 *			assocs have been processed.  For each pair, we use
 *			primArg(r) to calculate the integer offset and then
 *			run procedure p with this offset in whnfInt and the
 *			associated value in top(), to be popped before p is
 *			done.
 * aSetElt;		To be used with aAssocs: if arr[whnfInt] is NIL,
 *			set it to top(), otherwise set to undefined.
 * aAddElt(f);		To be used with aAssocs: replace whnfInt element e
 *			of arr with ap(ap(primArg(f),e),top())
 * aNullElts;		Set any null elements in arr to nameEltUndef.
 * aCopyNull(a);	Replace any null elements in arr with corresponding
 *			values in array primArg(a).
 * aMapElts(f);		Replace every element e in arr with ap(primArg(f),e).
 * aGetElt(a);		Push value of whnfInt'th element of primArg(a).
 * aPutElt(a,v);	Put v into whnfInt'th slot of primArg(a).
 * aElems(a);		Evaluate array at primArg(a), and return its list of
 *			elements on top of stack in reverse order, backed onto
 *			NIL (ready for revOnto(top(),nameNil)).
 * aBounds()		Extract bounds from arr.
 * aGetBounds(a)	Extract bounds from primArg(a).
 *
 * There is no guarantee that the representation used for arr will be the
 * same as for any other array.  The following method does however ensure
 * that the standard representation is used when a value is finally returned:
 *
 * updarrRoot();	Updates root of redex with array represented by arr.
 *			(Should also reset arr to avoid space leaks.)
 * aRetForST()		Update root to return an array from ST monad;
 *			i.e. return (arr, primArg(1)) as Haskell pair.
 */

#define declArr		StackPtr arrPos=sp+1; Int alen; push(NIL)
#define arr		stack(arrPos)

#define aNewNil(b,s)	aNewSet(b,s,NIL)
#define aNewSet(b,s,v)	{   Int i;				\
			    eval(primArg(s));			\
			    alen = (whnfInt>0)?whnfInt:0;	\
			    for (arr=NIL, i=alen; i>0; i--)	\
				arr = ap(v,arr);		\
			    arr = ap(primArg(b),arr);		\
			}
#define aNewCopy(a)	{   Cell es = snd(primArg(a));		\
			    for (arr=ap(hd(es),NIL), alen=0;	\
				 nonNull(es=tl(es)); ++alen)	\
				arr = ap(hd(es),arr);		\
			    arr = rev(arr);			\
			}
#define aNewLike(a,v)	{   Cell es = snd(primArg(a));		\
			    for (arr=ap(hd(es),NIL), alen=0;	\
				 nonNull(es=tl(es)); ++alen)	\
				arr = ap(v,arr);		\
			    arr = rev(arr);			\
			}
#define aEvalModel(a)	eval(primArg(a)); primArg(a)=whnfHead
#define aSetElt		{   List us = snd(arr);			\
			    for (; 0<whnfInt--; us=tl(us));	\
			    hd(us) = isNull(hd(us))?top():nameEltUndef;\
			    drop();				\
			}
#define aAddElt(f)	{   List us = snd(arr);			\
			    for (; 0<whnfInt--; us=tl(us));	\
			    hd(us) = ap(primArg(f),hd(us));	\
			    hd(us) = ap(hd(us),pop());		\
			}
#define aNullElts	{   List us = snd(arr);			\
			    for (; nonNull(us); us=tl(us))	\
				if (isNull(hd(us)))		\
				    hd(us) = nameEltUndef;	\
			}
#define aCopyNull(a)	{   List us = snd(snd(primArg(a)));	\
			    List vs = snd(arr);			\
			    for (; nonNull(vs); vs=tl(vs), us=tl(us))\
				if (isNull(hd(vs)))		\
				    hd(vs) = hd(us);		\
			}
#define aMapElts(f)	{   List us = snd(arr);			\
			    for (; nonNull(us); us=tl(us))	\
				hd(us) = ap(primArg(f),hd(us));	\
			}
#define aGetElt(a)	{   List es = snd(snd(primArg(a)));	\
			    while (0<whnfInt--)			\
				es = tl(es);			\
			    push(hd(es));			\
			}
#define aPutElt(a,v)	{   List es = snd(snd(primArg(a)));	\
			    while (0<whnfInt--)			\
				es = tl(es);			\
			    hd(es) = v;				\
			}
#define aElems(a)	{   List us;				\
			    eval(primArg(a));			\
			    us = snd(snd(primArg(a)));		\
			    chkStack(2); onto(NIL); onto(NIL);	\
			    for(; nonNull(us); us=tl(us)) {	\
				top()     = ap(nameCons,hd(us));\
				pushed(1) = ap(top(),pushed(1));\
			    }					\
			    drop();				\
			}
#define aBounds()	fst(arr)
#define aGetBounds(a)	fst(snd(primArg(a)))
#define updarrRoot()	updapRoot(ARRAY,arr); arr=NIL
#define aRetForST()	arr = ap(ARRAY,arr);			\
			updapRoot(ap(mkTuple(2),arr),primArg(1));\
			arr = NIL;
#define aRetForIO()	arr = ap(ARRAY,arr);			\
			updapRoot(primArg(1),arr);		\
			arr = NIL;

/* The implementation of aAssocs(r,p) should be independent of the
 * representation for arrays:
 */
#define aAssocs(r,as,p)	push(primArg(as)); primArg(as)=NIL;	\
			eval(pop());				\
			while (whnfHead==nameCons) {		\
			    eval(pop());			\
			    eval(ap(primArg(r),top()));		\
			    if (whnfInt<0 || whnfInt>=alen) {	\
				updapRoot(ap(nameOutBounds,aBounds()),top());\
				cantReduce();			\
			    }					\
			    drop(); p; eval(pop());		\
			}

/* Finally, we come to the implementation of the Haskell array primitives: */

primFun(primArray) {			/* :: (a,a)			   */
    declArr;				/*    -> Int			   */
    aNewNil(4,3);			/*	 -> (a -> Int)		   */
    aAssocs(2,1,aSetElt);		/*	    -> [(a,b)]		   */
    aNullElts;				/*	       -> Array a b	   */
    updarrRoot();
}

primFun(primUpdate) {			/* :: [(a,b)]			   */
    declArr;				/*    -> Array a b		   */
    aEvalModel(2);			/*       -> (a -> Int)		   */
    aNewLike(2,NIL);			/*          -> Array a b	   */
    aAssocs(1,3,aSetElt);
    aCopyNull(2);
    updarrRoot();
}

primFun(primAccum) {			/* :: [(a,c)] -> Array a b	   */
    declArr;				/*    -> (b -> c -> b) -> (a->Int) */
    aEvalModel(3);			/*	 -> Array a b		   */
    aNewCopy(3);
    aAssocs(1,4,aAddElt(2));
    updarrRoot();
}

primFun(primAccumArray) {		/* :: (a,a) -> Int		   */
    declArr;				/*    -> (b -> c -> b) -> b	   */
    aNewSet(6,5,primArg(3));		/*	 -> (a -> Int) -> [(a,c)]  */
    aAssocs(2,1,aAddElt(4));		/*	    -> Array a b	   */
    updarrRoot();
}

primFun(primAmap) {			/* :: (a -> b)			   */
    declArr;				/*    -> Array c a		   */
    aEvalModel(1);			/*       -> Array c b		   */
    aNewCopy(1);
    aMapElts(2);
    updarrRoot();
}

primFun(primSubscript) {		/* :: ((a,a) -> a -> Int)	   */
    aEvalModel(2);			/*    -> Array a b		   */
    primArg(3) = ap(primArg(3),		/*	 -> a			   */
		    aGetBounds(2));	/*	    -> b		   */
    eval(ap(primArg(3),primArg(1)));
    aGetElt(2);
    updateRoot(top());
}

primFun(primBounds) {			/* :: Array a b -> (a,a)	   */
    aEvalModel(1);
    updateRoot(aGetBounds(1));
}

primFun(primElems) {			/* :: Array a b -> [b]		   */
    aEvalModel(1);
    aElems(1);
    updateRoot(revOnto(top(),nameNil));
}

#if LAZY_ST
primFun(primSTNewArr) {			/* :: (a,a)			   */
    declArr;				/*    -> Int			   */
    eval(primArg(1));			/*	 -> b			   */
    aNewSet(4,3,primArg(2));		/*	    -> ST s (MutArr s a b) */
    aRetForST();
}

primFun(primSTReadArr) {		/* :: ((a,a) -> a -> Int)	   */
    eval(primArg(1));			/*    -> MutArr s a b		   */
    aEvalModel(3);			/*	 -> a			   */ 
    primArg(4) = ap(primArg(4),		/*	    -> ST s b		   */
		    aGetBounds(3));
    eval(ap(primArg(4),primArg(2)));
    aGetElt(3);
    topfun(mkTuple(2));
    updapRoot(top(),primArg(1));
}

primFun(primSTWriteArr) {		/* :: ((a,a) -> a -> Int)	   */
    eval(primArg(1));			/*    -> MutArr s a b		   */
    aEvalModel(4);			/*	 -> a			   */
    primArg(5) = ap(primArg(5),		/*	    -> b		   */
		    aGetBounds(4));	/*	       -> ST s ()	   */
    eval(ap(primArg(5),primArg(3)));
    aPutElt(4,primArg(2));
    updapRoot(ap(mkTuple(2),nameUnit),primArg(1));
}

primFun(primSTFreeze) {			/* :: MutArr s a b		   */
    declArr;				/*    -> ST s (Array a b)	   */
    eval(primArg(1));
    aEvalModel(2);
    aNewCopy(2);
    aRetForST();
}

primFun(primSTArrEq) {		        /* :: MutArr s a b                 */
    aEvalModel(1);                      /*    -> MutArr s a b -> Bool      */
    aEvalModel(2);
    updateRoot(primArg(1)==primArg(2) ? nameTrue : nameFalse);
}

#endif /* LAZY_ST */

#if IO_MONAD
primFun(primIONewArr) {			/* :: (a,a)			   */
    declArr;				/*    -> Int			   */
    aNewSet(5,4,primArg(3));		/*	 -> b			   */
    aRetForIO();			/*	    -> IO (IOArray a b)	   */
}

primFun(primIOReadArr) {		/* :: ((a,a) -> a -> Int)	   */
    aEvalModel(4);			/*    -> IOArray a b		   */
    primArg(5) = ap(primArg(5),		/*	 -> a			   */ 
		    aGetBounds(4));	/*	    -> IO b		   */
    eval(ap(primArg(5),primArg(3)));
    aGetElt(4);
    updapRoot(primArg(1),top());
}

primFun(primIOWriteArr) {		/* :: ((a,a) -> a -> Int)	   */
    aEvalModel(5);			/*    -> IOArray a b		   */
    primArg(6) = ap(primArg(6),		/*	 -> a			   */
	            aGetBounds(5));	/*	    -> b		   */
    eval(ap(primArg(6),primArg(4)));	/*	       -> IO ()		   */
    aPutElt(5,primArg(3));
    updapRoot(primArg(1),nameUnit);
}

primFun(primIOFreeze) {			/* :: IOArray a b		   */
    declArr;				/*    -> IO (Array a b)		   */
    aEvalModel(3);
    aNewCopy(3);
    aRetForIO();
}

primFun(primIOArrEq) {		        /* :: IOArray a b                 */
    aEvalModel(1);                      /*    -> IOArray a b -> Bool      */
    aEvalModel(2);
    updateRoot(primArg(1)==primArg(2) ? nameTrue : nameFalse);
}

#endif /* IO_MONAD */

/* Retire macros used in the implementation of arrays -------------------- */

#undef aNewSet
#undef aNewNil
#undef aNewCopy
#undef aNewLike
#undef aEvalModel
#undef aAssocs
#undef aSetElt
#undef aAddElt
#undef aNullElts
#undef aCopyNull
#undef aMapElts
#undef aGetElt
#undef aPutElt
#undef aElems
#undef aBounds
#undef aGetBounds
#undef updarrRoot
#undef aRetForST
#undef aRetForIO

/*-------------------------------------------------------------------------*/
