/* --------------------------------------------------------------------------
 * Mutable variables,  based on `Lazy State Threads' by Launchbury and
 * Peyton Jones, PLDI 94.
 *
 *   type ST s a = State s -> (a, State s)
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the Oregon Graduate Institute of Science and
 * Technology, 1994-1999, All rights reserved.  It is distributed as
 * free software under the license in the file "License", which is
 * included in the distribution.
 *
 * $RCSfile: stmonad.c,v $
 * $Revision: 1.3 $
 * $Date: 1999/09/13 11:01:08 $
 * ------------------------------------------------------------------------*/

/* --------------------------------------------------------------------------
 * ST monad control:
 * ------------------------------------------------------------------------*/

static Void stmonadControl Args((Int));
static Void stmonadControl(what)
Int what; {
    switch (what) {
	case INSTALL : setCurrModule(modulePrelude);
		       break;
    }
}

PROTO_PRIM(primSTRun);
#if IO_MONAD
PROTO_PRIM(primSTtoIO);
#endif
PROTO_PRIM(primSTFix);
PROTO_PRIM(primSTReturn);
PROTO_PRIM(primSTLazyBind);
PROTO_PRIM(primSTStrictBind);
PROTO_PRIM(primSTInter);
PROTO_PRIM(primSTNew);
PROTO_PRIM(primSTAssign);
PROTO_PRIM(primSTDeref);
PROTO_PRIM(primSTMutVarEq);
PROTO_PRIM(primSTEql);
PROTO_PRIM(primSTHash);

static struct primitive stmonadPrimTable[] = {
  {"runST",		1, primSTRun},
#if IO_MONAD
  {"primSTtoIO",	3, primSTtoIO},
#endif
  {"STFix",		2, primSTFix},
  {"STReturn",	        1, primSTReturn},
  {"STLazyBind",	3, primSTLazyBind},
  {"STStrictBind",	3, primSTStrictBind},
  {"STInter",		2, primSTInter},
  {"STNew",		2, primSTNew},
  {"STAssign",		3, primSTAssign},
  {"STDeref",		2, primSTDeref},
  {"STMutVarEq",	2, primSTMutVarEq},
  {"STEql",	        3, primSTEql},
  {"STHash",	        2, primSTHash},
  {0,			0, 0}
};

static struct primInfo stmonadPrims = { stmonadControl, stmonadPrimTable, 0 };

/*-------------------------------------------------------------------------*/

primFun(primSTRun) {			/* ST monad encapsulate		   */
    updapRoot(nameFst,			/*  :: (forall s. ST s a) -> a	   */
	      ap(primArg(1),nameUnit));
}

#if IO_MONAD
primFun(primSTtoIO) {			/* Embed state transformer in IO   */
    toparg(nameUnit);			/*  :: ST s a -> IO a		   */
    eval(pop());			/* Assume ST s a = State s ->	   */
    updapRoot(primArg(1),top());	/*		      (a, State s) */
}					/* and that State s = ()	   */
#endif

primFun(primSTFix) {			/* ST monad encapsulate		   */
    push(ap(primArg(2),NIL));           /*  :: (a -> ST s a) -> ST s a	   */
    push(ap(top(),primArg(1)));
    snd(pushed(1)) = ap(nameFst,top());
    updateRoot(top());
}

primFun(primSTReturn) {		        /* ST monad return		   */
    updapRoot(mkTuple(2),primArg(1));	/* return    :: a -> ST s a	   */
}					/* return a   = \s -> (a,s)	   */

primFun(primSTLazyBind) {		/* lazy ST monad bind		   */
    Cell r = ap(primArg(3),primArg(1));	/* :: ST s a ->			   */
    push(r);				/*     (a -> ST s b) ->		   */
    topfun(nameFst);			/*	ST s b			   */
    updapRoot(ap(primArg(2),top()),	/* lazy version of bind on ST	   */
	      ap(nameSnd,r));
}

primFun(primSTStrictBind) {		/* strict ST monad bind		   */
    eval(ap(primArg(3),primArg(1)));	/* :: ST s a ->			   */
    top() = ap(primArg(2),top());       /*     (a -> ST s b) ->		   */
    updapRoot(top(),pushed(1));         /*      ST s b                     */
}

primFun(primSTInter) {			/* ST monad interleave		   */
    push(ap(primArg(2),primArg(1)));	/*  :: ST s a ->		   */
    topfun(nameFst);			/*      ST s a			   */
    updapRoot(ap(mkTuple(2),top()),primArg(1));
}

primFun(primSTNew) {			/* ST monad variable allocator	   */
    eval(primArg(1));			/*  :: a ->			   */
    push(ap(MUTVAR,primArg(2)));	/*	ST s (MutVar s a)	   */
    updapRoot(ap(mkTuple(2),top()),primArg(1));
}

primFun(primSTAssign) {			/* ST monad assignment		   */
    eval(primArg(1));			/*  :: MutVar s a -> a -> ST s ()  */
    eval(primArg(3));
    if (!isPair(whnfHead) || fst(whnfHead)!=MUTVAR)
	internal("type error in assign");
    snd(whnfHead) = primArg(2);		/* Arrgh! impurity! :-)		   */
    updapRoot(ap(mkTuple(2),nameUnit),primArg(1));
}

primFun(primSTDeref) {			/* ST monad dereference		   */
    eval(primArg(1));			/*  :: MutVar s a -> ST s a	   */
    eval(primArg(2));
    if (!isPair(whnfHead) || fst(whnfHead)!=MUTVAR)
	internal("type error in deref");
    updapRoot(ap(mkTuple(2),snd(whnfHead)),primArg(1));
}

primFun(primSTMutVarEq) {		/* ST monad variable equality	   */
    Cell x;				/*  :: MutVar s a -> 		   */
    eval(primArg(2));   		/*	MutVar s a -> Bool	   */
    x = whnfHead;
    eval(primArg(1));
    updateRoot(x==whnfHead ? nameTrue : nameFalse);
}

/* --------------------------------------------------------------------------
 * Primitives for implementing disposable memo functions
 * Byron Cook -- byron@cse.ogi.edu
 *
 * STEql :: Eval a => a -> a -> ST Mem Bool 
 *   if argument is an Int or Char
 *   then use ==
 *   else use pointer identity
 *
 * STHash :: Eval a => a -> ST Mem Int
 *   if a is an Int or Char
 *   then use value cast as an Int
 *   else use pointer identity
 *
 * (Earlier versions made Float a special case too - but that's not very
 *  portable since it assumes that sizeof(FloatPro) == sizeof(Int).)
 * ------------------------------------------------------------------------*/

#define STReturn(x)     updapRoot(ap(mkTuple(2),(x)),primArg(1))
#define STReturnBool(x) STReturn( (x) ? nameTrue : nameFalse )
#define STReturnInt(x)  STReturn( mkInt(x) )

primFun(primSTEql) {		    /* :: Eval a => a -> a -> ST Mem Bool */
    Cell x = NIL;
    Cell y = NIL;
    eval(primArg(2));
    eval(primArg(3));
    x = followInd(primArg(2));
    y = followInd(primArg(3));

    if (whatIs(x) == whatIs(y)) {
	switch (whatIs(x)) {
	   case INTCELL   : STReturnBool(intOf(x)==intOf(y));
			    return;
	   case CHARCELL  : STReturnBool(charOf(x)==charOf(y));
			    return;
	   /* deliberate fall through to end */
	}
    }
    STReturnBool(x==y);
}

primFun(primSTHash) {                      /* :: Eval a => a -> ST Mem Int */
    Cell x = primArg(2);
    eval(primArg(2));
    x = followInd(primArg(2)); 

    switch(whatIs(x)) {
	case INTCELL   : STReturnInt(intOf(x)); 
			 return;
	case CHARCELL  : STReturnInt(charOf(x));
			 return;
    }
    STReturnInt((Int)x); 
}

#undef STReturn
#undef STReturnBool
#undef STReturnInt

/* See also: implementation of ST primitives for mutable arrays in array.c */

