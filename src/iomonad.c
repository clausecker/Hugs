/* --------------------------------------------------------------------------
 * Implementation of Haskell IO monad.
 *
 * The primitives below implement the standard IO monad for Haskell 1.3
 * using a continuation passing bimonad (two streams of processing for
 * `exceptional' and `normal' I/O respectively).  The primitives are
 * believed to give a reasonably good implementation of the semantics
 * specified by the Haskell 1.3 report.  There are also some additional
 * primitives, particularly for dealing with IOError and Handle values
 * that are not included in the prelude, but have been suggested for
 * inclusion in standard libraries.  I don't know what semantics and
 * specifications will be specified for these operations in the final
 * version of the 1.3 I/O specification, so be prepared for changes.
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the Oregon Graduate Institute of Science and
 * Technology, 1994-1999, All rights reserved.  It is distributed as
 * free software under the license in the file "License", which is
 * included in the distribution.
 *
 * $RCSfile: iomonad.c,v $
 * $Revision: 1.11 $
 * $Date: 2001/02/14 12:15:05 $
 * ------------------------------------------------------------------------*/
 
Name nameIORun;			        /* run IO code                     */
Name nameIOBind;		        /* bind IO code                    */
Name namePutStr;		        /* Prelude.putStr                  */

static Name namePass;			/* auxiliary:: \f b c a -> f a b c */
#if IO_HANDLES
static Name nameHreader;	        /* auxiliary function		   */
static FILE *writingFile = 0;		/* points to file open for writing */
#endif

static Void local pushString  Args((String));

/* --------------------------------------------------------------------------
 * IO monad control:
 * ------------------------------------------------------------------------*/

static Void iomonadControl Args((Int));
static Void iomonadControl(what)
Int what; {
    switch (what) {
	case INSTALL : 
		       setCurrModule(modulePrelude);
#define pFun(n,s,t)    addPrim(0,n=newName(findText(s),NIL),t,modulePrelude,NIL)
		       pFun(namePass,	 "_pass",    "passIO");
#if    IO_HANDLES
		       pFun(nameHreader, "_hreader", "hreader");
#endif
#undef pFun
#define predef(nm,str) nm=newName(findText(str),NIL); name(nm).defn=PREDEFINED
		       predef(nameIORun,    "hugsIORun");
		       predef(nameIOBind,   "primbindIO");
		       predef(namePutStr,   "hugsPutStr");
#undef predef
		       break;

	case RESET   : 
#if IO_HANDLES
		       if (writingFile) {
			   fclose(writingFile);
			   writingFile = 0;
		       }
#endif
		       break;
    }
}

PROTO_PRIM(primLunit);
PROTO_PRIM(primRunit);
PROTO_PRIM(primLbind);
PROTO_PRIM(primRbind);
PROTO_PRIM(primPass);

PROTO_PRIM(primGC);
PROTO_PRIM(primGetEnv);
PROTO_PRIM(primSystem);
PROTO_PRIM(primGetRandomSeed);

PROTO_PRIM(primArgc);
PROTO_PRIM(primArgv);

PROTO_PRIM(primShowIOError);
PROTO_PRIM(primUserError);
PROTO_PRIM(primIsUserErr);

PROTO_PRIM(primIsSearchErr);
PROTO_PRIM(primIsNameErr);

PROTO_PRIM(primGetCh);
PROTO_PRIM(primGetChar);
PROTO_PRIM(primPutChar);
PROTO_PRIM(primPutStr);

#if IO_HANDLES
static Void local fwritePrim  Args((StackPtr,Bool,Bool));
static Void local fopenPrim   Args((StackPtr,Bool));

PROTO_PRIM(primHGetChar);
PROTO_PRIM(primHPutChar);
PROTO_PRIM(primHPutStr);
PROTO_PRIM(primHreader);
PROTO_PRIM(primHContents);
PROTO_PRIM(primContents);
PROTO_PRIM(primOpenFile);
PROTO_PRIM(primOpenBinaryFile);
PROTO_PRIM(primStdin);
PROTO_PRIM(primStdout);
PROTO_PRIM(primStderr);
PROTO_PRIM(primHIsEOF);
PROTO_PRIM(primHFlush);
PROTO_PRIM(primHClose);
PROTO_PRIM(primHGetPosn);
PROTO_PRIM(primHSetPosn);
PROTO_PRIM(primHIsOpen);
PROTO_PRIM(primHIsClosed);
PROTO_PRIM(primHIsReadable);
PROTO_PRIM(primHIsWritable);
PROTO_PRIM(primEqHandle);
PROTO_PRIM(primReadFile);
PROTO_PRIM(primWriteFile);
PROTO_PRIM(primAppendFile);
PROTO_PRIM(primReadBinaryFile);
PROTO_PRIM(primWriteBinaryFile);
PROTO_PRIM(primAppendBinaryFile);
PROTO_PRIM(primIsIllegal);
PROTO_PRIM(primIsWriteErr);
PROTO_PRIM(primIsEOFError);
PROTO_PRIM(primIsUnsupported);
PROTO_PRIM(primGetHandle);
PROTO_PRIM(primGetFileName);
#endif

#if IO_REFS
PROTO_PRIM(primNewRef);
PROTO_PRIM(primDerefRef);
PROTO_PRIM(primAssignRef);
PROTO_PRIM(primEqRef);
#endif

PROTO_PRIM(primMakeSP);
PROTO_PRIM(primDerefSP);
PROTO_PRIM(primFreeSP);

PROTO_PRIM(primMakeFO);
PROTO_PRIM(primWriteFO);
PROTO_PRIM(primEqFO);

#if GC_WEAKPTRS
PROTO_PRIM(primMakeWeakPtr);
PROTO_PRIM(primDerefWeakPtr);
PROTO_PRIM(primWeakPtrEq);
PROTO_PRIM(primMkWeak);
PROTO_PRIM(primDeRefWeak);
PROTO_PRIM(primReplaceFinalizer);
PROTO_PRIM(primFinalize);
PROTO_PRIM(primRunFinalizer);
PROTO_PRIM(primFinalizerWaiting);
#endif

#if STABLE_NAMES
PROTO_PRIM(primMakeSN);
PROTO_PRIM(primDerefSN);
PROTO_PRIM(primHashSN);
PROTO_PRIM(primEqSN);
#endif

#ifdef HSCRIPT
PROTO_PRIM(primGetCurrentScript);
#endif

static struct primitive iomonadPrimTable[] = {
  {"lunitIO",		3, primLunit},
  {"runitIO",		3, primRunit},
  {"lbindIO",		4, primLbind},
  {"rbindIO",		4, primRbind},
  {"passIO",		4, primPass},

  {"primGC",	        2, primGC},
  {"getEnv",	        3, primGetEnv},
  {"primSystem",	3, primSystem},
  {"getRandomSeed",	2, primGetRandomSeed},

  {"primArgc",	        2, primArgc},
  {"primArgv",	        3, primArgv},

  {"getCh",		2, primGetCh},
  {"getChar",		2, primGetChar},
  {"putChar",		3, primPutChar},
  {"putStr",		3, primPutStr},

  {"primShowIOError",	1, primShowIOError},
  {"userError",		1, primUserError},
  {"isUserError",	1, primIsUserErr},
  /* non-standard tests */
  {"hugsIsSearchErr",	1, primIsSearchErr},
  {"hugsIsNameErr",	        1, primIsNameErr},
  /* end non-standard tests */

#if IO_HANDLES
  {"hGetChar",		3, primHGetChar},
  {"hPutChar",		4, primHPutChar},
  {"hPutStr",		4, primHPutStr},
  {"hreader",		1, primHreader},
  {"hGetContents",	3, primHContents},
  {"getContents",	2, primContents},
  {"openFile",          4, primOpenFile},
  {"openBinaryFile",    4, primOpenBinaryFile},
  {"stdin",		0, primStdin},
  {"stdout",		0, primStdout},
  {"stderr",		0, primStderr},
  {"hIsEOF",		3, primHIsEOF},
  {"hFlush",		3, primHFlush},
  {"hClose",		3, primHClose},
  {"hGetPosn",		3, primHGetPosn},
  {"hSetPosn",		4, primHSetPosn},
  {"hIsOpen",		3, primHIsOpen},
  {"hIsClosed",		3, primHIsClosed},
  {"hIsWritable",	3, primHIsWritable},
  {"hIsReadable",	3, primHIsReadable},
  {"primEqHandle",	2, primEqHandle},
  {"readFile",		3, primReadFile},
  {"writeFile",		4, primWriteFile},
  {"appendFile",	4, primAppendFile},
  {"readBinaryFile",	3, primReadBinaryFile},
  {"writeBinaryFile",	4, primWriteBinaryFile},
  {"appendBinaryFile",	4, primAppendBinaryFile},
  {"isAlreadyExistsError", 1, primIsUnsupported},
  {"isDoesNotExistError",  1, primIsSearchErr},
  {"isAlreadyInUseError",  1, primIsUnsupported},
  {"isFullError",	   1, primIsUnsupported},
  {"isEOFError",	   1, primIsEOFError},
  {"isIllegalOperation",   1, primIsIllegal},
  {"isPermissionError",	   1, primIsUnsupported},
  /* non-standard tests */
  {"hugsIsWriteErr",       1, primIsWriteErr},
  /* end non-standard tests */
  {"ioeGetHandle",	1, primGetHandle},
  {"ioeGetFileName",	1, primGetFileName},
#endif

#if IO_REFS
  {"newRef",            3, primNewRef},
  {"getRef",		3, primDerefRef},
  {"setRef",		4, primAssignRef},
  {"eqRef",		2, primEqRef},
#endif

  {"makeStablePtr",	3, primMakeSP},
  {"deRefStablePtr",	3, primDerefSP},
  {"freeStablePtr",	3, primFreeSP},

  {"makeForeignObj",	4, primMakeFO},
  {"writeForeignObj",	4, primWriteFO},
  {"eqForeignObj",	2, primEqFO},

#if GC_WEAKPTRS
  {"makeWeakPtr",       3, primMakeWeakPtr},
  {"derefWeakPtr",      3, primDerefWeakPtr},
  {"weakPtrEq",		2, primWeakPtrEq},
  {"mkWeak",		5, primMkWeak},
  {"deRefWeak",		3, primDeRefWeak},
  {"replaceFinalizer",	4, primReplaceFinalizer},
  {"finalize",		3, primFinalize},
  {"runFinalizer",	2, primRunFinalizer},
  {"finalizerWaiting",	2, primFinalizerWaiting},
#endif

#if STABLE_NAMES
  {"makeStableName",	3, primMakeSN},
  {"deRefStableName",	1, primDerefSN},
  {"hashStableName",	1, primHashSN},
  {"eqStableName",	2, primEqSN},
#endif

#ifdef HSCRIPT
  {"getCurrentScript",  2, primGetCurrentScript},
#endif

  {0,			0, 0}
};

static struct primInfo iomonadPrims = { iomonadControl, iomonadPrimTable, 0 };

/* --------------------------------------------------------------------------
 * Macros
 *
 * Note: the IOReturn and IOFail macros do not use the standard "do while"
 * trick to create a single statement because some C compilers (eg sun)
 * report warning messages "end-of-loop code not reached".
 * This may lead to syntax errors if used where a statement is required - such
 * errors can be fixed by adding braces round the call.  Blech!
 * ------------------------------------------------------------------------*/

#define IOArg(n)    primArg((n)+2)
#define IOReturn(r) { updapRoot(primArg(1),r); return; }
#define IOFail(r)   { updapRoot(primArg(2),r); return; }

/* --------------------------------------------------------------------------
 * Building strings:
 * ------------------------------------------------------------------------*/

static Void local pushString(s)       /* push pointer to string onto stack */
String s; {
    Int  l      = strlen(s);
    push(nameNil);
    while (--l >= 0) {
	topfun(consChar(s[l]));
    }
}

/* --------------------------------------------------------------------------
 * The monad combinators:
 * ------------------------------------------------------------------------*/

primFun(primLunit) {			/* bimonad left unit		   */
    updapRoot(primArg(2),primArg(3));	/* lunit 3 2 1 = 2 3		   */
}

primFun(primRunit) {			/* bimonad right unit		   */
    updapRoot(primArg(1),primArg(3));	/* lunit 3 2 1 = 1 3		   */
}

primFun(primLbind) {			/* bimonad left bind		   */
    push(ap(namePass,primArg(3)));	/* lbind 4 3 2 1 = 4 (pass 3 2 1) 1*/
    toparg(primArg(2));
    toparg(primArg(1));
    updapRoot(ap(primArg(4),top()),primArg(1));
}

primFun(primRbind) {			/* bimonad right bind		   */
    push(ap(namePass,primArg(3)));	/* rbind 4 3 2 1 = 4 2 (pass 3 2 1)*/
    toparg(primArg(2));
    toparg(primArg(1));
    updapRoot(ap(primArg(4),primArg(2)),top());
}

primFun(primPass) {			/* Auxiliary function		   */
    push(ap(primArg(4),primArg(1)));	/* pass 4 3 2 1 = 4 1 3 2	   */
    toparg(primArg(3));
    updapRoot(top(),primArg(2));
}

/* --------------------------------------------------------------------------
 * IO Errors (more defined for file ops)
 * ------------------------------------------------------------------------*/

primFun(primShowIOError) {		/* showIOError :: IOError -> String*/
    eval(primArg(1));
    out = NIL;
    if (whnfHead==nameUserErr)		/* test for various err conditions */
	outStr("User error: ");
    else if (whnfHead==nameNameErr)
	outStr("Illegal file name: ");
    else if (whnfHead==nameSearchErr)
	outStr("File or variable not found: ");
    else if (whnfHead==nameWriteErr)
	outStr("Cannot write to file: ");
    else if (whnfHead==nameIllegal) {
	outStr("Illegal operation");
	push(nameNil);
    }
    else {
	outStr("Unrecognised I/O exception!");
	push(nameNil);
    }
    updateRoot(revOnto(out,top()));
    out   = NIL;
}

primFun(primUserError) {		/* :: String -> IOError		   */
    updapRoot(nameUserErr,primArg(1));
}

primFun(primIsUserErr) {		/* :: IOError -> Bool        	   */
    eval(primArg(1));
    BoolResult(whnfHead==nameUserErr);
}

primFun(primIsNameErr) {		/* :: IOError -> Bool		   */
    eval(primArg(1));
    BoolResult(whnfHead==nameNameErr);
}

primFun(primIsSearchErr) {		/* :: IOError -> Bool		   */
    eval(primArg(1));
    BoolResult(whnfHead==nameSearchErr);
}

primFun(primIsUnsupported) {		/* :: IOError -> Bool		   */
    updateRoot(nameFalse);
}

/* --------------------------------------------------------------------------
 * Misc.
 * ------------------------------------------------------------------------*/

primFun(primGC) {			/* force a GC right now            */
    garbageCollect();
    IOReturn(nameUnit);
}

#if BIGNUMS && defined HAVE_TIME_H
#include <time.h>

primFun(primGetRandomSeed) {		/* generate a random seed          */
    IOReturn(bigInt(clock()));
}

#else

primFun(primGetRandomSeed) {		/* generate a random seed          */
    ERRMSG(0) "getRandomSeed is not implemented on this architecture"
    EEND;
}

#endif
				     
primFun(primGetEnv) {                 /* primGetEnv :: String -> IO String */
    String s = evalName(IOArg(1));    /* Eval name	                   */
    String r;
    if (!s) {			      /* check for valid name		   */
	IOFail(ap(nameNameErr,IOArg(1)));
    } else if ((r = getenv(s))!=0) {    
	pushString(r);
	IOReturn(pop());
    } else {
	IOFail(ap(nameSearchErr,IOArg(1)));
    }
}

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_WAIT_H
# include <sys/wait.h>
#endif
#ifndef WEXITSTATUS
# define WEXITSTATUS(stat_val) ((unsigned)(stat_val) >> 8)
#endif
#ifndef WIFEXITED
# define WIFEXITED(stat_val) (((stat_val) & 255) == 0)
#endif

					 
primFun(primSystem) {                   /* primSystem :: String -> IO Int  */
    String s = evalName(IOArg(1));	/* Eval name	                   */
    Int r;
    if (s) {				/* check for valid string          */
#if HAVE_MACSYSTEM
        r = macsystem(s);
#else
	r = system(s);
#endif
	IOReturn(mkInt(WEXITSTATUS(r)));
    } else {
	IOFail(ap(nameNameErr,IOArg(1)));
    }
}
    
static String defaultArgv[] = {
  "Hugs"  /* program name */
};

static String* hugsArgv = defaultArgv;
static Int     hugsArgc = sizeof defaultArgv / sizeof defaultArgv[0];

Void setHugsArgs(argc,argv)
Int    argc;
String argv[]; {
    hugsArgc = argc;
    hugsArgv = argv;
}

primFun(primArgc) {                     /* primArgc :: IO Int              */
    IOReturn(mkInt(hugsArgc));
}
    
primFun(primArgv) {                     /* primArgv :: Int -> IO String    */
    Int i;
    IntArg(i,3);
    if (0 <= i && i < hugsArgc) {
	pushString(hugsArgv[i]);
	IOReturn(pop());
    } else {
	IOFail(nameIllegal);
    }
}
    
/* --------------------------------------------------------------------------
 * Console IO
 * ------------------------------------------------------------------------*/

primFun(primGetCh) {			/* Get character from stdin wo/echo*/
    IOReturn(mkChar(readTerminalChar()));
}

#if __MWERKS__ && macintosh
primFun(primGetChar) {			 /* Metrowerks console has no NO_ECHO mode. */
    IOReturn(mkChar(readTerminalChar()));
}
#else
primFun(primGetChar) {			/* Get character from stdin w/ echo*/
    Char c = readTerminalChar();
    putchar(c);
    fflush(stdout);
    IOReturn(mkChar(c));
}
#endif

primFun(primPutChar) {			/* print character on stdout	   */
    eval(pop());
    putchar(charOf(whnfHead));
    fflush(stdout);
    IOReturn(nameUnit);
}

primFun(primPutStr) {			/* print string on stdout	   */
    blackHoleRoot();            	/* supposedly = hPutStr stdout,	   */
    eval(pop());			/* included here for speed	   */
    while (whnfHead==nameCons) {
	eval(top());
	checkChar();
	putchar(charOf(whnfHead));
#if FLUSHEVERY
	fflush(stdout);
#endif
	drop();
	eval(pop());
    }
#if !FLUSHEVERY
    fflush(stdout);
#endif
    IOReturn(nameUnit);
}

/* --------------------------------------------------------------------------
 * File IO
 * ------------------------------------------------------------------------*/

#if IO_HANDLES

#define HandleArg(nm,offset)  \
    eval(primArg(offset));    \
    nm = intValOf(whnfHead)

#define IOBoolResult(e)  \
    IOReturn((e)?nameTrue:nameFalse)

primFun(primHGetChar) {			/* Read character from handle	   */
    Int h;
    HandleArg(h,3);
    if (handles[h].hmode&HREAD) {
	Char c = (h==HSTDIN ? readTerminalChar() : getc(handles[h].hfp));
	if (c!=EOF) {
	    IOReturn(mkChar(c));
	}
    }
    IOFail(nameIllegal);
}

primFun(primHPutChar) {			/* print character on handle	   */
    Char c = 0;
    Int  h;
    HandleArg(h,4);
    CharArg(c,3);
    if (handles[h].hmode&(HWRITE|HAPPEND)) {
	putc(c,handles[h].hfp);
	IOReturn(nameUnit);
    }
    IOFail(nameIllegal);
}

primFun(primHPutStr) {			/* print string on handle	   */
    Int h;
    HandleArg(h,4);
    push(primArg(3));
    primArg(3) = NIL;
    if (handles[h].hmode&(HWRITE|HAPPEND)) {
	blackHoleRoot();
	eval(pop());
	while (whnfHead==nameCons) {
	    eval(pop());
	    putc(charOf(whnfHead),handles[h].hfp);
#if FLUSHEVERY
	    fflush(handles[h].hfp);
#endif
	    eval(pop());
	}
#if !FLUSHEVERY
	fflush(handles[h].hfp);
#endif
	IOReturn(nameUnit);
    }
    IOFail(nameIllegal);
}

primFun(primHreader) {			/* read String from a handle 	   */
    Int h;                              /* Handle -> String                */
    HandleArg(h,1);
    if (handles[h].hmode&HSEMICLOSED) {	/* read requires semi-closed handle*/
	Int c = (h==HSTDIN ? readTerminalChar() : getc(handles[h].hfp));
	if (c!=EOF && c>=0 && c<NUM_CHARS) {
	    updapRoot(consChar(c),ap(nameHreader,primArg(1)));
	    return;
	}
	clearerr(handles[h].hfp);
    }
    updateRoot(nameNil);
}

primFun(primHContents) {		/* hGetContents :: Handle -> IO Str*/
    Int h;
    HandleArg(h,3);
    if ((handles[h].hmode&HREAD)==0) {	/* must have readable handle	   */
	IOFail(nameIllegal);
    } else {				/* semi-close handle		   */
	handles[h].hmode = HSEMICLOSED;
	IOReturn(ap(nameHreader,IOArg(1)));
    }
}

primFun(primContents) {			/* Get contents of stdin	   */
    if ((handles[HSTDIN].hmode&HREAD)==0) {
	IOFail(nameIllegal);
    } else {
	handles[HSTDIN].hmode = HSEMICLOSED;
	IOReturn(ap(nameHreader,handles[HSTDIN].hcell));
    }
}

static Void local fopenPrim(root,binary)/* Auxiliary function for          */
StackPtr root;                          /* opening a file                  */
Bool     binary; {
    String s = evalName(IOArg(2));	/* Eval and check filename	   */
    Int    m = HCLOSED;

    if (!s) {				/* check for valid name		   */
	IOFail(ap(nameNameErr,IOArg(2)));
    }
    eval(IOArg(1));			/* Eval IOMode			   */
    if (isName(whnfHead) && isCfun(whnfHead))
	switch (cfunOf(whnfHead)) {	/* we have to use numeric consts   */
	    case 1 : m = HREAD;		/* here to avoid the need to put   */
		     break;		/* IOMode in startup environment   */
	    case 2 : m = HWRITE;
		     break;
	    case 3 : m = HAPPEND;
		     break;
	}

    if (m!=HCLOSED) {			/* Only accept legal modes	   */
	Cell hnd = openHandle(s,m,binary);
	if (nonNull(hnd)) {
	    IOReturn(hnd);
	}
    }

    IOFail(nameIllegal);
}

primFun(primOpenFile) {			/* open handle to a text file	   */
    fopenPrim(root,FALSE);
}

primFun(primOpenBinaryFile) {		/* open handle to a binary file	   */
    fopenPrim(root,TRUE);
}

primFun(primStdin) {			/* Standard input handle	   */
    push(handles[HSTDIN].hcell);
}

primFun(primStdout) {			/* Standard output handle	   */
    push(handles[HSTDOUT].hcell);
}

primFun(primStderr) {			/* Standard error handle	   */
    push(handles[HSTDERR].hcell);
}

/* NOTE: this doesn't implement the Haskell 1.3 semantics */
primFun(primHIsEOF) {			/* Test for end of file on handle  */
    Int h;
    HandleArg(h,3);
    if (handles[h].hmode!=HCLOSED) {
	IOReturn(feof(handles[h].hfp) ? nameTrue : nameFalse);
    } else {
	IOFail(nameIllegal);
    }
}

primFun(primHFlush) {			/* Flush handle			   */
    Int h;
    HandleArg(h,3);
    if (handles[h].hmode!=HCLOSED) {
	fflush(handles[h].hfp);
	IOReturn(nameUnit);
    }
    else
	IOFail(nameIllegal);
}

primFun(primHClose) {			/* Close handle                   */
    Int h;
    HandleArg(h,3);
    if (handles[h].hmode!=HCLOSED) {
	if (h>HSTDERR && handles[h].hfp)
	    fclose(handles[h].hfp);
	handles[h].hfp   = 0;
	handles[h].hmode = HCLOSED;
	IOReturn(nameUnit);
    }
    IOFail(nameIllegal);
}

primFun(primHGetPosn) {			/* Get file position               */
    Int h;
    HandleArg(h,3);
    if (handles[h].hmode!=HCLOSED) {
#if HAVE_FTELL
	long pos = ftell(handles[h].hfp);
	IOReturn(mkInt((Int)pos));
#else
	/* deliberate fall through to IOFail */
#endif
    }
    IOFail(nameIllegal);
}

primFun(primHSetPosn) {			/* Set file position               */
#if HAVE_FSEEK
    long   pos = 0;
#endif
    Int    h;
    HandleArg(h,3);
    IntArg(pos,4);
    if (handles[h].hmode!=HCLOSED) {
#if HAVE_FSEEK
	if (fseek(handles[h].hfp,pos,SEEK_SET)) {
	    IOReturn(nameUnit);
	}
#else
	/* deliberate fall through to IOFail */
#endif
    }
    IOFail(nameIllegal);
}

primFun(primHIsOpen) {			/* Test is handle open             */
    Int h;
    HandleArg(h,3);
    IOBoolResult(handles[h].hmode!=HCLOSED 
		 && handles[h].hmode!=HSEMICLOSED);
}

primFun(primHIsClosed) {		/* Test is handle closed           */
    Int h;
    HandleArg(h,3);
    IOBoolResult(handles[h].hmode==HCLOSED);
}

primFun(primHIsReadable) {		/* Test is handle readable         */
    Int h;
    HandleArg(h,3);
    IOBoolResult(handles[h].hmode&HREAD);
}

primFun(primHIsWritable) {		/* Test is handle writable         */
    Int h;
    HandleArg(h,3);
    IOBoolResult(handles[h].hmode&HWRITE);
}

primFun(primEqHandle) {			/* Test for handle equality        */
    Int h1, h2;
    HandleArg(h1,1);
    HandleArg(h2,2);
    BoolResult(h1==h2);
}

primFun(primReadFile) {			/* read file as lazy string	   */
    String s = evalName(IOArg(1));	/* Eval and check filename	   */
    if (!s) {
	IOFail(ap(nameNameErr,IOArg(1)));
    }
    else {
	Cell hnd = openHandle(s,HREAD,FALSE);
	if (isNull(hnd)) {
	    IOFail(ap(nameSearchErr,IOArg(1)));
	}
	handles[intValOf(hnd)].hmode = HSEMICLOSED;
	IOReturn(ap(nameHreader,hnd));
    }
}

primFun(primReadBinaryFile) {		/* read file as lazy string	   */
    String s = evalName(IOArg(1));	/* Eval and check filename	   */
    if (!s) {
	IOFail(ap(nameNameErr,IOArg(1)));
    }
    else {
	Cell hnd = openHandle(s,HREAD,TRUE);
	if (isNull(hnd)) {
	    IOFail(ap(nameSearchErr,IOArg(1)));
	}
	handles[intValOf(hnd)].hmode = HSEMICLOSED;
	IOReturn(ap(nameHreader,hnd));
    }
}

primFun(primWriteFile) {		/* write string to specified file  */
    fwritePrim(root,FALSE,FALSE);
}

primFun(primAppendFile) {		/* append string to specified file */
    fwritePrim(root,TRUE,FALSE);
}

primFun(primWriteBinaryFile) {		/* write string to specified file  */
    fwritePrim(root,FALSE,TRUE);
}

primFun(primAppendBinaryFile) {		/* append string to specified file */
    fwritePrim(root,TRUE,TRUE);
}

#if HAVE_GETFINFO
extern int access(char *fileName, int dummy);
#endif

static Void local fwritePrim(root,append,binary)/* Auxiliary function for  */
StackPtr root;					/* writing/appending to	   */
Bool     append; 				/* an output file	   */
Bool     binary; {
    String mode = binary ? (append ? "ab" : "wb")
			 : (append ? "a"  : "w");
    String s    = evalName(IOArg(2));		/* Eval and check filename */
    if (!s) {
	IOFail(ap(nameNameErr,IOArg(2)));
    }
    else if (append && access(s,0)!=0) {	/* Check that file exists  */
	IOFail(ap(nameSearchErr,IOArg(2)));
    }
    else if ((writingFile=fopen(s,mode))==0) {	/* Open file for writing   */
	IOFail(ap(nameWriteErr,IOArg(2)));
    }
    else {					/* Output characters	   */
	blackHoleRoot();
	drop();
	eval(pop());
	while (whnfHead==nameCons) {
	    eval(top());
	    checkChar();
	    fputc(charOf(whnfHead),writingFile);
	    drop();
	    eval(pop());
	}
	fclose(writingFile);
	writingFile = 0;
	IOReturn(nameUnit);
    }
}

primFun(primIsIllegal) {		/* :: IOError -> Bool		   */
    eval(primArg(1));
    BoolResult(whnfHead==nameIllegal);
}

primFun(primIsWriteErr) {		/* :: IOError -> Bool		   */
    eval(primArg(1));
    BoolResult(whnfHead==nameWriteErr);
}

primFun(primIsEOFError) {		/* :: IOError -> Bool		   */
    eval(primArg(1));
    BoolResult(whnfHead==nameEOFErr);
}

primFun(primGetHandle) {		/* :: IOError -> Maybe Handle	   */
    eval(primArg(1));
    /* insert tests here */
    updateRoot(nameNothing);
}

primFun(primGetFileName) {		/* :: IOError -> Maybe FilePath	   */
    eval(primArg(1));
    if (whnfHead==nameNameErr || whnfHead==nameSearchErr
			      || whnfHead==nameWriteErr)
	updapRoot(nameJust,top());
    else
	updateRoot(nameNothing);
}

#endif /* IO_HANDLES */

/* --------------------------------------------------------------------------
 * Mutable variables
 * ------------------------------------------------------------------------*/

#if IO_REFS

#if CHECK_TAGS
#define checkRef() if (MUTVAR != whatIs(whnfHead)) internal("Ref expected")
#else
#define checkRef() /* do nothing */
#endif

primFun(primNewRef) {			/* a -> IO (Ref a)		   */
    IOReturn(ap(MUTVAR,IOArg(1)));
}

primFun(primDerefRef) {			/* Ref a -> IO a		   */
    eval(pop());
    checkRef();
    IOReturn(snd(whnfHead));
}

primFun(primAssignRef) {		/* Ref a -> a -> IO ()		   */
    eval(IOArg(2));
    checkRef();
    snd(whnfHead) = IOArg(1);
    IOReturn(nameUnit);
}

primFun(primEqRef) {			/* Ref a -> Ref a -> Bool	   */
    eval(primArg(2));
    checkRef();
    push(whnfHead);
    eval(primArg(1));
    checkRef();
    updateRoot(pop()==whnfHead ? nameTrue : nameFalse);
}
#endif

/* --------------------------------------------------------------------------
 * Stable Pointers
 * ------------------------------------------------------------------------*/

primFun(primMakeSP) {			/* a -> IO (StablePtr a)	   */
    Int sp = mkStablePtr(IOArg(1));
    if (sp > 0) {
	IOReturn(mkInt(sp));
    } else {
	IOFail(nameIllegal); 
    }
}

primFun(primDerefSP) {			/* StablePtr a -> IO a   	   */
    eval(IOArg(1));
    IOReturn(derefStablePtr(whnfInt));
}

primFun(primFreeSP) {			/* StablePtr a -> IO ()   	   */
    eval(IOArg(1));
    freeStablePtr(whnfInt);
    IOReturn(nameUnit);
}
    
/* --------------------------------------------------------------------------
 * Foreign Objects
 * ------------------------------------------------------------------------*/

#if CHECK_TAGS
#define checkForeign() if (MPCELL != whatIs(whnfHead)) internal("ForeignObj expected")
#else
#define checkForeign() /* do nothing */
#endif

primFun(primMakeFO) {			/* a -> IO (Ref a)		   */
    Pointer addr = 0;
    Void (*free)(Pointer) = 0;
    eval(IOArg(2));
    addr = ptrOf(whnfHead);
    eval(IOArg(1));
    free = (Void (*)(Pointer))ptrOf(whnfHead);
    IOReturn(mkMallocPtr(addr,free));
}

primFun(primWriteFO) {		/* ForeignObj -> Addr -> IO ()		   */
    Cell mp = NIL;
    eval(IOArg(2));
    checkForeign();
    mp = whnfHead;
    eval(IOArg(1));
    derefMP(mp) = ptrOf(whnfHead);
    IOReturn(nameUnit);
}

primFun(primEqFO) {			/* ForeignObj -> ForeignObj -> Bool*/
    eval(primArg(2));
    checkForeign();
    push(whnfHead);
    eval(primArg(1));
    checkForeign();
    updateRoot(pop()==whnfHead ? nameTrue : nameFalse);
}

#if STABLE_NAMES
/* --------------------------------------------------------------------------
 * Stable Names
 * ------------------------------------------------------------------------*/

primFun(primMakeSN) {		/* a -> IO (StableName a)		   */
    IOReturn(ap(STABLENAME,IOArg(1)));
}

primFun(primDerefSN) {		/* StableName a -> a			   */
    eval(primArg(1));
    updateRoot(snd(whnfHead));
}

primFun(primHashSN) {		/* StableName a -> Int			   */
    eval(primArg(1));
    updateRoot(mkInt(whnfHead));
}
primFun(primEqSN) {		/* StableName a -> StableName a -> Bool	   */
    eval(primArg(2));
    push(whnfHead);
    eval(primArg(1));
    updateRoot(pop()==whnfHead ? nameTrue : nameFalse);
}
#endif

#if GC_WEAKPTRS
/* --------------------------------------------------------------------------
 * Weak Pointers
 * ------------------------------------------------------------------------*/

#if CHECK_TAGS
#define checkWeak() if(WEAKCELL!=whatIs(whnfHead)) internal("weakPtr expected");
#else
#define checkWeak() /* do nothing */
#endif

primFun(primMakeWeakPtr) {		/* a -> IO (Weak a)		   */
    assert(isGenPair(IOArg(1)));	/* (Sadly, this may not be true)   */
    IOReturn(mkWeakPtr(IOArg(1)));	/* OLD ... retire soon		   */
}

primFun(primDerefWeakPtr) {		/* Weak a -> IO (Maybe a)	   */
    eval(IOArg(1));			/* OLD ... retire soon		   */
    checkWeak();
    if (isNull(derefWeakPtr(whnfHead))) {
	IOReturn(nameNothing);
    } else {
	IOReturn(ap(nameJust,derefWeakPtr(whnfHead)));
    }
}

primFun(primWeakPtrEq) {		/* Weak a -> Weak a -> Bool	   */
    eval(primArg(2));
    push(whnfHead);
    eval(primArg(1));
    updateRoot(pop()==whnfHead ? nameTrue : nameFalse);
}

primFun(primMkWeak) {			/* k -> v -> Maybe (IO ())	   */
    Cell w = NIL;			/*		    -> IO (Weak v) */
    eval(IOArg(1));
    if (whnfHead==nameJust) {		/* Look for finalizer		   */
	w = pop();
    }
    w		     = ap(NIL,ap(NIL,ap(NIL,w)));
    fst(snd(w))      = IOArg(3);
    fst(snd(snd(w))) = IOArg(2);
    liveWeakPtrs     = cons(w,liveWeakPtrs);
    fst(w)           = WEAKFIN;
    IOReturn(w);
}

primFun(primDeRefWeak) {		/* Weak v -> IO (Maybe v)	   */
    eval(IOArg(1));
    if (whatIs(whnfHead)!=WEAKFIN) {
	internal("primDeRefWeak");
    }
    if (nonNull(snd(whnfHead))) {
	IOReturn(ap(nameJust,fst(snd(snd(whnfHead)))));
    } else {
	IOReturn(nameNothing);
    }
}

primFun(primReplaceFinalizer) {		/* Weak v -> Maybe (IO ())	   */
					/*	-> IO (Maybe (IO ()))	   */
    eval(IOArg(1));			/* Grab new finalizer ...	   */
    if (whnfHead!=nameJust) {
	push(NIL);
    }
    eval(IOArg(2));			/* Get weak pointer ...		   */
    if (whatIs(whnfHead)!=WEAKFIN) {
	internal("primReplaceFinalizer");
    } else if (nonNull(snd(whnfHead))) {/* ... and replace finalizer	   */
	Cell oldfin = snd(snd(snd(whnfHead)));
	snd(snd(snd(whnfHead))) = pop();
	if (nonNull(oldfin)) {
	    IOReturn(ap(nameJust,oldfin));
	}
    }
    IOReturn(nameNothing);
}

primFun(primFinalize) {			/* Weak v -> IO ()		   */
    eval(IOArg(1));			/* Bring weak pointer to an early  */
    if (whatIs(whnfHead)!=WEAKFIN) {	/* end ...			   */
	internal("primFinalize");
    } else if (nonNull(snd(whnfHead))) {
	Cell wp = whnfHead;
	Cell vf = snd(snd(wp));
	if (isPair(vf)) {
	    if (nonNull(snd(vf))) {
		fst(vf)    = snd(vf);
		snd(vf)    = finalizers;
		finalizers = vf;
	    }
	    fst(snd(wp)) = NIL;
	    snd(snd(wp)) = NIL;
	    snd(wp)      = NIL;
	}
	liveWeakPtrs = removeCell(wp,liveWeakPtrs);
    }
    IOReturn(nameUnit);
}

primFun(primRunFinalizer) {		/* IO ()			   */
    if (isNull(finalizers)) {
	IOReturn(nameUnit);
    } else {
	updapRoot(ap(hd(finalizers),primArg(2)),primArg(1));
	finalizers = tl(finalizers);
	return;
    }
}

primFun(primFinalizerWaiting) {		/* IO Boolean			   */
    if (isNull(finalizers)) {
	IOReturn(nameFalse);
    } else {
	IOReturn(nameTrue);
    }
}
#endif /* GC_WEAKPTRS */

#if HSCRIPT
#if EMBEDDED
extern void* getCurrentScript(void);

primFun(primGetCurrentScript) {  /* IO Int */
    IOReturn( mkInt( (int)getCurrentScript() ) );
}

#else
 
primFun(primGetCurrentScript) {  /* IO Int */
    IOReturn( mkInt( 0 ) );
}

#endif /* EMBEDDED */
#endif /* HSCRIPT */

/*-------------------------------------------------------------------------*/
