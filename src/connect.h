/* --------------------------------------------------------------------------
 * Connections between components of the Hugs system
 *
 * Hugs 98 is Copyright (c) Mark P Jones, Alastair Reid and the Yale
 * Haskell Group 1994-99, and is distributed as Open Source software
 * under the Artistic License; see the file "Artistic" that is included
 * in the distribution for details.
 *
 * $RCSfile: connect.h,v $
 * $Revision: 1.1 $
 * $Date: 1999/06/07 23:53:37 $
 * ------------------------------------------------------------------------*/

/* --------------------------------------------------------------------------
 * Standard data:
 * ------------------------------------------------------------------------*/

extern Bool   haskell98;		/* TRUE => Haskell 98 compatibility*/
extern Module modulePrelude;

/* --------------------------------------------------------------------------
 * Primitive constructor functions 
 * ------------------------------------------------------------------------*/

extern Name  nameFalse, nameTrue;
extern Name  nameNil,   nameCons;
extern Name  nameJust,	nameNothing;
extern Name  nameLeft,	nameRight;
extern Name  nameUnit;

extern Name  nameLT,      nameEQ;
extern Name  nameGT;
extern Name  nameFst,     nameSnd;      /* standard combinators		   */
extern Name  nameId,	  nameOtherwise;
extern Name  nameNegate,  nameFlip;	/* primitives reqd for parsing	   */
extern Name  nameFrom,    nameFromThen;
extern Name  nameFromTo,  nameFromThenTo;
extern Name  nameFatbar,  nameFail;	/* primitives reqd for translation */
extern Name  nameIf,	  nameSel;
extern Name  nameCompAux;
extern Name  namePmInt,	  namePmFlt;	/* primitives for pattern matching */
extern Name  namePmInteger;
#if NPLUSK
extern Name  namePmNpk,	  namePmSub;	/* primitives for (n+k) patterns   */
#endif
extern Name  nameError;			/* For runtime error messages	   */
extern Name  nameUndefined;		/* A generic undefined value	   */
extern Name  nameBlackHole;		/* For GC-detected black hole	   */
extern Name  nameInd;			/* For dict indirection		   */
extern Name  nameAnd,	  nameOr;	/* For optimisation of && and ||   */
extern Name  nameFromInt, nameFromDouble;/*coercion of numerics		   */
extern Name  nameFromInteger;
extern Name  nameEq,	  nameCompare;	/* names used for deriving	   */
extern Name  nameMinBnd,  nameMaxBnd;
extern Name  nameIndex,	  nameInRange;
extern Name  nameRange;
extern Name  nameLe,      nameGt;
extern Name  nameShowsPrec, nameReadsPrec;
extern Name  nameMult,	  namePlus;
extern Name  nameConCmp,  nameEnRange;
extern Name  nameEnIndex, nameEnInRng;
extern Name  nameEnToEn,  nameEnFrEn;
extern Name  nameEnFrom,  nameEnFrTh;
extern Name  nameEnFrTo;
extern Name  nameComp,	  nameApp;	/* composition and append	   */
extern Name  nameShowField;		/* display single field		   */
extern Name  nameShowParen;		/* wrap with parens		   */
extern Name  nameReadField;		/* read single field		   */
extern Name  nameReadParen;             /* unwrap from parens              */
extern Name  nameLex;                   /* lexer                           */
extern Name  nameRangeSize;		/* calculate size of index range   */
extern Class classMonad;		/* Monads			   */
extern Name  nameReturn,  nameBind;	/* for translating monad comps	   */
extern Name  nameMFail;
extern Name  nameListMonad;		/* builder function for List Monad */

#if EVAL_INSTANCES
extern Name  nameStrict,  nameSeq;	/* Members of class Eval	   */
extern Name  nameIStrict, nameISeq;	/* ... and their implementations   */
#endif

extern Name  namePrint;			/* printing primitive		   */

#if    IO_MONAD
extern Type   typeProgIO;		/* For the IO monad, IO ()	   */
extern Name   nameIORun;	        /* IO monad executor		   */
extern Name   namePutStr;	        /* Prelude.putStr                  */
extern Name   nameUserErr;              /* primitives required for IOError */
extern Name   nameNameErr,  nameSearchErr;
#endif

#if IO_HANDLES
extern Name   nameWriteErr, nameIllegal;/* primitives required for IOError */
extern Name   nameEOFErr;
#endif

extern Text  textPrelude;
extern Text  textNum;			/* used to process default decls   */
#if    NPLUSK
extern Text  textPlus;			/* Used to recognise n+k patterns  */
#endif
#if TREX
extern Name  nameNoRec;			/* The empty record		   */
extern Type  typeNoRow;			/* The empty row		   */
extern Type  typeRec;			/* Record formation		   */
extern Kind  extKind;			/* Kind of extension, *->row->row  */
extern Name  nameRecExt;		/* Extend a record		   */
extern Name  nameRecBrk;		/* Break a record		   */
extern Name  nameAddEv;			/* Addition of evidence values	   */
extern Name  nameRecSel;		/* Select a record		   */
extern Name  nameRecShw;		/* Show a record		   */
extern Name  nameShowRecRow;		/* Used to output rows		   */
extern Name  nameRecEq;			/* Compare records		   */
extern Name  nameEqRecRow;		/* Used to compare rows		   */
extern Name  nameInsFld;		/* Field insertion routine	   */
#endif

extern String repeatStr;		/* Repeat last command string	   */
extern String hugsEdit;			/* String for editor command	   */
extern String hugsPath;			/* String for file search path	   */
extern String projectPath;		/* String for project search path  */

extern Type  typeArrow;			/* Builtin type constructors	   */
extern Type  typeList;
extern Type  typeUnit;

#define fn(from,to)  ap(ap(typeArrow,from),to)	/* make type: from -> to   */

extern List  stdDefaults;		/* List of standard default types  */

extern Class classEq;			/* `standard' classes		   */
extern Class classOrd;
extern Class classShow;
extern Class classRead;
extern Class classIx;
extern Class classEnum;
#if EVAL_INSTANCES
extern Class classEval;
#endif
extern Class classBounded;

extern Class classReal;			/* `numeric' classes		   */
extern Class classIntegral;
extern Class classRealFrac;
extern Class classRealFloat;
extern Class classFractional;
extern Class classFloating;
extern Class classNum;

extern Cell  *CStackBase;		/* pointer to base of C stack	   */

extern List  tyconDefns;		/* list of type constructor defns  */
extern List  typeInDefns;		/* list of synonym restrictions	   */
extern List  valDefns;			/* list of value definitions       */
extern List  classDefns;		/* list of class definitions       */
extern List  instDefns;			/* list of instance definitions    */
extern List  selDefns;			/* list of selector lists	   */
extern List  genDefns;			/* list of generated defns	   */
extern List  primDefns;			/* list of primitive definitions   */
extern List  unqualImports;		/* unqualified import list         */
extern List  defaultDefns;		/* default definitions (if any)	   */
extern Int   defaultLine;		/* line in which default defs occur*/
extern List  evalDefaults;		/* defaults for evaluator	   */
extern Cell  inputExpr;			/* evaluator input expression      */
extern Addr  inputCode;			/* Code for compiled input expr    */

extern Int   whnfArgs;		 	/* number of args of term in whnf  */
extern Cell  whnfHead;		 	/* head of term in whnf            */
extern Int   whnfInt;		 	/* integer value of term in whnf   */
extern Float whnfFloat;		 	/* float value of term in whnf	   */
extern Long  numReductions;		/* number of reductions used       */
extern Long  numCells;			/* number of cells allocated       */
extern Int   numGcs;			/* number of garbage collections   */
extern Bool  broken;			/* indicates interrupt received    */
extern Bool  preludeLoaded;		/* TRUE => prelude has been loaded */

extern Bool  gcMessages;		/* TRUE => print GC messages	   */
extern Bool  literateScripts;		/* TRUE => default lit scripts     */
extern Bool  literateErrors;		/* TRUE => report errs in lit scrs */
extern Bool  failOnError;		/* TRUE => error produces immediate*/
					/*	   termination		   */

extern Int   cutoff;			/* Constraint Cutoff depth	   */

#if USE_PREPROCESSOR
extern String preprocessor;             /* preprocessor command            */
#endif

#if DEBUG_CODE
extern Bool  debugCode;		        /* TRUE => print G-code to screen  */
#endif
#if DEBUG_SHOWSC
extern Bool  debugSC;			/* TRUE => print SC to screen  */
extern Void  printSc Args((FILE*, Text, Int, Cell));
#endif
extern Bool  kindExpert;		/* TRUE => display kind errors in  */
					/* 	   full detail		   */
extern Bool  allowOverlap;		/* TRUE => allow overlapping insts */

/* --------------------------------------------------------------------------
 * Function prototypes etc...
 * ------------------------------------------------------------------------*/

extern Void everybody Args((Int));

#define RESET   1		/* reset subsystem                         */
#define MARK    2		/* mark parts of graph in use by subsystem */
#define INSTALL 3		/* install subsystem (executed once only)  */
#define EXIT	4		/* Take action immediately before exit()   */
#define BREAK   5		/* Take action after program break	   */

typedef long   Target;
extern  Void   setGoal          Args((String, Target));
extern  Void   soFar            Args((Target));
extern  Void   done             Args((Void));
extern  String fromEnv		Args((String,String));
extern  Bool   chase		Args((List));

extern  Void   storage          Args((Int));

extern  Void   input            Args((Int));
extern  Void   consoleInput     Args((String));
extern  Void   projInput	Args((String));
extern  Void   stringInput      Args((String));
extern  Void   parseScript      Args((String,Long));
extern  Void   parseExp         Args((Void));
extern  String readFilename     Args((Void));
extern  String readLine		Args((Void));
extern  Syntax defaultSyntax    Args((Text));
extern  Syntax syntaxOf		Args((Name));
extern  String unlexChar        Args((Char,Char));
extern  Void   printString	Args((String));

extern  Void   substitution	Args((Int));

extern  Void   staticAnalysis   Args((Int));
#if IGNORE_MODULES
#define startModule(m)       doNothing()
#define setExportList(l)     doNothing()
#define setExports(l)        doNothing()
#define addQualImport(m,as)  doNothing()
#define addUnqualImport(m,l) doNothing()
#else
extern  Void   startModule      Args((Cell));
extern  Void   setExportList    Args((List));
extern  Void   setExports       Args((List));
extern  Void   addQualImport    Args((Text,Text));
extern  Void   addUnqualImport  Args((Text,List));
#endif
extern  Void   tyconDefn	Args((Int,Cell,Cell,Cell));
extern  Void   setTypeIns	Args((List));
extern  Void   clearTypeIns	Args((Void));
extern  Type   fullExpand	Args((Type));
extern  Bool   isAmbiguous	Args((Type));
extern  Void   ambigError	Args((Int,String,Cell,Type));
extern  Void   classDefn	Args((Int,Cell,Cell));
extern  Void   instDefn		Args((Int,Cell,Cell));
extern  Void   addTupInst	Args((Class,Int));
#if EVAL_INSTANCES
extern  Void   addEvalInst	Args((Int,Cell,Int,List));
#endif
#if TREX
extern  Inst   addRecShowInst	Args((Class,Ext));
extern  Inst   addRecEqInst	Args((Class,Ext));
#endif
extern  Void   primDefn		Args((Cell,List,Cell));
extern  Void   defaultDefn	Args((Int,List));
extern  Void   checkExp		Args((Void));
extern  Void   checkDefns	Args((Void));
extern  Bool   h98Pred		Args((Bool,Cell));
extern  Cell   h98Context	Args((Bool,List));
extern  Void   h98CheckCtxt	Args((Int,String,Bool,List,Inst));
extern  Void   h98CheckType	Args((Int,String,Cell,Type));
extern  Void   h98DoesntSupport	Args((Int,String));

extern  Void   typeChecker      Args((Int));
extern  Type   typeCheckExp	Args((Bool));
extern  Void   typeCheckDefns	Args((Void));
extern  Cell   provePred	Args((Kinds,List,Cell));
extern  List   simpleContext	Args((List,Int));
extern  Cell   rhsExpr		Args((Cell));
extern  Int    rhsLine		Args((Cell));
extern  Bool   isProgType	Args((List,Type));
extern  Cell   superEvid	Args((Cell,Class,Class));
extern  Void   linkPreludeTC    Args((Void));
extern  Void   linkPreludeCM    Args((Void));

extern  Void   compiler         Args((Int));
extern  Void   compileDefns     Args((Void));
extern  Void   compileExp       Args((Void));
extern  Bool   failFree		Args((Cell));
extern  Int    discrArity       Args((Cell));

extern  Void   machine          Args((Int));
extern  Addr   codeGen          Args((Name,Int,Cell));
extern  Void   implementCfun	Args((Name,List));
#if TREX
extern  Name   implementRecShw  Args((Text,Cell));
extern  Name   implementRecEq   Args((Text,Cell));
#endif
extern  Void   addCfunTable	Args((Tycon));
extern  Name   succCfun		Args((Name));
extern  Name   nextCfun		Args((Name,Name));
extern  Name   cfunByNum	Args((Name,Int));
extern  Void   unwind           Args((Cell));
extern  Void   run              Args((Addr,StackPtr));

extern  Void   eval             Args((Cell));
extern  Cell   evalWithNoError  Args((Cell));
extern  Void   evalFails        Args((StackPtr));

#if BYTECODE_PRIMS
extern Int     IntAt            Args((Addr));
#if !BREAK_FLOATS
extern Float   FloatAt          Args((Addr));
#endif
extern Cell    CellAt           Args((Addr));
extern Text    TextAt           Args((Addr));
extern Addr    AddrAt           Args((Addr));
extern Int     InstrAt          Args((Addr));
#endif /* BYTECODE_PRIMS */

extern  Void   builtIn          Args((Int));
extern  Void   abandon		Args((String,Cell));
extern  Void   outputString	Args((FILE *));
extern  Void   dialogue		Args((Cell));
extern  Cell   consChar		Args((Char));
#if BIGNUMS
extern  Bignum bigInt		Args((Int));
extern  Bignum bigDouble	Args((double));
extern  Bignum bigNeg		Args((Bignum));
extern  Cell   bigToInt		Args((Bignum));
extern  Cell   bigToFloat	Args((Bignum));
extern  Bignum bigStr		Args((String));
extern  Cell   bigOut		Args((Bignum,Cell,Bool));
extern  Bignum bigShift		Args((Bignum,Int,Int));
extern  Int    bigCmp		Args((Bignum,Bignum));
#endif
#if IO_MONAD
extern Void   setHugsArgs       Args((Int,String[]));
#endif

extern  Void   machdep          Args((Int));
extern  String findPathname	Args((String,String));
extern  String findMPathname    Args((String,String,String));
#if PROFILING
extern  String timeString	Args((Void));
#endif

extern  Int    shellEsc		Args((String));
extern  Int    getTerminalWidth Args((Void));
extern  Void   normalTerminal	Args((Void));
extern  Void   noechoTerminal	Args((Void));
extern  Int    readTerminalChar Args((Void));
extern  Void   gcStarted	Args((Void));
extern  Void   gcScanning	Args((Void));
extern  Void   gcRecovered	Args((Int));
extern  Void   gcCStack		Args((Void));
extern  Void   needPrims        Args((Int)); 

extern  Void   plugins          Args((Int));
extern  Bool   havePlugin       Args((String));

/*-------------------------------------------------------------------------*/
