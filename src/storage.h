/* --------------------------------------------------------------------------
 * Defines storage datatypes: Text, Name, Module, Tycon, Cell, List, Pair,
 * Triple, ...
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the Oregon Graduate Institute of Science and
 * Technology, 1994-1999, All rights reserved.  It is distributed as
 * free software under the license in the file "License", which is
 * included in the distribution.
 *
 * $RCSfile: storage.h,v $
 * $Revision: 1.16 $
 * $Date: 2001/02/14 12:15:05 $
 * ------------------------------------------------------------------------*/

/* --------------------------------------------------------------------------
 * Typedefs for main data types:
 * Many of these type names are used to indicate the intended us of a data
 * item, rather than for type checking purposes.  Sadly (although sometimes,
 * fortunately), the C compiler cannot distinguish between the use of two
 * different names defined to be synonyms for the same types.
 * ------------------------------------------------------------------------*/

typedef Int	     Text;			 /* text string 	   */
typedef Unsigned     Syntax;			 /* syntax (assoc,preced)  */
typedef Int	     Addr;			 /* address of code	   */
typedef Int	     Cell;			 /* general cell value	   */
typedef Cell far     *Heap;			 /* storage of heap	   */
typedef Cell	     Pair;			 /* pair cell		   */
typedef Int	     StackPtr;			 /* stack pointer	   */
typedef Cell	     Offset;			 /* offset/generic variable*/
typedef Int	     Script;			 /* script file number	   */
typedef Int	     Module;			 /* module 	           */
typedef Cell	     Tycon;			 /* type constructor	   */
typedef Cell	     Type;			 /* type expression	   */
typedef Cell	     Kind;			 /* kind expression	   */
typedef Cell	     Kinds;			 /* list of kinds	   */
typedef Cell	     Constr;			 /* constructor expression */
typedef Cell	     Name;			 /* named value 	   */
typedef Void	     (*Prim) Args((StackPtr));	 /* primitive function	   */
typedef Cell	     Class;			 /* type class		   */
typedef Cell	     Inst;			 /* instance of type class */
typedef Cell	     Triple;			 /* triple of cell values  */
typedef Cell	     List;			 /* list of cells	   */
#if BIGNUMS
typedef Cell	     Bignum;			 /* bignum integer	   */
#endif
typedef FloatImpType Float;			 /* implementation of Float*/
#if TREX
typedef Cell	     Ext;			 /* extension label	   */
#endif
#if OBSERVATIONS
typedef Int          Observe;                    /* observation            */
typedef Int          Breakpt;                    /* Breakpoint             */
#endif

/* --------------------------------------------------------------------------
 * Text storage:
 * provides storage for the characters making up identifier and symbol
 * names, string literals, character constants etc...
 * ------------------------------------------------------------------------*/

extern	String	     textToStr		Args((Text));
extern	Text	     findText		Args((String));
extern	Text         inventText		Args((Void));
extern  Text	     inventDictText	Args((Void));
extern  Bool	     inventedText	Args((Text));
extern	String	     identToStr		Args((Cell));
extern	Text	     fixLitText	 	Args((Text));
extern	Text	     concatText		Args((String,String));

/* --------------------------------------------------------------------------
 * Specification of syntax (i.e. default written form of application)
 * ------------------------------------------------------------------------*/

#define MIN_PREC  0		       /* weakest binding operator	   */
#define MAX_PREC  9		       /* strongest binding operator	   */
#define FUN_PREC  (MAX_PREC+2)	       /* binding of function symbols	   */
#define DEF_PREC  MAX_PREC
#define APPLIC	  0	 	       /* written applicatively 	   */
#define LEFT_ASS  1	 	       /* left associative infix	   */
#define RIGHT_ASS 2	 	       /* right associative infix	   */
#define NON_ASS   3	 	       /* non associative infix 	   */
#define DEF_ASS   LEFT_ASS

#define UMINUS_PREC  6			/* Change these settings at your   */
#define UMINUS_ASSOC LEFT_ASS		/* own risk; they may not work!	   */

#define assocOf(x)	((x)&NON_ASS)
#define precOf(x)	((x)>>2)
#define mkSyntax(a,p)	((a)|((p)<<2))
#define DEF_OPSYNTAX	mkSyntax(DEF_ASS,DEF_PREC)
#define NO_SYNTAX	(-1)

/* --------------------------------------------------------------------------
 * Primitive functions:
 * ------------------------------------------------------------------------*/

extern struct primitive {		/* table of primitives		   */
    String ref;				/* primitive reference string	   */
    Int	   arity;			/* primitive function arity	   */
    Prim   imp;				/* primitive implementation	   */
} primitives[];

/* Modules that define new primitive functions must register a control
 * function (defining INSTALL, RESET, etc code) and a (null-terminated)
 * table of primitive functions.
 *
 * They are stored as a linked list - so there's no wired in limits. 
 * Control functions are called in the order they are registered
 * after all other control functions have been called.
 * (At the moment) there's no way of unregistering a module.
 */

struct primInfo {
    Void              (*controlFun) Args((Int));
    struct primitive  *primFuns;
    struct primInfo   *nextPrimInfo;
};

extern  Void registerPrims Args((struct primInfo*)); 
extern  Void controlFuns   Args((Int)); /* Call all control functions in   */
					/* prim list.                      */

/* --------------------------------------------------------------------------
 * Program code storage: for holding compiled function defns etc...
 * ------------------------------------------------------------------------*/

extern	Addr	     getMem	Args((Int));
extern  Void	     nextInstr	Args((Addr));

/* --------------------------------------------------------------------------
 * Heap storage:
 * Provides a garbage collectable heap for storage of expressions etc.
 * ------------------------------------------------------------------------*/

#define heapAlloc(s) (Heap)(farCalloc(s,sizeof(Cell)))
#define heapBuilt()  (heapFst)
extern  Int          heapSize;
extern  Heap	     heapFst, heapSnd;
#ifdef  GLOBALfst
register Heap	     heapTopFst GLOBALfst;
#else
extern   Heap        heapTopFst;
#endif
#ifdef  GLOBALsnd
register Heap	     heapTopSnd GLOBALsnd;
#else
extern   Heap        heapTopSnd;
#endif
extern   Bool        consGC;		/* Set to FALSE to turn off gc from*/
					/* C stack; use with extreme care! */
extern   Int	     cellsRecovered;	/* cells recovered by last gc	   */

#define fst(c)	     heapTopFst[c]
#define snd(c)	     heapTopSnd[c]
#if PROFILING
extern   Heap	     heapThd, heapTopThd;
#define thd(c)	     heapTopThd[c]
extern   Name	     producer;
extern   Bool        profiling;
extern   Int 	     profInterval;
extern   Void	     profilerLog     Args((String));
#endif

extern	Pair	     pair	     Args((Cell,Cell));
extern  Void         garbageCollect  Args((Void));

extern  Void	     overwrite	     Args((Pair,Pair));
extern  Cell	     markExpr	     Args((Cell));
extern  Void	     markWithoutMove Args((Cell));

#define mark(v)      v=markExpr(v)

#define isPair(c)    ((c)<0)
#define isGenPair(c) ((c)<0 && -heapSize<=(c))

extern	Cell	     whatIs    Args((Cell));

/* --------------------------------------------------------------------------
 * Box cell tags are used as the fst element of a pair to indicate that
 * the snd element of the pair is to be treated in some special way, other
 * than as a Cell.  Examples include holding integer values, variable name
 * and string text etc.
 * ------------------------------------------------------------------------*/

#define TAGMIN	     1		  /* Box and constructor cell tag values   */
#define BCSTAG	     30 	  /* Box=TAGMIN..BCSTAG-1		   */
#define isTag(c)     (TAGMIN<=(c) && (c)<SPECMIN) /* Tag cell values	   */
#define isBoxTag(c)  (TAGMIN<=(c) && (c)<BCSTAG)  /* Box cell tag values   */
#define isConTag(c)  (BCSTAG<=(c) && (c)<SPECMIN) /* Constr cell tag values*/

#define INDIRECT     1		  /* Indirection node:	      snd :: Cell  */
#define INDIRECT1    2		  /* Temporary indirection:   snd :: Cell  */
#define FREECELL     3		  /* Free list cell:	      snd :: Cell  */
#define VARIDCELL    4		  /* Identifier variable:     snd :: Text  */
#define VAROPCELL    5		  /* Operator variable:       snd :: Text  */
#define DICTVAR	     6		  /* Dictionary variable:     snd :: Text  */
#define CONIDCELL    7		  /* Identifier constructor:  snd :: Text  */
#define CONOPCELL    8		  /* Operator constructor:    snd :: Text  */
#define STRCELL      9		  /* String literal:	      snd :: Text  */
#define INTCELL	     10		  /* Int literal:	      snd :: Int   */
#if NPLUSK
#define ADDPAT	     11		  /* (_+k) pattern discr:     snd :: Int   */
#endif
#if IO_HANDLES
#define HANDCELL     12		  /* IO monad Handle:	      snd :: Int   */
#endif
#if GC_WEAKPTRS
#define WEAKCELL     13		  /* WeakPtr Cell:	      snd::(Cell,List)*/
#define WEAKFIN	     14		  /* Weak pointer with fin:   snd::(k,(v,f))*/
#endif
#if !BREAK_FLOATS
#define FLOATCELL    15		  /* Floating pt number:      snd :: Float */
#endif
#if TREX
#define EXTCOPY	     16		  /* Copy of an Ext:	      snd :: Text  */
#endif
#if GC_MALLOCPTRS
#define MPCELL       17		  /* Malloc Ptr Cell:         snd :: Int   */
#endif
#if SIZEOF_INTP == SIZEOF_INT
#define PTRCELL      18           /* C Heap Pointer:          snd ::Pointer*/
#endif
#if IPARAM
#define IPCELL       19		  /* Imp Param Cell:	      snd :: Text  */
#define IPVAR	     20		  /* ?x:		      snd :: Text  */
#endif

#define textOf(c)	((Text)(snd(c)))         /* c ::  (VAR|CON)(ID|OP) */
#define qmodOf(c)       (textOf(fst(snd(c))))    /* c ::  QUALIDENT        */
#define qtextOf(c)      (textOf(snd(snd(c))))    /* c ::  QUALIDENT        */
#define mkVar(t)	ap(VARIDCELL,t)
#define mkVarop(t)	ap(VAROPCELL,t)
#define mkCon(t)	ap(CONIDCELL,t)
#define mkConop(t)	ap(CONOPCELL,t)
#define mkQVar(m,t)	ap(QUALIDENT,pair(mkCon(m),mkVar(t)))
#define mkQCon(m,t)	ap(QUALIDENT,pair(mkCon(m),mkCon(t)))
#define mkQVarOp(m,t)	ap(QUALIDENT,pair(mkCon(m),mkVarop(t)))
#define mkQConOp(m,t)	ap(QUALIDENT,pair(mkCon(m),mkConop(t)))
#define intValOf(c)	(snd(c))
#define inventVar()	mkVar(inventText())
#define mkDictVar(t)	ap(DICTVAR,t)
#define inventDictVar() mkDictVar(inventDictText())
#define mkStr(t)	ap(STRCELL,t)
#if IPARAM
#define mkIParam(c)	ap(IPCELL,snd(c))
#define isIP(p)		(whatIs(p) == IPCELL)
#define ipMatch(pi, t)	(isIP(fun(pi)) && textOf(fun(pi)) == t)
#define ipVar(pi)	textOf(fun(pi))
#else
#define isIP(p)		FALSE
#endif

extern	Bool		isVar       Args((Cell));
extern	Bool		isCon       Args((Cell));
extern	Bool		isQVar      Args((Cell));
extern	Bool		isQCon      Args((Cell));
extern	Bool		isQualIdent Args((Cell));
extern	Bool		isIdent     Args((Cell));

#define isFloat(c)      (isPair(c) && fst(c)==FLOATCELL)
extern	Cell	        mkFloat		Args((FloatPro));
extern  FloatPro	floatOf		Args((Cell));
extern  String		floatToString   Args((FloatPro));
extern  FloatPro	stringToFloat   Args((String));
#if BREAK_FLOATS
extern  Cell		part1Float	Args((FloatPro));
extern  Cell		part2Float	Args((FloatPro));
extern  FloatPro	floatFromParts	Args((Cell,Cell));
#endif

#define isPtr(c)        (isPair(c) && fst(c)==PTRCELL)
extern  Cell            mkPtr           Args((Pointer));
extern  Pointer         ptrOf           Args((Cell));

/* --------------------------------------------------------------------------
 * Constructor cell tags are used as the fst element of a pair to indicate
 * a particular syntactic construct described by the snd element of the
 * pair.
 * Note that a cell c will not be treated as an application (AP/isAp) node
 * if its first element is a constructor cell tag, whereas a cell whose fst
 * element is a special cell will be treated as an application node.
 * ------------------------------------------------------------------------*/

#define LETREC	     30 	  /* LETREC	snd :: ([Decl],Exp)	   */
#define COND	     31 	  /* COND	snd :: (Exp,Exp,Exp)	   */
#define LAMBDA	     32 	  /* LAMBDA	snd :: Alt		   */
#define FINLIST      33 	  /* FINLIST	snd :: [Exp]		   */
#define DOCOMP       34 	  /* DOCOMP	snd :: (Exp,[Qual])	   */

#if MUDO
#define MDOCOMP	     44		  /* MDOCOMP	snd :: (Exp,[Qual])	   */
#endif 

#define BANG	     35		  /* BANG	snd :: Type		   */
#define COMP         36 	  /* COMP	snd :: (Exp,[Qual])	   */
#define ASPAT	     37 	  /* ASPAT	snd :: (Var,Exp)	   */
#define ESIGN	     38 	  /* ESIGN	snd :: (Exp,Type)	   */
#define RSIGN	     39 	  /* RSIGN	snd :: (Rhs,Type)	   */
#define CASE	     40 	  /* CASE	snd :: (Exp,[Alt])	   */
#define NUMCASE	     41		  /* NUMCASE	snd :: (Exp,Disc,Rhs)	   */
#define FATBAR	     42 	  /* FATBAR	snd :: (Exp,Exp)	   */
#define LAZYPAT      43 	  /* LAZYPAT	snd :: Exp		   */
#define DERIVE	     45		  /* DERIVE	snd :: Cell		   */
#if BREAK_FLOATS
#define FLOATCELL    46		  /* FLOATCELL  snd :: (Int,Int)	   */
#endif

#if BIGNUMS
#define POSNUM	     47		  /* POSNUM	snd :: [Int]		   */
#define NEGNUM	     48		  /* NEGNUM	snd :: [Int]		   */
#endif

#define BOOLQUAL     49 	  /* BOOLQUAL	snd :: Exp		   */
#define QWHERE	     50 	  /* QWHERE	snd :: [Decl]		   */
#define FROMQUAL     51 	  /* FROMQUAL	snd :: (Exp,Exp)	   */
#define DOQUAL	     52		  /* DOQUAL	snd :: Exp		   */
#define MONADCOMP    53 	  /* MONADCOMP	snd :: ((m,m0),(Exp,[Qual])*/

#define GUARDED      54 	  /* GUARDED	snd :: [guarded exprs]	   */

#define ARRAY        55		  /* Array	snd :: (Bounds,[Values])   */
#define MUTVAR	     56		  /* Mutvar	snd :: Cell		   */
#if INTERNAL_PRIMS
#define HUGSOBJECT   57		  /* HUGSOBJECT	snd :: Cell	           */
#endif

#if STABLE_NAMES
#define STABLENAME   58		  /* Stable Nm  snd :: Cell		   */
#endif

#if IPARAM
#define WITHEXP      59 	  /* WITH	snd :: [(Var,Exp)]	   */
#endif

#define POLYTYPE     60		  /* POLYTYPE	snd :: (Kind,Type)	   */
#define QUAL	     61		  /* QUAL	snd :: ([Classes],Type)	   */
#define RANK2	     62		  /* RANK2	snd :: (Int,Type)	   */
#define EXIST	     63		  /* EXIST	snd :: (Int,Type)	   */
#define POLYREC	     64		  /* POLYREC	snd :: (Int,Type)	   */
#define BIGLAM	     65		  /* BIGLAM	snd :: (vars,patterns)	   */
#define CDICTS	     66		  /* CDICTS	snd :: ([Pred],Type)	   */

#define LABC	     70		  /* LABC	snd :: (con,[(Vars,Type)]) */
#define CONFLDS	     71		  /* CONFLDS	snd :: (con,[Field])	   */
#define UPDFLDS	     72		  /* UPDFLDS	snd :: (Exp,[con],[Field]) */
#if TREX
#define RECORD	     73		  /* RECORD	snd :: [Val]		   */
#define EXTCASE	     74		  /* EXTCASE	snd :: (Exp,Disc,Rhs)	   */
#define RECSEL	     75		  /* RECSEL	snd :: Ext		   */
#endif
#define IMPDEPS	     78		  /* IMPDEPS	snd :: [Binding]	   */

#define QUALIDENT    80           /* Qualified identifier  snd :: (Id,Id)  */
#define HIDDEN       81           /* hiding import list    snd :: [Entity] */
#define MODULEENT    82           /* module in export list snd :: con      */

#if OBSERVATIONS
#define OBSERVEHEAD  83           /* obs. list; snd ::(first,last)         */
#define OBSERVE      84           /* observe marker; snd :: (Cell,observe) */
#define OBSERVESTK   85           /* observe marker on stack               */
#endif

#define INFIX	     90		  /* INFIX	snd :: (see tidyInfix)	   */
#define ONLY	     91		  /* ONLY	snd :: Exp		   */
#define NEG	     92		  /* NEG	snd :: Exp		   */

#if ZIP_COMP
#define ZCOMP        93 	  /* ZCOMP	snd :: (Exp,[[Qual]])	   */
#endif

#if SIZEOF_INTP != SIZEOF_INT
#define PTRCELL      99           /* C Heap Pointer snd :: (Int,Int)       */
#endif

/* Last constructor tag must be less than SPECMIN */

/* --------------------------------------------------------------------------
 * Special cell values:
 * ------------------------------------------------------------------------*/

#define SPECMIN      101
#define isSpec(c)    (SPECMIN<=(c) && (c)<TUPMIN)/* Special cell values    */

#define NONE	     101	  /* Dummy stub				   */
#define STAR	     102	  /* Representing the kind of types	   */
#if TREX
#define ROW	     103	  /* Representing the kind of rows	   */
#endif
#define WILDCARD     104	  /* Wildcard pattern			   */

#define SKOLEM	     105	  /* Skolem constant			   */

#define DOTDOT       106          /* ".." in import/export list            */

#if BIGNUMS
#define ZERONUM      108	  /* The zero bignum (see POSNUM, NEGNUM)  */
#endif

#define NAME	     110	  /* whatIs code for isName		   */
#define TYCON	     111	  /* whatIs code for isTycon		   */
#define CLASS	     112	  /* whatIs code for isClass		   */
#define MODULE       113          /* whatIs code for isModule              */
#define INSTANCE     114          /* whatIs code for isInst                */
#define TUPLE	     115	  /* whatIs code for tuple constructor	   */
#define OFFSET	     116	  /* whatis code for offset		   */
#define AP	     117	  /* whatIs code for application node	   */
#define CHARCELL     118	  /* whatIs code for isChar		   */
#if TREX
#define EXT	     119	  /* whatIs code for isExt		   */
#endif

#define SIGDECL      120	  /* Signature declaration		   */
#define FIXDECL      121	  /* Fixity declaration			   */
#define FUNBIND	     122	  /* Function binding			   */
#define PATBIND	     123	  /* Pattern binding			   */

#define DATATYPE     130	  /* Datatype type constructor		   */
#define NEWTYPE	     131	  /* Newtype type constructor		   */
#define SYNONYM	     132	  /* Synonym type constructor		   */
#define RESTRICTSYN  133	  /* Synonym with restricted scope	   */

#define NODEPENDS    135	  /* Stop calculation of deps in type check*/
#define PREDEFINED   136	  /* Predefined name, not yet filled       */

/* --------------------------------------------------------------------------
 * Tuple data/type constructors:
 * ------------------------------------------------------------------------*/

#define TUPMIN	     201
#if TREX
#define isTuple(c)   (TUPMIN<=(c) && (c)<EXTMIN)
#else
#define isTuple(c)   (TUPMIN<=(c) && (c)<OFFMIN)
#endif
#define mkTuple(n)   (TUPMIN+(n))
#define tupleOf(n)   ((Int)((n)-TUPMIN))

#if TREX
#define EXTMIN	     (TUPMIN+NUM_TUPLES)
#define isExt(c)     (EXTMIN<=(c) && (c)<OFFMIN)
#define extText(e)   tabExt[(e)-EXTMIN]
#define extField(c)  arg(fun(c))
#define extRow(c)    arg(c)

extern Text	     DECTABLE(tabExt);
extern Ext           mkExt Args((Text));
#else
#define mkExt(t) NIL
#endif

/* --------------------------------------------------------------------------
 * Offsets: (generic types/stack offsets)
 * ------------------------------------------------------------------------*/

#if TREX
#define OFFMIN	     (EXTMIN+NUM_EXT)
#else
#define OFFMIN	     (TUPMIN+NUM_TUPLES)
#endif
#define isOffset(c)  (OFFMIN<=(c) && (c)<MODMIN)
#define offsetOf(c)  ((c)-OFFMIN)
#define mkOffset(o)  (OFFMIN+(o))

/* --------------------------------------------------------------------------
 * Observations
 * ------------------------------------------------------------------------*/

#if OBSERVATIONS
#define OBSMIN	      (OFFMIN+NUM_OFFSETS)
#define observe(n)    tabObserve[(n)-OBSMIN]

struct Observe {
	Text tag;		/* tag in observe primitive		*/
	Cell head;
};

#define firstObs(c) snd3(c)
#define lastObs(c)  thd3(c)

#define nextObs(c)  fst3(c)
#define seqObs(c)   snd3(c)
#define exprObs(c)  thd3(c)

#define markedExpr(c) snd3(c)
#define markedObs(c)  thd3(c)

extern struct    Observe          DECTABLE(tabObserve);
extern Observe   firstObserve	  Args((Void));
extern Observe   nextObserve	  Args((Void));
extern Void      clearObserve	  Args((Void));
extern Cell      addObsInstance	  Args((String,Cell,Int));
extern Void      insertAfterObs   Args((Cell,Cell));

/* --------------------------------------------------------------------------
 * Breakpoints
 * ------------------------------------------------------------------------*/

#define BRKMIN	      (OBSMIN+NUM_OBS_TAGS)
#define breakpt(n)    tabBreakpt[(n)-BRKMIN]

struct Breakpt {
	Text tag;
	Bool enabled;
	Int  count;			/* number of breaks to skip 	   */
};

extern Void     clearAllBreak   Args((Void));
extern Bool     breakNow	Args((String));
extern Void     setBreakpt	Args((String,Bool)); 
extern Void     setBreakCount   Args((String,Int));
#endif

/* --------------------------------------------------------------------------
 * Modules:
 * ------------------------------------------------------------------------*/

#if OBSERVATIONS
#define MODMIN        (BRKMIN+NUM_BRKPTS)
#else
#define MODMIN        (OFFMIN+NUM_OFFSETS)
#endif

#if IGNORE_MODULES
#define setCurrModule(m) doNothing()
#else /* !IGNORE_MODULES */
#define isModule(c)   (MODMIN<=(c) && (c)<TYCMIN)
#define mkModule(n)   (MODMIN+(n))
#define module(n)     tabModule[(n)-MODMIN]

/* Under Haskell 1.3, the list of qualified imports is always a subset
 * of the list of unqualified imports.  For simplicity and flexibility,
 * we do not attempt to exploit this fact - when a module is imported
 * unqualified, it is added to both the qualified and unqualified
 * import lists.
 * Similarily, Haskell 1.3 does not allow a constructor to be imported
 * or exported without exporting the type it belongs to but the export
 * list is just a flat list of Texts (before static analysis) or
 * Tycons, Names and Classes (after static analysis).
 */
struct Module {
    Text  text;
    /* Lists of top level objects (local defns + imports)                  */
    List  tycons;
    List  names;
    List  classes;
    List  exports; /* [ Entity | (Entity, NIL|DOTDOT) ] */
    /* List of qualified imports.  Used both during compilation and when
     * evaluating an expression in the context of the current module.
     */
    List  qualImports;
};

extern Module currentModule;           /* Module currently being processed */
extern struct Module DECTABLE(tabModule);

extern Bool   isValidModule Args((Module));
extern Module newModule	    Args((Text));
extern Module findModule    Args((Text));
extern Module findModid     Args((Cell));
extern Void   setCurrModule Args((Module));

#define isPrelude(m) (m==modulePrelude)
#endif /* !IGNORE_MODULES */

/* --------------------------------------------------------------------------
 * Type constructor names:
 * ------------------------------------------------------------------------*/

#define TYCMIN	     (MODMIN+NUM_MODULE)
#define isTycon(c)   (TYCMIN<=(c) && (c)<NAMEMIN)
#define mkTycon(n)   (TCMIN+(n))
#define tycon(n)     tabTycon[(n)-TYCMIN]

struct strTycon {
    Text  text;
    Int   line;
#if !IGNORE_MODULES
    Module mod;                         /* module that defines it          */
#endif
    Int   arity;
    Kind  kind;				/* kind (includes arity) of Tycon  */
    Cell  what;				/* DATATYPE/SYNONYM/RESTRICTSYN... */
    Cell  defn;
    Tycon nextTyconHash;
};

extern struct strTycon DECTABLE(tabTycon);

extern Tycon newTycon	  Args((Text));
extern Tycon findTycon	  Args((Text));
extern Tycon addTycon	  Args((Tycon));
extern Tycon findQualTycon Args((Cell));
extern Tycon addPrimTycon Args((Text,Kind,Int,Cell,Cell));

#define isSynonym(h)	(isTycon(h) && tycon(h).what==SYNONYM)
#define isQualType(t)	(isPair(t) && fst(t)==QUAL)
#define mkPolyType(n,t)	pair(POLYTYPE,pair(n,t))
#define isPolyType(t)	(isPair(t) && fst(t)==POLYTYPE)
#define isPolyOrQualType(t) (isPair(t) && (fst(t)==POLYTYPE || fst(t)==QUAL))
#define polySigOf(t)	fst(snd(t))
#define monotypeOf(t)	snd(snd(t))

/* --------------------------------------------------------------------------
 * Globally defined name values:
 * ------------------------------------------------------------------------*/

#define NAMEMIN      (TYCMIN+NUM_TYCON)
#define isName(c)    (NAMEMIN<=(c) && (c)<INSTMIN)
#define mkName(n)    (NAMEMIN+(n))
#define name(n)      tabName[(n)-NAMEMIN]

struct strName {
    Text text;
    Int  line;
#if !IGNORE_MODULES
    Module mod;                         /* module that defines it          */
#endif
    Syntax syntax;
    Cell parent;                        /* enclosing object                */
					/* :: Class | Instance | Name | NIL*/
    Int  arity;
    Int  number;
    Cell type;
    Cell defn;
    Addr code;
    Text callconv;                      /* for foreign import/export       */
    Text extFun;                        /* for foreign import/export       */
    Prim primDef;
    Name nextNameHash;
#if PROFILING
    Int  count;
#endif
};

extern struct strName DECTABLE(tabName);

/* The number field in a name is used to distinguish various kinds of name:
 *   mfunNo(i) = code for member function, offset i
 *		 members that are sole elements of dict use mfunNo(0)
 *		 members of dicts with more than one elem use mfunNo(n), n>=1
 *   EXECNAME  = code for executable name (bytecodes or primitive)
 *   SELNAME   = code for selector function
 *   DFUNNAME  = code for dictionary builder or selector
 *   cfunNo(i) = code for data constructor
 *		 datatypes with only one constructor uses cfunNo(0)
 *		 datatypes with multiple constructors use cfunNo(n), n>=1
 */

#define EXECNAME	0
#define SELNAME		1
#define DFUNNAME	2
#define CFUNNAME	3

#define isSfun(n)	(name(n).number==SELNAME)
#define isDfun(n)	(name(n).number==DFUNNAME)

#define isCfun(n)	(name(n).number>=CFUNNAME)
#define cfunOf(n)	(name(n).number-CFUNNAME)
#define cfunNo(i)	((i)+CFUNNAME)
#define hasCfun(cs)	(nonNull(cs) && isCfun(hd(cs)))

#define isMfun(n)	(name(n).number<0)
#define mfunOf(n)	((-1)-name(n).number)
#define mfunNo(i)	((-1)-(i))

extern Name   newName	   Args((Text,Cell));
extern Name   findName	   Args((Text));
extern Name   addName	   Args((Name));
extern Name   findQualName Args((Cell));
extern Name   findQualFun  Args((Text,Text));
extern Void   addPrim	   Args((Int,Name,String,Module,Type));
extern Name   addPrimCfun  Args((Text,Int,Int,Cell));
extern Int    sfunPos	   Args((Name,Name));

/* --------------------------------------------------------------------------
 * Type class values:
 * ------------------------------------------------------------------------*/

#define INSTMIN      (NAMEMIN+NUM_NAME)	/* instances			   */
#define isInst(c)    (INSTMIN<=(c) && (c)<CLASSMIN)
#define mkInst(n)    (INSTMIN+(n))
#define instOf(c)    ((Int)((c)-INSTMIN))
#define inst(in)     tabInst[(in)-INSTMIN]

struct strInst {
    Class c;				/* class C			   */
    Int   line;
    Kinds kinds;			/* Kinds of variables in head	   */
    Cell  head;				/* :: Pred			   */
    List  specifics;			/* :: [Pred]			   */
    Int   numSpecifics;			/* length(specifics)		   */
    List  implements;
    Name  builder;			/* Dictionary constructor function */
};

/* a predicate (an element :: Pred) is an application of a Class to one or
 * more type expressions
 */

#define CLASSMIN     (INSTMIN+NUM_INSTS)
#define isClass(c)   (CLASSMIN<=(c) && (c)<CHARMIN)
#define mkClass(n)   (CLASSMIN+(n))
#define cclass(n)    tabClass[(n)-CLASSMIN]

struct strClass {
    Text   text;			/* Name of class		   */
    Int    line;			/* Line where declaration begins   */
#if !IGNORE_MODULES
    Module mod;				/* module that declares it         */
#endif
    Int    level;			/* Level in class hierarchy	   */
    Int    arity;			/* Number of arguments		   */
    Kinds  kinds;			/* Kinds of constructors in class  */
    Cell   head;			/* Head of class		   */
    List   fds;				/* Functional Dependencies	   */
    List   xfds;			/* Xpanded Functional Dependencies */
    Name   dcon;			/* Dictionary constructor function */
    List   supers;			/* :: [Pred]			   */
    Int    numSupers;			/* length(supers)		   */
    List   dsels;			/* Superclass dictionary selectors */
    List   members;			/* :: [Name]			   */
    Int    numMembers;			/* length(members)		   */
    List   defaults;			/* :: [Name]			   */
    List   instances;			/* :: [Inst]			   */
};

extern struct strClass    DECTABLE(tabClass);
extern struct strInst far *tabInst;

extern Class newClass	   Args((Text));
extern Class classMax	   Args((Void));
extern Class findClass	   Args((Text));
extern Class addClass	   Args((Class));
extern Class findQualClass Args((Cell));
extern Inst  newInst	   Args((Void));
extern Inst  findFirstInst Args((Tycon));
extern Inst  findNextInst  Args((Tycon,Inst));

/* --------------------------------------------------------------------------
 * Character values:
 * ------------------------------------------------------------------------*/

#define CHARMIN      (CLASSMIN+NUM_CLASSES)
#define MAXCHARVAL   (NUM_CHARS-1)
#define isChar(c)    (CHARMIN<=(c) && (c)<INTMIN)
#define charOf(c)    ((Char)(c-CHARMIN))
#define mkChar(c)    ((Cell)(CHARMIN+(((unsigned)(c))%NUM_CHARS)))

/* --------------------------------------------------------------------------
 * Small Integer values:
 * ------------------------------------------------------------------------*/

#define INTMIN	     (CHARMIN+NUM_CHARS)
#define INTMAX	     (MAXPOSINT)
#define isSmall(c)   (INTMIN<=(c))
#define INTZERO      (INTMIN/2 + INTMAX/2)
#define MINSMALLINT  (INTMIN - INTZERO)
#define MAXSMALLINT  (INTMAX - INTZERO)
#define mkDigit(c)   ((Cell)((c)+INTMIN))
#define digitOf(c)   ((Int)((c)-INTMIN))

extern	Bool isInt    Args((Cell));
extern	Int  intOf    Args((Cell));
extern	Cell mkInt    Args((Int));
#if BIGNUMS
extern  Bool isBignum Args((Cell));
#endif

/* --------------------------------------------------------------------------
 * Implementation of triples:
 * ------------------------------------------------------------------------*/

#define triple(x,y,z) pair(x,pair(y,z))
#define fst3(c)      fst(c)
#define snd3(c)      fst(snd(c))
#define thd3(c)      snd(snd(c))

/* --------------------------------------------------------------------------
 * Implementation of lists:
 * ------------------------------------------------------------------------*/

#define NIL	     0
#define isNull(c)    ((c)==NIL)
#define nonNull(c)   (c)
#define cons(x,xs)   pair(x,xs)
#define singleton(x) cons(x,NIL)
#define doubleton(x,y) cons(x,cons(y,NIL))
#define hd(c)	     fst(c)
#define tl(c)	     snd(c)

extern	Int	     length	  Args((List));
extern	List	     appendOnto   Args((List,List));	/* destructive	   */
extern	List	     dupOnto      Args((List,List));
extern  List	     dupList	  Args((List));
extern  List	     dupUpto	  Args((List,Cell));
extern	List	     revOnto	  Args((List, List));	/* destructive	   */
#define rev(xs)      revOnto((xs),NIL)			/* destructive	   */
extern	Cell	     cellIsMember Args((Cell,List));
extern  Cell         cellAssoc    Args((Cell,List));
extern  Cell         cellRevAssoc Args((Cell,List));
extern	List	     concat	  Args((List));
extern	List	     intersect	  Args((List,List));
extern	Cell	     varIsMember  Args((Text,List));
extern	Name	     nameIsMember Args((Text,List));
extern  Cell	     intIsMember  Args((Int,List));
extern	List	     replicate	  Args((Int,Cell));
extern	List	     diffList	  Args((List,List));	/* destructive	   */
extern  List         deleteCell   Args((List,Cell));	/* non-destructive */
extern  List	     take	  Args((Int,List));	/* destructive	   */
extern  List	     splitAt	  Args((Int,List));	/* non-destructive */
extern  Cell	     nth	  Args((Int,List));
extern  List	     removeCell	  Args((Cell,List));	/* destructive	   */

/* The following macros provide `inline expansion' of some common ways of
 * traversing, using and modifying lists:
 *
 * N.B. We use the names _f, _a, _xs, Zs, in an attempt to avoid clashes
 *	with identifiers used elsewhere.
 */

#define mapBasic(_init,_step)		{List Zs=(_init);\
					 for(;nonNull(Zs);Zs=tl(Zs))  \
					 _step;}
#define mapModify(_init,_step)		mapBasic(_init,hd(Zs)=_step)

#define mapProc(_f,_xs)			mapBasic(_xs,_f(hd(Zs)))
#define map1Proc(_f,_a,_xs)		mapBasic(_xs,_f(_a,hd(Zs)))
#define map2Proc(_f,_a,_b,_xs)		mapBasic(_xs,_f(_a,_b,hd(Zs)))
#define map3Proc(_f,_a,_b,_c,_xs)	mapBasic(_xs,_f(_a,_b,_c,hd(Zs)))
#define map4Proc(_f,_a,_b,_c,_d,_xs)	mapBasic(_xs,_f(_a,_b,_c,_d,hd(Zs)))

#define mapOver(_f,_xs)			mapModify(_xs,_f(hd(Zs)))
#define map1Over(_f,_a,_xs)		mapModify(_xs,_f(_a,hd(Zs)))
#define map2Over(_f,_a,_b,_xs)		mapModify(_xs,_f(_a,_b,hd(Zs)))
#define map3Over(_f,_a,_b,_c,_xs)	mapModify(_xs,_f(_a,_b,_c,hd(Zs)))
#define map4Over(_f,_a,_b,_c,_d,_xs)	mapModify(_xs,_f(_a,_b,_c,_d,hd(Zs)))

/* This is just what you want for functions with accumulating parameters */
#define mapAccum(_f,_acc,_xs) 	        mapBasic(_xs,_acc=_f(_acc,hd(Zs)))
#define map1Accum(_f,_acc,_a,_xs)	mapBasic(_xs,_acc=_f(_acc,_a,hd(Zs)))
#define map2Accum(_f,_acc,_a,_b,_xs)	mapBasic(_xs,_acc=_f(_acc,_a,_b,hd(Zs)))
#define map3Accum(_f,_acc,_a,_b,_c,_xs) mapBasic(_xs,_acc=_f(_acc,_a,_b,_c,hd(Zs)))

/* --------------------------------------------------------------------------
 * Implementation of function application nodes:
 * ------------------------------------------------------------------------*/

#define ap(f,x)      pair(f,x)
#define ap1(f,x)     ap(f,x)
#define ap2(f,x,y)   ap(ap(f,x),y)
#define ap3(f,x,y,z) ap(ap(ap(f,x),y),z)
#define fun(c)	     fst(c)
#define arg(c)	     snd(c)
#define isAp(c)      (isPair(c) && !isTag(fst(c)))
extern	Cell	     getHead	 Args((Cell));
extern	List	     getArgs	 Args((Cell));
extern	Int	     argCount;
extern  Cell         nthArg	 Args((Int,Cell));
extern  Int	     numArgs	 Args((Cell));
extern  Cell	     applyToArgs Args((Cell,List));

/* --------------------------------------------------------------------------
 * Stack implementation:
 *
 * NB: Use of macros makes order of evaluation hard to predict.
 *     For example, "push(1+pop());" doesn't increment TOS.
 * ------------------------------------------------------------------------*/

extern  Cell DECTABLE(cellStack);
#ifdef  GLOBALsp
register StackPtr    sp GLOBALsp;
#else
extern	StackPtr sp;
#endif

#define clearStack() sp=(-1)
#define stackEmpty() (sp==(-1))
#define stack(p)     cellStack[p]
#define chkStack(n)  if (sp>=NUM_STACK-(n)) hugsStackOverflow()
#define push(c)      \
  do {               \
    chkStack(1);     \
    onto(c);         \
  } while (0)
#define onto(c)	     stack(++sp)=(c)
#define pop()	     stack(sp--)
#define drop()	     sp--
#define top()	     stack(sp)
#define pushed(n)    stack(sp-(n))
#define topfun(f)    top()=ap((f),top())
#define toparg(x)    top()=ap(top(),(x))

extern  Void hugsStackOverflow Args((Void));

#if __MWERKS__ && macintosh
#include <Memory.h>
#define STACK_HEADROOM 16384
#define STACK_CHECK if (StackSpace() <= STACK_HEADROOM) \
		      internal("Macintosh function parameter stack overflow.");
#else
#define STACK_CHECK
#endif

/* --------------------------------------------------------------------------
 * Script file control:
 * The implementation of script file storage is hidden.
 * ------------------------------------------------------------------------*/

extern Script	   startNewScript   Args((String));
extern Bool        moduleThisScript Args((Module));
extern Module      moduleOfScript   Args((Script));
extern Bool        isPreludeScript  Args((Void));
extern Module      lastModule       Args((Void));
extern Script	   scriptThisFile   Args((Text));
extern Script	   scriptThisName   Args((Name));
extern Script	   scriptThisTycon  Args((Tycon));
extern Script	   scriptThisInst   Args((Inst));
extern Script	   scriptThisClass  Args((Class));
extern String      fileOfModule     Args((Module));
extern Void	   dropScriptsFrom  Args((Script));

/* --------------------------------------------------------------------------
 * I/O Handles:
 * ------------------------------------------------------------------------*/

#if IO_HANDLES
#define HSTDIN		0	/* Numbers for standard handles		   */
#define HSTDOUT		1
#define HSTDERR		2

struct strHandle {		/* Handle description and status flags	   */
    Cell hcell;			/* Heap representation of handle (or NIL)  */
    FILE *hfp;			/* Corresponding file pointer		   */
    Int  hmode;			/* Current mode: see below		   */
};

#define HCLOSED		0000	/* no I/O permitted			   */
#define HSEMICLOSED     0001	/* semiclosed reads only		   */
#define HREAD		0002	/* set to enable reads from handle 	   */
#define HWRITE		0004	/* set to enable writes to handle	   */
#define HAPPEND		0010	/* opened in append mode		   */

extern Cell   openHandle Args((String,Int,Bool));
extern struct strHandle  DECTABLE(handles);
#endif

/* --------------------------------------------------------------------------
 * Malloc Pointers
 * ------------------------------------------------------------------------*/

#if GC_MALLOCPTRS
struct strMallocPtr {		/* Malloc Ptr description                  */
    Cell mpcell;		/* Back pointer to MPCELL                  */
    Void *ptr;			/* Pointer into C world                    */
    Int  refCount;              /* Reference count                         */
    Void (*cleanup) Args((Void *)); /* Code to free the C pointer          */
};

extern struct strMallocPtr       mallocPtrs[];
extern Cell   mkMallocPtr        Args((Void *, Void (*)(Void *)));
extern Void   freeMallocPtr      Args((Cell));
extern Void   incMallocPtrRefCnt Args((Int, Int));

#define mpOf(c)	   snd(c)
#define derefMP(c) (mallocPtrs[(Int)mpOf(c)].ptr)
#endif /* GC_MALLOCPTRS */

#if GC_WEAKPTRS
/* --------------------------------------------------------------------------
 * Weak Pointers
 * ------------------------------------------------------------------------*/

#define mkWeakPtr(c)    pair(WEAKCELL,pair(c,NIL))
#define derefWeakPtr(c) fst(snd(c))
#define nextWeakPtr(c) snd(snd(c))

extern List finalizers;
extern List liveWeakPtrs;

#endif /* GC_WEAKPTRS */

/* --------------------------------------------------------------------------
 * Stable pointers
 * ------------------------------------------------------------------------*/

#if GC_STABLEPTRS
extern  Int  mkStablePtr     Args((Cell));
extern  Cell derefStablePtr  Args((Int));
extern  Void freeStablePtr   Args((Int));
#endif

/* --------------------------------------------------------------------------
 * Plugins
 * ------------------------------------------------------------------------*/

/* This is an exact copy of the declaration found in GreenCard.h */

typedef int     HugsStackPtr;
typedef int     HugsStablePtr;
typedef Pointer HugsForeign;

typedef struct {

  /* evaluate next argument */
  int            (*getInt   )     Args(());  
  unsigned int   (*getWord  )     Args(());
  void*     	 (*getAddr  )     Args(());
  float     	 (*getFloat )     Args(());
  double    	 (*getDouble)     Args(());
  char      	 (*getChar  )     Args(());
  HugsForeign    (*getForeign)    Args(());
  HugsStablePtr  (*getStablePtr)  Args(());

  /* push part of result   */
  void      	 (*putInt   )     Args((int));           
  void      	 (*putWord  )     Args((unsigned int));
  void      	 (*putAddr  )     Args((void*));
  void      	 (*putFloat )     Args((double));
  void      	 (*putDouble)     Args((double));
  void      	 (*putChar  )     Args((char));
  void      	 (*putForeign)    Args((HugsForeign, void (*)(HugsForeign)));
  void      	 (*putStablePtr)  Args((HugsStablePtr));

  /* return n values in IO monad or Id monad */
  void      	 (*returnIO)      Args((HugsStackPtr, int));
  void      	 (*returnId)      Args((HugsStackPtr, int));
  int      	 (*runIO)         Args((int));

  /* free a stable pointer */	    			 
  void      	 (*freeStablePtr) Args((HugsStablePtr));

  /* register the prim table */	    			 
  void      	 (*registerPrims) Args((struct primInfo*));
			   
  /* garbage collect */
  void		 (*garbageCollect) Args(());

  /* API3 additions follow */
  HugsStablePtr  (*lookupName)     Args((char*, char*));
  void           (*ap)             Args((int));
  void           (*getUnit)        Args(());
  void*          (*mkThunk)        Args((void*, HugsStablePtr));
  void           (*freeThunk)      Args((void*));
  int     	 (*getBool)        Args(());
  void      	 (*putBool)        Args((int));

} HugsAPI3;

extern  HugsAPI3* hugsAPI3     Args((Void));
typedef Void (*InitModuleFun3) Args((HugsAPI3*));

typedef struct {

  /* evaluate next argument */
  int            (*getInt   )     Args(());  
  unsigned int   (*getWord  )     Args(());
  void*     	 (*getAddr  )     Args(());
  float     	 (*getFloat )     Args(());
  double    	 (*getDouble)     Args(());
  char      	 (*getChar  )     Args(());
  HugsForeign    (*getForeign)    Args(());
  HugsStablePtr  (*getStablePtr)  Args(());

  /* push part of result   */
  void      	 (*putInt   )     Args((int));           
  void      	 (*putWord  )     Args((unsigned int));
  void      	 (*putAddr  )     Args((void*));
  void      	 (*putFloat )     Args((double));
  void      	 (*putDouble)     Args((double));
  void      	 (*putChar  )     Args((char));
  void      	 (*putForeign)    Args((HugsForeign, void (*)(HugsForeign)));
  void      	 (*putStablePtr)  Args((HugsStablePtr));

  /* return n values in IO monad or Id monad */
  void      	 (*returnIO)      Args((HugsStackPtr, int));
  void      	 (*returnId)      Args((HugsStackPtr, int));
  int      	 (*runIO)         Args((int));

  /* free a stable pointer */	    			 
  void      	 (*freeStablePtr) Args((HugsStablePtr));

  /* register the prim table */	    			 
  void      	 (*registerPrims) Args((struct primInfo*));
			   
  /* garbage collect */
  void		 (*garbageCollect) Args(());

} HugsAPI2;

extern  HugsAPI2* hugsAPI2     Args((Void));
typedef Void (*InitModuleFun2) Args((HugsAPI2*));

typedef struct {
  Name  nameTrue, nameFalse;
  Name  nameNil,  nameCons;
  Name  nameJust, nameNothing;
  Name  nameLeft, nameRight;
  Name  nameUnit;
  Name  nameIORun;

  Cell  (*makeInt)         Args((Int));
			   
  Cell  (*makeChar)        Args((Char));
  Char  (*CharOf)          Args((Cell));
			   
  Cell  (*makeFloat)       Args((FloatPro));
  Cell  (*makeTuple)       Args((Int));
  Pair  (*pair)            Args((Cell,Cell));
			   
  Cell  (*mkMallocPtr)     Args((Void *, Void (*)(Void *)));
  Void *(*derefMallocPtr)  Args((Cell));
			   
  Int   (*mkStablePtr)     Args((Cell));
  Cell  (*derefStablePtr)  Args((Int));
  Void  (*freeStablePtr)   Args((Int));
			   
  Void  (*eval)            Args((Cell));
  Cell  (*evalWithNoError) Args((Cell));
  Void  (*evalFails)       Args((StackPtr));
  Int   *whnfArgs;	   
  Cell  *whnfHead;	   
  Int   *whnfInt;	   
  Float *whnfFloat;	   
			   
  Void  (*garbageCollect)  Args(());
  Void  (*stackOverflow)   Args(());
  Void  (*internal)        Args((String)) HUGS_noreturn;

  Void  (*registerPrims)   Args((struct primInfo*));
  Name  (*addPrimCfun)     Args((Text,Int,Int,Cell));
  Text  (*inventText)      Args(());

  Cell *(*Fst)             Args((Cell));
  Cell *(*Snd)             Args((Cell));

  Cell  *cellStack;
  StackPtr *sp;
} HugsAPI1;

extern  HugsAPI1* hugsAPI1     Args((Void));
typedef Void (*InitModuleFun1) Args((HugsAPI1*));

/* --------------------------------------------------------------------------
 * Misc:
 * ------------------------------------------------------------------------*/

extern  Void   setLastExpr       Args((Cell));
extern  Cell   getLastExpr       Args((Void));
extern  List   addTyconsMatching Args((String,List));
extern  List   addNamesMatching  Args((String,List));

/*-------------------------------------------------------------------------*/
