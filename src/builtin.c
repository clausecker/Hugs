/* --------------------------------------------------------------------------
 * Primitive functions, input output etc...
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2002, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 * $RCSfile: builtin.c,v $
 * $Revision: 1.37 $
 * $Date: 2002/11/29 13:20:29 $
 * ------------------------------------------------------------------------*/

/* We include math.h before prelude.h because SunOS 4's cpp incorrectly
 * reports an error if you use "name(n)" as a macro with arguments and
 * "name" as a normal identifier (with no arguments).  ADR
 */
#include <math.h>
#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"
#include <ctype.h>
#if HAVE_IO_H
#include <io.h>
#endif

/* Header files needed to compile the IO primitives */
#ifdef IO_MONAD
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#else
# ifdef HAVE_TYPES_H
#  include <types.h>
# endif
#endif
#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#else
# ifdef HAVE_STAT_H
#  include <stat.h>
# endif
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifndef __MINGW32__
# ifdef HAVE_SYS_TIMES_H
#  include <sys/times.h>
# endif
#endif

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#ifndef __MINGW32__
# if defined(HAVE_SYS_RESOURCE_H)
#  include <sys/resource.h>
# endif
#endif

#ifdef HAVE_ERRNO_H
# include <errno.h>
#endif

#ifdef HAVE_SYS_TIMEB_H
#include <sys/timeb.h>
#endif

#ifdef HAVE_WINDOWS_H
# include <windows.h>
#endif

#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif

#ifdef HAVE_DIRECT_H
#include <direct.h>
#endif

#endif /* IO_MONAD */

Name nameNegate,  nameFlip;             /* primitives reqd for parsing     */
Name nameFrom,    nameFromThen;
Name nameFromTo,  nameFromThenTo;
Name nameFatbar,  nameFail;             /* primitives reqd for translation */
Name nameIf,      nameSel;
Name nameId,      nameOtherwise;
Name nameConCmp,  nameEnRange;          /* primitives used for deriv inst  */
Name nameEnIndex, nameEnInRng;
Name nameEnToEn,  nameEnFrEn;
Name nameEnFrom,  nameEnFrTh;
Name nameEnFrTo;
Name nameBlackHole;                     /* for GC-detected black hole      */
Name nameInd;				/* for dict indirection		   */
Name namePrint,   nameNPrint;           /* primitives for printing         */
#if EVAL_INSTANCES
Name nameIStrict, nameISeq;             /* primitives for strictness       */
#endif

Name nameFst,     nameSnd;              /* 2-tuple selector functions      */
Name nameAnd,     nameOr;               /* built-in logical connectives    */
Name namePrimThrow;                     /* throw primitive function        */
Name nameError;                         /* error primitive function        */
Name nameUndefined;                     /* generic undefined value         */
Name nameComp;                          /* function composition            */
Name nameApp;                           /* list append                     */
Name nameShowField;                     /* display single field            */
Name nameShowParen;                     /* wrap with parens                */
Name nameReadField;                     /* read single field               */
Name nameReadParen;                     /* unwrap from parens              */
Name nameLex;                           /* lexer                           */
Name nameRangeSize;                     /* calculate size of index range   */
Name nameCompAux;                       /* auxiliary function for compares */
Name namePmInt,   namePmFlt;            /* primitives for pattern matching */
Name nameReturnIO;
Name namePmInteger;
#if NPLUSK
Name namePmNpk,   namePmSub;            /* primitives for (n+k) patterns   */
#endif
#if TREX
Name nameRecExt,  nameRecBrk;           /* Extend and break a record       */
Name nameRecSel,  nameRecShw;           /* Select and show a record        */
Name nameRecEq;				/* Compare records		   */
Name nameAddEv;				/* Add up evidence		   */
#endif

#if SHORT_CIRCUIT_COERCIONS
Name nameRationalToFloat;
Name nameFloatToRational;
Name nameDoubleToRational;
Name nameDoubleToRatio;
Name nameIntToRatio;
Name nameIntToFloat;
#endif

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

#define PROTO_PRIM(name)      static Void name Args((StackPtr))
#define primFun(name)         static Void name(root) StackPtr root;
#define primCAF(name)         static Void name(root) StackPtr root HUGS_unused;
#define primArg(n)            stack(root+n)

/* IMPORTANT: the second element of an update must be written first.
 * this is to deal with the case where an INDIRECT tag is written into
 * a Cell before the second value has been set.  If a garbage collection
 * occurs before the second element was set then the INDIRECTion will be
 * (wrongly) elided and result in chaos.  I know.  It happened to me.
 */

#define update(l,r)             ((snd(stack(root))=r),(fst(stack(root))=l))
#define updateRoot(c)           update(INDIRECT,c)
#define updapRoot(l,r)          update(l,r)
#define blackHoleRoot()         update(nameBlackHole,nameBlackHole)
#define cantReduce()            evalFails(root)

#if CHECK_TAGS

# define checkChar()  if (!isChar(whnfHead))  internal("Char expected")
# define checkInt()   if (!isInt(whnfHead))   internal("Int expected")
# define checkWord()  if (!isInt(whnfHead))   internal("Word expected")
# define checkPtr()   if (!isPtr(whnfHead))   internal("Ptr expected")
# define checkFloat() if (!isFloat(whnfHead)) internal("Float expected")

# define checkBool()  if (whnfHead != nameTrue && whnfHead != nameFalse) internal("Bool expected");

# define checkCon()   if (!isName(whnfHead) || !isCfun(whnfHead)) internal("Constructor expected");

#else

# define checkChar()  doNothing()
# define checkInt()   doNothing()
# define checkWord()  doNothing()
# define checkPtr()   doNothing()
# define checkFloat() doNothing()
# define checkBool()  doNothing()
# define checkCon()   doNothing()

#endif

/* e is a constant expression                     */
#define CAFPtr(nm,e)                               \
  primCAF(nm) {                                    \
      Pointer r = e;                               \
      push(mkPtr(r));                              \
  }

#define PtrArg(nm,offset)                          \
    eval(primArg(offset));                         \
    checkPtr();                                    \
    nm = ptrOf(whnfHead)

/* nm should be a variable in which result is stored.
   If you use an expression, reevaluation might occur */
#define PtrResult(nm)                              \
   updateRoot(mkPtr(nm))

/* e is an expression with free variables x and y */
#define PtrInt2Ptr(nm,e)                           \
  primFun(nm) {                                    \
    Pointer x, r;                                  \
    Int y;                                         \
    PtrArg(x,2);                                   \
    IntArg(y,1);                                   \
    r = e;                                         \
    PtrResult(r);                                  \
}

/* e is an expression with free variables x */
#define Ptr2Int(nm,e)                              \
  primFun(nm) {                                    \
    Pointer x;				   	   \
    Int r;					   \
    PtrArg(x,1);                                   \
    r = e;                                         \
    IntResult(r);                                  \
}

/* e is a constant expression                     */
#define CAFInt(nm,e)                               \
  primCAF(nm) {                                    \
      Int r = e;                                   \
      push(mkInt(r));                              \
  }

#define IntArg(nm,offset)                          \
    eval(primArg(offset));                         \
    checkInt();                                    \
    nm = whnfInt

/* nm should be a variable in which result is stored.
   If you use an expression, reevaluation might occur */
#define IntResult(nm)                              \
   updateRoot(mkInt(nm))

/* nm should be a variable in which result is stored.
   If you use an expression, reevaluation might occur */
#define IntIntResult(e1,e2)                        \
   do {                                            \
       Int _arg1 = e1;                             \
       Int _arg2 = e2;                             \
       push(mkInt(_arg1));                         \
       topfun(mkTuple(2));                         \
       updapRoot(top(),mkInt(_arg2));              \
   } while (0)

/* e is a constant expression                     */
#define CAFWord(nm,e)                              \
  primCAF(nm) {                                    \
      Unsigned r = e;                              \
      push(mkInt(r));                              \
  }

#define WordArg(nm,offset)                         \
    eval(primArg(offset));                         \
    checkWord();                                   \
    nm = (Unsigned) whnfInt

/* nm should be a variable in which result is stored.
   If you use an expression, reevaluation might occur */
#define WordResult(nm)                             \
   updateRoot(mkInt(nm))

/* nm should be a variable in which result is stored.
   If you use an expression, reevaluation might occur */
#define WordWordResult(e1,e2)                      \
   do {                                            \
       Unsigned _arg1 = e1;                        \
       Unsigned _arg2 = e2;                        \
       push(mkInt(_arg1));                         \
       topfun(mkTuple(2));                         \
       updapRoot(top(),mkInt(_arg2));              \
   } while (0)

#define FloatArg(nm,offset)                        \
    eval(primArg(offset));                         \
    checkFloat();                                  \
    nm = whnfFloat

/* nm should be a variable in which result is stored.
   If you use an expression, reevaluation might occur */
#define FloatResult(nm)                            \
   updateRoot(mkFloat(nm))

#define BoolArg(nm, offset)                        \
   eval(primArg(offset));                          \
   checkBool();                                    \
   nm = (whnfHead == nameTrue)

/* e can be an expression if you want */
#define BoolResult(e)                              \
   updateRoot((e) ? nameTrue : nameFalse)
   
#define ConArg(nm,offset)                          \
    eval(primArg(offset));                         \
    checkCon();                                    \
    nm = cfunOf(whnfHead)                          \

/* e is an expression with free variables x and y */
#define IntInt2Int(nm,e)                           \
  primFun(nm) {                                    \
    Int x, y, r;                                   \
    IntArg(x,2);                                   \
    IntArg(y,1);                                   \
    r = e;                                         \
    IntResult(r);                                  \
}

/* e is a predicate with free variables x and y   */
#define PtrPtr2Bool(nm,e)                          \
  primFun(nm) {                                    \
    Pointer x, y;                                  \
    PtrArg(x,2);                                   \
    PtrArg(y,1);                                   \
    BoolResult(e);                                 \
}


/* e is an expression with free variables x and y */
/* pre is a precondition (fvs x,y) to test        */
#define IntInt2IntPre(nm,e,pre)                    \
  primFun(nm) {                                    \
    Int x, y, r;                                   \
    IntArg(x,2);                                   \
    IntArg(y,1);                                   \
    if (!(pre)) cantReduce();                      \
    r = e;                                         \
    IntResult(r);                                  \
}

/* e is an expression with free variable x        */
#define Int2Int(nm,e)                              \
  primFun(nm) {                                    \
    Int x, r;                                      \
    IntArg(x,1);                                   \
    r = e;                                         \
    IntResult(r);                                  \
}

/* e is a predicate with free variables x and y   */
#define IntInt2Bool(nm,e)                          \
  primFun(nm) {                                    \
    Int x, y;                                      \
    IntArg(x,2);                                   \
    IntArg(y,1);                                   \
    BoolResult(e);                                 \
}

/* e is a predicate with free variable x          */
#define Int2Bool(nm,e)                             \
  primFun(nm) {                                    \
    Int x;                                         \
    IntArg(x,1);                                   \
    BoolResult(e);                                 \
}

/* e is an expression with free variables x and y */
#define WordWord2Word(nm,e)                        \
  primFun(nm) {                                    \
    Unsigned x, y, r;                              \
    WordArg(x,2);                                  \
    WordArg(y,1);                                  \
    r = e;                                         \
    WordResult(r);                                 \
}

/* e is an expression with free variables x and y */
/* pre is a precondition (fvs x,y) to test        */
#define WordWord2WordPre(nm,e,pre)                 \
  primFun(nm) {                                    \
    Unsigned x, y, r;                              \
    WordArg(x,2);                                  \
    WordArg(y,1);                                  \
    if (!(pre)) cantReduce();                      \
    r = e;                                         \
    WordResult(r);                                 \
}

/* e is an expression with free variable x        */
#define Word2Word(nm,e)                            \
  primFun(nm) {                                    \
    Unsigned x, r;                                 \
    WordArg(x,1);                                  \
    r = e;                                         \
    WordResult(r);                                 \
}
/* e is an expression with free variable x        */
#define Word2Word(nm,e)                            \
  primFun(nm) {                                    \
    Unsigned x, r;                                 \
    WordArg(x,1);                                  \
    r = e;                                         \
    WordResult(r);                                 \
}

/* e is a predicate with free variables x and y   */
#define WordWord2Bool(nm,e)                        \
  primFun(nm) {                                    \
    Unsigned x, y;                                 \
    WordArg(x,2);                                  \
    WordArg(y,1);                                  \
    BoolResult(e);                                 \
}

/* e is a predicate with free variables x and y   */
#define WordInt2Bool(nm,e)                         \
  primFun(nm) {                                    \
    Unsigned x;                                    \
    Int y;                                         \
    WordArg(x,2);                                  \
    IntArg(y,1);                                   \
    BoolResult(e);                                 \
}

/* e is a predicate with free variables x and y   */
#define WordInt2Word(nm,e)                         \
  primFun(nm) {                                    \
    Unsigned x;                                    \
    Int y;                                         \
    Unsigned r;                                    \
    WordArg(x,2);                                  \
    IntArg(y,1);                                   \
    r = e;                                         \
    WordResult(r);                                 \
}

/* e is a predicate with free variable x          */
#define Int2Word(nm,e)                             \
  primFun(nm) {                                    \
    Int x;                                         \
    Unsigned r;                                    \
    IntArg(x,1);                                   \
    r = e;                                         \
    WordResult(r);                                 \
}

/* e is a predicate with free variable x          */
#define Word2Bool(nm,e)                            \
  primFun(nm) {                                    \
    Unsigned x;                                    \
    WordArg(x,1);                                  \
    BoolResult(e);                                 \
}

/* e is an expression with free variables x and y */
#define FloatFloat2Float(nm,e)                     \
  primFun(nm) {                                    \
    Float x, y, r;                                 \
    FloatArg(x,2);                                 \
    FloatArg(y,1);                                 \
    r = e;                                         \
    FloatResult(r);                                \
}

/* e is an expression with free variables x and y */
/* pre is a precondition (fvs x,y) to test        */
#define FloatFloat2FloatPre(nm,e,pre)              \
  primFun(nm) {                                    \
    Float x, y, r;                                 \
    FloatArg(x,2);                                 \
    FloatArg(y,1);                                 \
    if (!(pre)) cantReduce();                      \
    r = e;                                         \
    FloatResult(r);                                \
}

/* e is an expression with free variable x        */
#define Float2Float(nm,e)                          \
  primFun(nm) {                                    \
    Float x, r;                                    \
    FloatArg(x,1);                                 \
    r = (Float)e;                                  \
    FloatResult(r);                                \
}

/* e is an expression with free variable x        */
#define Float2Int(nm,e)                            \
  primFun(nm) {                                    \
    Float x;                                       \
    Int r;                                         \
    FloatArg(x,1);                                 \
    r = e;                                         \
    IntResult(r);                                  \
}

/* e is an expression with free variable x        */
#define Float2Bool(nm,e)                           \
  primFun(nm) {                                    \
    Float x;                                       \
    Bool r;                                        \
    FloatArg(x,1);                                 \
    r = e;                                         \
    BoolResult(r);                                 \
}

/* e is an expression with free variable x        */
/* pre is a precondition (fv x) to test           */
#define Float2FloatPre(nm,e,pre)                   \
  primFun(nm) {                                    \
    Float x, r;                                    \
    FloatArg(x,1);                                 \
    if (!(pre)) cantReduce();                      \
    r = (Float)e;                                  \
    FloatResult(r);                                \
}

#define CharArg(nm,offset)                         \
    eval(primArg(offset));                         \
    checkChar();                                   \
    nm = charOf(whnfHead)

/* nm should be a variable in which result is stored.
   If you use an expression, reevaluation might occur */
#define CharResult(nm)                             \
   updateRoot(mkChar(nm))
   

/* e is a predicate with free variables x and y   */
#define CharChar2Bool(nm,e)                        \
  primFun(nm) {                                    \
    Cell x, y;                                     \
    CharArg(x,2);                                  \
    CharArg(y,1);                                  \
    BoolResult(e);                                 \
}

/* e is a predicate with free variables x and y   */
#define FloatFloat2Bool(nm,e)                      \
  primFun(nm) {                                    \
    Float x, y;                                    \
    FloatArg(x,2);                                 \
    FloatArg(y,1);                                 \
    BoolResult(e);                                 \
}

PROTO_PRIM(primFatbar);
PROTO_PRIM(primFail);
PROTO_PRIM(primCatchError);
PROTO_PRIM(primThrowException);
PROTO_PRIM(primCatchException);
PROTO_PRIM(primSel);
PROTO_PRIM(primIf);
PROTO_PRIM(primStrict);
PROTO_PRIM(primSeq);
PROTO_PRIM(primTrace);
PROTO_PRIM(primConCmp);
PROTO_PRIM(primEnRange);
PROTO_PRIM(primEnIndex);
PROTO_PRIM(primEnInRng);
PROTO_PRIM(primEnFrEn);
PROTO_PRIM(primEnToEn);
PROTO_PRIM(primEnFrom);
PROTO_PRIM(primEnFrTh);
PROTO_PRIM(primEnFrTo);

PROTO_PRIM(primMinInt);
PROTO_PRIM(primMaxInt);
PROTO_PRIM(primPlusInt);
PROTO_PRIM(primMinusInt);
PROTO_PRIM(primMulInt);
PROTO_PRIM(primDivInt);
PROTO_PRIM(primQuotInt);
PROTO_PRIM(primModInt);
PROTO_PRIM(primRemInt);
PROTO_PRIM(primQrmInt);
PROTO_PRIM(primNegInt);
PROTO_PRIM(primEvenInt);

PROTO_PRIM(primAndInt);
PROTO_PRIM(primOrInt);
PROTO_PRIM(primXorInt);
PROTO_PRIM(primComplementInt);
PROTO_PRIM(primShiftInt);
PROTO_PRIM(primBitInt);
PROTO_PRIM(primTestInt);

PROTO_PRIM(primCharToInt);
PROTO_PRIM(primIntToChar);
PROTO_PRIM(primWord32ToInt);
PROTO_PRIM(primIntToWord32);
PROTO_PRIM(primIntToFloat);
PROTO_PRIM(primDummyCvt);

PROTO_PRIM(primRationalToFloat);

#if WORD_OPS
PROTO_PRIM(primMaxWord);
PROTO_PRIM(primPlusWord);
PROTO_PRIM(primMinusWord);
PROTO_PRIM(primNegateWord);
PROTO_PRIM(primMulWord);
PROTO_PRIM(primDivWord);
PROTO_PRIM(primQuotWord);
PROTO_PRIM(primModWord);
PROTO_PRIM(primRemWord);
PROTO_PRIM(primQrmWord);
PROTO_PRIM(primEvenWord);

PROTO_PRIM(primAndWord);
PROTO_PRIM(primOrWord);
PROTO_PRIM(primXorWord);
PROTO_PRIM(primComplementWord);
PROTO_PRIM(primShiftWord);
PROTO_PRIM(primRotateWord);
PROTO_PRIM(primBitWord);
PROTO_PRIM(primTestWord);

PROTO_PRIM(primEqInt32);
PROTO_PRIM(primCmpInt32);

PROTO_PRIM(primI32toI);
PROTO_PRIM(primI32toI8);
PROTO_PRIM(primI32toI16);
PROTO_PRIM(primI32toI64);

PROTO_PRIM(primItoI32);
PROTO_PRIM(primI8toI32);
PROTO_PRIM(primI16toI32);
PROTO_PRIM(primI64toI32);

PROTO_PRIM(primW32toW8);
PROTO_PRIM(primW32toW16);
PROTO_PRIM(primW32toW64);

PROTO_PRIM(primW8toW32);
PROTO_PRIM(primW16toW32);
PROTO_PRIM(primW64toW32);
#endif

PROTO_PRIM(primFreeHFunPtr);

PROTO_PRIM(primPlusFloat);
PROTO_PRIM(primMinusFloat);
PROTO_PRIM(primMulFloat);
PROTO_PRIM(primDivFloat);
PROTO_PRIM(primNegFloat);

#ifdef HAVE_LIBM
PROTO_PRIM(primSinFloat);
PROTO_PRIM(primCosFloat);
PROTO_PRIM(primTanFloat);
PROTO_PRIM(primAsinFloat);
PROTO_PRIM(primAcosFloat);
PROTO_PRIM(primAtanFloat);
#if 0 /* Not used in current Prelude */
PROTO_PRIM(primAtan2Float);
#endif
PROTO_PRIM(primExpFloat);
PROTO_PRIM(primLogFloat);
PROTO_PRIM(primSqrtFloat);
PROTO_PRIM(primFloatToInt);
PROTO_PRIM(primFloatRadix);
PROTO_PRIM(primFloatDigits);
PROTO_PRIM(primFloatMinExp);
PROTO_PRIM(primFloatMaxExp);
PROTO_PRIM(primFloatDecode);
PROTO_PRIM(primFloatEncode);
#endif /* HAVE_LIBM */

PROTO_PRIM(primNullAddr);
PROTO_PRIM(primPlusAddr);
PROTO_PRIM(primAddrToInt);
PROTO_PRIM(primEqAddr);

PROTO_PRIM(primEqInt);
PROTO_PRIM(primCmpInt);

PROTO_PRIM(primEqWord);
PROTO_PRIM(primCmpWord);

PROTO_PRIM(primEqChar);
PROTO_PRIM(primCmpChar);

PROTO_PRIM(primEqFloat);
PROTO_PRIM(primCmpFloat);

#if TREX
PROTO_PRIM(primRecExt);
PROTO_PRIM(primRecBrk);
PROTO_PRIM(primRecSel);
PROTO_PRIM(primRecShw);
PROTO_PRIM(primRecEq);
#endif

#if OBSERVATIONS
PROTO_PRIM(primObserve);
PROTO_PRIM(primBkpt);
PROTO_PRIM(primSetBkpt);
#endif

PROTO_PRIM(primPtrEq);
PROTO_PRIM(primPtrToInt);

static String local evalName            Args((Cell));
static Cell local followInd Args(( Cell ));

/* --------------------------------------------------------------------------
 * Table of primitive/built-in values:
 * ------------------------------------------------------------------------*/

static struct primitive builtinPrimTable[] = {
  {"fatbar",            2, primFatbar},
  {"fail",              0, primFail},
  {"catchError",        1, primCatchError},
  {"primThrowException",1, primThrowException},
  {"primCatchException",1, primCatchException},
  {"gcBhole",           0, primFail},
  {"error",             1, primFail},
  {"sel",               3, primSel},
  {"if",                3, primIf},
  {"trace",             2, primTrace},
  {"conCmp",            2, primConCmp},
  {"enRange",           1, primEnRange},
  {"enIndex",           2, primEnIndex},
  {"enInRng",           2, primEnInRng},
  {"enToEn",            2, primEnToEn},
  {"enFrEn",            1, primEnFrEn},
  {"enFrom",            1, primEnFrom},
  {"enFrTh",            2, primEnFrTh},
  {"enFrTo",            2, primEnFrTo},

  {"primMinInt",        0, primMinInt},
  {"primMaxInt",        0, primMaxInt},
  {"primPlusInt",       2, primPlusInt},
  {"primMinusInt",      2, primMinusInt},
  {"primMulInt",        2, primMulInt},
  {"primDivInt",        2, primDivInt},
  {"primQuotInt",       2, primQuotInt},
  {"primModInt",        2, primModInt},
  {"primRemInt",        2, primRemInt},
  {"primNegInt",        1, primNegInt},
  {"primEvenInt",       1, primEvenInt},
  {"primQrmInt",        2, primQrmInt},

  {"primAndInt",        2, primAndInt},
  {"primOrInt",         2, primOrInt},
  {"primXorInt",        2, primXorInt},
  {"primComplementInt", 1, primComplementInt},
  {"primShiftInt",      2, primShiftInt},
  {"primBitInt",        1, primBitInt},
  {"primTestInt",       2, primTestInt},

#if WORD_OPS
  {"primMaxWord",       0, primMaxWord},
  {"primPlusWord",      2, primPlusWord},
  {"primMinusWord",     2, primMinusWord},
  {"primNegateWord",    1, primNegateWord},
  {"primMulWord",       2, primMulWord},
  {"primDivWord",       2, primDivWord},
  {"primQuotWord",      2, primQuotWord},
  {"primModWord",       2, primModWord},
  {"primRemWord",       2, primRemWord},
  {"primEvenWord",      1, primEvenWord},
  {"primQrmWord",       2, primQrmWord},

  {"primAndWord",       2, primAndWord},
  {"primOrWord",        2, primOrWord},
  {"primXorWord",       2, primXorWord},
  {"primComplementWord",1, primComplementWord},
  {"primShiftWord",     2, primShiftWord},
  {"primRotateWord",    3, primRotateWord},
  {"primBitWord",       1, primBitWord},
  {"primTestWord",      2, primTestWord},

  {"primEqInt32",       2, primEqInt32},
  {"primCmpInt32",      2, primCmpInt32},

  {"primInt32ToInt",    1, primI32toI},
  {"primInt32ToInt8",   1, primI32toI8},
  {"primInt32ToInt16",  1, primI32toI16},
  {"primInt32ToInt64",  2, primI32toI64},

  {"primIntToInt32",    1, primItoI32},
  {"primInt8ToInt32",   1, primI8toI32},
  {"primInt16ToInt32",  1, primI16toI32},
  {"primInt64ToInt32",  1, primI64toI32},

  {"primWord32ToWord8", 1, primW32toW8},
  {"primWord32ToWord16",1, primW32toW16},
  {"primWord32ToWord64",2, primW32toW64},
                        
  {"primWord8ToWord32", 1, primW8toW32},
  {"primWord16ToWord32",1, primW16toW32},
  {"primWord64ToWord32",1, primW64toW32},
#endif

  {"freeHaskellFunPtr", 3, primFreeHFunPtr},

#if !BIGNUMS                            /* Implement Integer as Int        */
  {"primPlusInteger",   2, primPlusInt},
  {"primMinusInteger",  2, primMinusInt},
  {"primMulInteger",    2, primMulInt},
  {"primQrmInteger",    2, primQrmInt},
  {"primNegInteger",    1, primNegInt},
  {"primIntToInteger",  1, primDummyCvt},
  {"primIntegerToInt",  1, primDummyCvt},
  {"primIntegerToFloat",1, primIntToFloat},
  {"primIntegerToDouble",1,primIntToFloat},
  {"primEqInteger",     2, primEqInt},
  {"primCmpInteger",    2, primCmpInt},
#endif

  {"primPlusFloat",     2, primPlusFloat},
  {"primMinusFloat",    2, primMinusFloat},
  {"primMulFloat",      2, primMulFloat},
  {"primDivFloat",      2, primDivFloat},
  {"primNegFloat",      1, primNegFloat},

  {"primPlusDouble",    2, primPlusFloat},      /* Currently Float */
  {"primMinusDouble",   2, primMinusFloat},     /* Currently Float */
  {"primMulDouble",     2, primMulFloat},       /* Currently Float */
  {"primDivDouble",     2, primDivFloat},       /* Currently Float */
  {"primNegDouble",     1, primNegFloat},       /* Currently Float */

#ifdef HAVE_LIBM
  {"primSinFloat",      1, primSinFloat},
  {"primCosFloat",      1, primCosFloat},
  {"primTanFloat",      1, primTanFloat},
  {"primAsinFloat",     1, primAsinFloat},
  {"primAcosFloat",     1, primAcosFloat},
  {"primAtanFloat",     1, primAtanFloat},
  {"primExpFloat",      1, primExpFloat},
  {"primLogFloat",      1, primLogFloat},
  {"primSqrtFloat",     1, primSqrtFloat},
  {"primFloatToInt",    1, primFloatToInt},
  {"primFloatRadix",    0, primFloatRadix},
  {"primFloatDigits",   0, primFloatDigits},
  {"primFloatMinExp",   0, primFloatMinExp},
  {"primFloatMaxExp",   0, primFloatMaxExp},
  {"primFloatDecode",   1, primFloatDecode},
  {"primFloatEncode",   2, primFloatEncode},

  {"primSinDouble",     1, primSinFloat},       /* Currently Float */
  {"primCosDouble",     1, primCosFloat},       /* Currently Float */
  {"primTanDouble",     1, primTanFloat},       /* Currently Float */
  {"primAsinDouble",    1, primAsinFloat},      /* Currently Float */
  {"primAcosDouble",    1, primAcosFloat},      /* Currently Float */
  {"primAtanDouble",    1, primAtanFloat},      /* Currently Float */
  {"primExpDouble",     1, primExpFloat},       /* Currently Float */
  {"primLogDouble",     1, primLogFloat},       /* Currently Float */
  {"primSqrtDouble",    1, primSqrtFloat},      /* Currently Float */
  {"primDoubleToInt",   1, primFloatToInt},     /* Currently Float */
  {"primDoubleRadix",   0, primFloatRadix},     /* Currently Float */
  {"primDoubleDigits",  0, primFloatDigits},    /* Currently Float */
  {"primDoubleMinExp",  0, primFloatMinExp},    /* Currently Float */
  {"primDoubleMaxExp",  0, primFloatMaxExp},    /* Currently Float */
  {"primDoubleDecode",  1, primFloatDecode},    /* Currently Float */
  {"primDoubleEncode",  2, primFloatEncode},    /* Currently Float */
#endif

  {"primIntToChar",     1, primIntToChar},
  {"primCharToInt",     1, primCharToInt},
  {"intToWord32",       1, primIntToWord32},
  {"word32ToInt",       1, primWord32ToInt},
  {"primIntToFloat",    1, primIntToFloat},
  {"primIntToDouble",   1, primIntToFloat},     /* Currently Float */
  {"doubleToFloat",     1, primDummyCvt},       /* dummy           */
  {"floatToDouble",     1, primDummyCvt},       /* dummy           */

  {"primRationalToFloat",  1, primRationalToFloat},
  {"primRationalToDouble", 1, primRationalToFloat},

  {"nullAddr",          0, primNullAddr},
  {"addrToInt",         1, primAddrToInt},
  {"plusAddr",          2, primPlusAddr},
  {"primEqAddr",        2, primEqAddr},

  {"primEqInt",         2, primEqInt},
  {"primCmpInt",        2, primCmpInt},
  {"primEqWord",        2, primEqWord},
  {"primCmpWord",       2, primCmpWord},
  {"primEqChar",        2, primEqChar},
  {"primCmpChar",       2, primCmpChar},
  {"primEqFloat",       2, primEqFloat},
  {"primCmpFloat",      2, primCmpFloat},
  {"primEqDouble",      2, primEqFloat},        /* Currently Float */
  {"primCmpDouble",     2, primCmpFloat},       /* Currently Float */

  {"primUnsafeCoerce",  1, primDummyCvt},       /* breaks the type system */

  {"strict",            2, primStrict},
  {"seq",               2, primSeq},

#if TREX
  {"recExt",            3, primRecExt},
  {"recBrk",            2, primRecBrk},
  {"recSel",            2, primRecSel},
  {"recShw",            5, primRecShw},
  {"recEq",		6, primRecEq},
#endif		        

#if OBSERVATIONS
  {"observe",           2, primObserve},
  {"bkpt",              2, primBkpt},
  {"setBkpt",           4, primSetBkpt},
#endif

  {"unsafePtrEq",       2, primPtrEq},          /* breaks the semantics  */
  {"unsafePtrToInt",    1, primPtrToInt},       /* breaks the semantics  */

  {0,                   0, 0}
};

/* --------------------------------------------------------------------------
 * Primitive functions:
 *
 * IMPORTANT NOTICE: the primitive function definitions in this file
 * should be written in a style that permits correct execution *without*
 * conservative garbage collection (i.e., without marking from the C stack).
 * Adding primitive definitions that do not meet this requirement may
 * corrupt the heap and lead to failed execution; do not modify this code
 * unless you are really confident about what you are doing.
 *
 * Some general guidelines follow, using c, e to denote expressions that
 * involve either at most 1 allocation, or the possibility/certainty of
 * multiple allocations, resp.
 *
 * push(c);             Ok.
 * push(e);             Bad -- intermediate result may be lost if GC occurs
 *                      in the middle of building e; break e into steps, and
 *                      use toparg(), topfun(), etc.
 *
 * Cell x = ...;        Safe if value assigned to x will never be an
 * <any code with a     indirection.  (Otherwise, cell assigned to x may
 * possible alloc>      be returned to freeList *before* the value is used.)
 * ... x ...            Probably best avoided in other circumstances.
 *
 * updateRoot(e);       All ok.
 * updapRoot(e,e);
 * updateRoot(mkInt(n));
 * eval(pop());
 *
 * eval(ap(c,pop()));   Bad -- a GC call may corrupt value pop'd off stack.
 *
 * It is also worth a reminder that the fst and snd values passed in any call
 * to the allocator are automatically marked and preserved if a GC is needed.
 * As a result, code like the following is guaranteed to be safe:
 *  return ap(ap(mkTuple(2),ZERONUM),ZERONUM);    (ZERONUM is a constant)
 *  for ( ... )                                   (PROVIDED that ds is the
 *     ds = cons(consChar(c),ds);                  only var that needs GC).
 *
 * If these restrictions are judged to be too onerous in particular cases,
 * temporarily enable conservative GC (and reset it to the original state,
 * either on or off at the beginning of the operation).  See bignums.c
 * for an example.
 *
 * There are also certain conventions that must always be obeyed, regardless
 * of whether conservative GC is in use.  For example:
 *
 * lhs = expr;          If lhs involves an address calculation that may be
 *                      invalidated by a gc, and expr could trigger an alloc,
 *                      then this expression is bad, or at least not portable:
 *                      it will only do the right thing under some evaluation
 *                      orders.  For example:  hd(top()) = ap(..,..) is bad,
 *                      unless you know that top() will never be modified
 *                      during a GC.
 *
 *                      This is no different from the problems that occur
 *                      with non-portable combinations of stack operators
 *                      like push(top());  The solution is also the same:
 *                      use an intermediate variable to make the order
 *                      of evaluation explicit.
 *
 * If this version of Hugs has been modified to allow different or
 * additional run-time representations for certain values, then the
 * examples and principles illustrated here may need to be reconsidered,
 * and possibly reclassified.  The same will also be true if the execution
 * mechanisms etc., are changed in any way.  (And all this is assuming
 * that the original implementations are correct...)
 * ------------------------------------------------------------------------*/

primFun(primFatbar) {                   /* Fatbar primitive                */
    Cell temp = evalWithNoError(primArg(2));
    if (nonNull(temp))
	if (temp==nameFail)             /* _FAIL [] r = r                  */
	    updateRoot(primArg(1));
	else {
	    updateRoot(temp);
	    cantReduce();
	}
    else
	updateRoot(primArg(2));         /* l     [] r = l  -- otherwise    */
}

primFun(primFail) {                     /* Failure primitive               */
    cantReduce();
}

primFun(primCatchError) {               /* Error catching  primitive       */
    Bool fOE = failOnError;
    Cell err = NIL;
    failOnError = FALSE;
    err = evalWithNoError(primArg(1));  /*  :: a -> Maybe a                */
    if (isNull(err)) {
	updapRoot(nameJust, primArg(1));
    } else {
	updateRoot(nameNothing);
    }
    failOnError = fOE;
}

/* ToDo: this just won't work! */
primFun(primThrowException) {           /* Failure primitive               */
    evalFails(root+1);                  /*  :: E×ception -> a              */
}

/* Cells have to be boxed (using the HUGSOBJECT tag) so that we can        */
/* evaluate a Cell expression such as "fst (cell1, cell2)" without         */
/* evaluating the thunk inside the Cell.                                   */

/* This function ought to be in the IO monad to preserve referential       */
/* transparency but it has tricky interactions with the concurrency parts  */
/* of the IO monad so we provide it in unsafe form here and make it safe   */
/* in the Prelude.                                                         */

primFun(primCatchException) {	       /* Error catching primitive         */
    Bool fOE = failOnError;            /*  :: a -> Either Cell a           */
    Cell err = NIL;
    failOnError = FALSE;
    err = evalWithNoError(primArg(1)); 
    if (isNull(err)) {
	updapRoot(nameRight, primArg(1));
    } else {
        if (isAp(err) && fun(err) == namePrimThrow) {
            err = arg(err);
        } else {
            err = ap(HUGSOBJECT, err);
        } 
	updapRoot(nameLeft, err);
    }
    failOnError = fOE;
}

primFun(primSel) {                      /* Component selection             */
    eval(primArg(2));                   /* _sel c e n  return nth component*/
    if (whnfHead==primArg(3))           /* in expr e, built with cfun c    */
	updateRoot(pushed(intOf(primArg(1))-1));
    else
	cantReduce();
}

primFun(primIf) {                       /* Conditional primitive           */
    eval(primArg(3));
    checkBool();
    if (whnfHead==nameTrue)
	updateRoot(primArg(2));
    else
	updateRoot(primArg(1));
}

primFun(primStrict) {                   /* Strict application primitive    */
    eval(primArg(1));                   /* evaluate 2nd argument           */
    updapRoot(primArg(2),primArg(1));   /* and apply 1st argument to result*/
}

primFun(primSeq) {                      /* Strict sequencing primitive     */
    eval(primArg(2));                   /* evaluate 1st argument           */
    updateRoot(primArg(1));             /* and return the first            */
}

primFun(primTrace) {                    /* an unsound trace primitive for  */
    fflush(stdout);                     /* debugging purposes              */
    eval(pop());                        /*  :: String -> a -> a            */
    while (whnfHead==nameCons) {
	eval(pop());
	putchar(charOf(whnfHead));
	eval(pop());
    }
    updateRoot(pop());
}

primFun(primConCmp) {                   /* compare constructors            */
    Int l,r;                            /*  :: a -> a -> Ordering          */
    ConArg(l,2);
    ConArg(r,1);
    updateRoot(l<r ? nameLT : (l>r ? nameGT : nameEQ));
}

primFun(primEnRange) {                  /* derived range for enum type     */
    eval(primArg(1));                   /* :: (a,a) -> [a]                 */
    updapRoot(ap(nameEnFrTo,primArg(3)),primArg(2));
}

primFun(primEnIndex) {                  /* derived index for enum type     */
    Int l,h,ix;                         /*  :: (a,a) -> a -> Int           */
    eval(primArg(2));
    ConArg(l,4);                        /* evaluate lower bound            */
    ConArg(h,3);                        /* evaluate upper bound            */
    ConArg(ix,1);                       /* evaluate index                  */
    if (l<=ix && ix<=h) {
	IntResult(ix-l);
    } else {
	cantReduce();
    }
}

primFun(primEnInRng) {                  /* derived inRange for enum type   */
    Int l,h,ix;                         /*  :: (a,a) -> a -> Bool          */
    eval(primArg(2));
    ConArg(l,4);                        /* evaluate lower bound            */
    ConArg(h,3);                        /* evaluate upper bound            */
    ConArg(ix,1);                       /* evaluate index                  */
    BoolResult(l<=ix && ix<=h);
}

primFun(primEnToEn) {                   /* derived toEnum for enum type    */
    Name n;                             /* :: a -> Int -> a                */
    Int  i;
    eval(primArg(2));
    checkCon();
    n = whnfHead;
    IntArg(i,1);
    if (nonNull(n = cfunByNum(n,i)))
	updateRoot(n);
    else
	cantReduce();
}

primFun(primEnFrEn) {                   /* derived fromEnum for enum type  */
    Int i;                              /* :: a -> Int                     */
    ConArg(i,1);
    IntResult(i==0 ? 0 : (i-1));
}

primFun(primEnFrom) {                   /* derived enumFrom for enum type  */
    Name cfs;                           /* :: a -> [a]                     */
    eval(primArg(1));
    checkCon();
    cfs = succCfun(whnfHead);
    push(isNull(cfs) ? nameNil : ap(nameEnFrom,cfs));
    updapRoot(ap(nameCons,whnfHead),top());
}

primFun(primEnFrTo) {                   /* derived enumFromTo for enum type*/
    Name l,r;                           /* :: a -> a -> [a]                */
    eval(primArg(2));
    checkCon();
    l = whnfHead;
    eval(primArg(1));
    checkCon();
    r = whnfHead;
    if (cfunOf(l) < cfunOf(r)) {
	push(ap(nameEnFrTo,succCfun(l)));
	updapRoot(ap(nameCons,l),ap(top(),whnfHead));
    }
    else if (l==r) {
	updapRoot(ap(nameCons,l),nameNil);
    } else {
	updateRoot(nameNil);
    }
}

primFun(primEnFrTh) {                   /* derived enumFromThen for enum ty*/
    Name f,n;                           /* :: a -> a -> [a]                */
    eval(primArg(2));   
    checkCon();
    f = whnfHead;
    eval(primArg(1));
    checkCon();
    n = nextCfun(f,whnfHead);
    if (isNull(n)) {
	push(ap(nameCons,whnfHead));
	toparg(nameNil);
    }
    else {
	push(ap(nameEnFrTh,whnfHead));
	toparg(n);
    }
    updapRoot(ap(nameCons,f),top());
}



/* --------------------------------------------------------------------------
 * Array primitives:
 * ------------------------------------------------------------------------*/

#if HASKELL_ARRAYS
#include "array.c"
#endif

/* --------------------------------------------------------------------------
 * Integer arithmetic primitives:
 * ------------------------------------------------------------------------*/

CAFInt(primMinInt,MINNEGINT)           /* minimum integer CAF              */
CAFInt(primMaxInt,MAXPOSINT)           /* maximum integer CAF              */
IntInt2Int(primPlusInt,x+y)            /* Integer addition primitive       */
IntInt2Int(primMinusInt,x-y)           /* Integer subtraction primitive    */
IntInt2Int(primMulInt,x*y)             /* Integer multiplication primitive */
Int2Int(primNegInt,-x)                 /* Integer negation primitive       */
Int2Bool(primEvenInt,!(x&1))           /* Integer even predicate           */
IntInt2IntPre(primQuotInt,x/y,y!=0)    /* Integer division primitive       */
				       /* truncated towards zero           */
IntInt2IntPre(primRemInt,x%y,y!=0)     /* Integer remainder primitive      */

/* quot and rem satisfy:                                                   */
/*     (x `quot` y)*y + (x `rem` y) == x                                   */
/* which is exactly the property described in K&R 2:                       */
/*     (a/b)*b + a%b == a                                                  */

primFun(primQrmInt) {                  /* Integer quotient and remainder   */
    Int x, y;                          /* truncated towards zero           */
    IntArg(x,2);
    IntArg(y,1);
    if (y==0)
	cantReduce();
    IntIntResult(x/y,x%y);
}

primFun(primDivInt) {                  /* Integer division primitive       */
    Int x,y,r;                         /* truncated towards -ve infinity   */
    IntArg(x,2);
    IntArg(y,1);
    if (y==0)
	cantReduce();
    r = x%y;
    x = x/y;
    if ((y<0 && r>0) || (y>0 && r<0))
	x--;
    IntResult(x);
}

primFun(primModInt) {                  /* Integer modulo primitive         */
    Int x,y,r;
    IntArg(x,2);
    IntArg(y,1);
    if (y==0)
	cantReduce();
    r = x%y;                           /* "... the modulo having the sign  */
    if ((r<0 && y>0) ||                /*              of the divisor ..." */
	(r>0 && y<0)) {                /* See definition on p.91 of Haskell*/
	IntResult(r+y);                /* report... (Haskell 1.1?)         */
    } else {
	IntResult(r);
    }
}

IntInt2Int(primAndInt,x&y)  
IntInt2Int(primOrInt, x|y)  
IntInt2Int(primXorInt,(x&~y) | (~x&y))   
Int2Int(primComplementInt,~x)
Int2Int(primBitInt, 1<<x)
IntInt2Bool(primTestInt,(x >> y) & 1)

primFun(primShiftInt) {
    Int x,y;
    IntArg(x,2);
    IntArg(y,1);
    if (y >= 0) {
	IntResult(x << y);
    } else {
	IntResult(x >> (-y));
    }
}

/* --------------------------------------------------------------------------
 * Unsigned arithmetic primitives:
 * ------------------------------------------------------------------------*/

#if WORD_OPS
CAFWord(primMaxWord,MAXHUGSWORD)       /* maximum integer CAF              */
WordWord2Word(primPlusWord,x+y)        /* Word addition primitive          */
WordWord2Word(primMinusWord,x-y)       /* Word subtraction primitive       */
Word2Word(primNegateWord,-x)           /* Word negation (modulo MAXWORD)   */
WordWord2Word(primMulWord,x*y)         /* Word multiplication primitive    */
Word2Bool(primEvenWord,!(x&1))         /* Word even predicate              */
WordWord2WordPre(primQuotWord,x/y,y!=0)/* Word division primitive          */
				       /* truncated towards zero           */
WordWord2WordPre(primDivWord,x/y,y!=0) /* Word division primitive          */
				       /* truncated towards zero           */
WordWord2WordPre(primRemWord,x%y,y!=0) /* Word remainder primitive         */
WordWord2WordPre(primModWord,x%y,y!=0) /* Word modulo primitive            */

/* quot and rem satisfy:                                                   */
/*     (x `quot` y)*y + (x `rem` y) == x                                   */
/* which is exactly the property described in K&R 2:                       */
/*     (a/b)*b + a%b == a                                                  */

primFun(primQrmWord) {                 /* Integer quotient and remainder   */
    Unsigned x, y;                     /* truncated towards zero           */
    WordArg(x,2);
    WordArg(y,1);
    if (y==0)
	cantReduce();
    WordWordResult(x/y,x%y);
}

WordWord2Word(primAndWord,x&y)  
WordWord2Word(primOrWord, x|y)  
WordWord2Word(primXorWord,(x&~y) | (~x&y))   
Word2Word(primComplementWord,~x)
Int2Word(primBitWord, 1<<x)
WordInt2Bool(primTestWord,(x >> y) & 1)

primFun(primShiftWord) {
    Unsigned x;         
    Int      y;
    WordArg(x,2);
    IntArg(y,1);
    if (y >= 0) {
	/* << isn't defined for y larger than word size */
	WordResult(y >= sizeof(x) * 8  ? 0 : x << y);
    } else {
	WordResult(y <= sizeof(x) * -8 ? 0 : x >> (-y));
    }
}

primFun(primRotateWord) {
    Unsigned x;         
    Int      y, z;
    WordArg(x,2);
    IntArg(y,1);
    IntArg(z,3);
    y = y % z;
    if (y >= 0) {
	WordResult((x << y) | (x >> (z - y)));
    } else {
	WordResult((x >> (-y)) | (x << (z + y)));
    }
}

Int2Int(primI32toI,   x&0xffffffff)
Int2Int(primI32toI8,  x&0xff)
Int2Int(primI32toI16, x&0xffff)

Int2Int(primItoI32,  x)
     Int2Int(primI8toI32, (Int8)x)   /* casts used to cause sign extension */
Int2Int(primI16toI32,(Int16)x)       /* casts used to cause sign extension */

Word2Word(primW32toW8 , x&0xff)
Word2Word(primW32toW16, x&0xffff)

Word2Word(primW8toW32 , x)
Word2Word(primW16toW32, x)

primFun(primI64toI32) {
    Int x, y;
    eval(primArg(1)); 
    x = fst(snd(whnfHead));
    y = snd(snd(whnfHead));
    IntIntResult(x,y);
}

primFun(primI32toI64) {
    Int x, y;
    IntArg(x,2);
    IntArg(y,1);
    updateRoot(pair(I64CELL,pair(x,y)));
}

primFun(primW64toW32) {
    Unsigned x, y;
    eval(primArg(1)); 
    x = fst(snd(whnfHead));
    y = snd(snd(whnfHead));
    WordWordResult(x,y);
}

primFun(primW32toW64) {
    Unsigned x, y;
    WordArg(x,2);
    WordArg(y,1);
    updateRoot(pair(I64CELL,pair(x,y)));
}

#endif /* WORD_OPS */

/* --------------------------------------------------------------------------
 * Haskell Integer (bignum) primitives:
 * ------------------------------------------------------------------------*/

#if BIGNUMS
#include "bignums.c"
#endif

/* --------------------------------------------------------------------------
 * Coercion primitives:
 * ------------------------------------------------------------------------*/

primFun(primCharToInt) {               /* Character to integer primitive   */
    Char c;
    CharArg(c,1);
    IntResult(c);
}

primFun(primIntToChar) {               /* Integer to character primitive   */
    Int i;
    IntArg(i,1);
    if (i<0  || i>MAXCHARVAL)
	cantReduce();
    CharResult(i);
}

primFun(primWord32ToInt) {             /* Word to integer primitive        */
    Unsigned x;
    WordArg(x,1);
    IntResult(x);
}

primFun(primIntToWord32) {             /* Integer to word primitive        */
    Int i;
    IntArg(i,1);
    WordResult(i);
}

primFun(primIntToFloat) {              /* Integer to Float primitive       */
    Int i;
    IntArg(i,1);
    FloatResult((Float)i);
}

primFun(primDummyCvt) {                /* dummy (identity) conversion      */
    updateRoot(primArg(1));
}

primFun(primRationalToFloat) {
#if SHORT_CIRCUIT_COERCIONS
    /* Optimisation: we try to short-circuit trivial conversions */
    Cell x = followInd(primArg(1));
    if (isAp(x)) {
	Cell f = followInd(fun(x));
	Cell a = arg(x);
	if (f == nameFloatToRational
	    || f == nameDoubleToRational
	    ) {
	    updateRoot(a);
	    return;
	} else if (isAp(f)) {
	    Cell g = followInd(fun(f));
	    if (g == nameDoubleToRatio) {
		updateRoot(a); /* ignore the dict - it must be right */
		return;
	    } else if (g == nameIntToRatio) {
		updapRoot(nameIntToFloat,a);
		return;
	    }
	}
    }
#endif
    updapRoot(nameRationalToFloat,primArg(1));
}

/* --------------------------------------------------------------------------
 * Float arithmetic primitives:
 * ------------------------------------------------------------------------*/

FloatFloat2Float(primPlusFloat,x+y)    /* Float addition primitive         */
FloatFloat2Float(primMinusFloat,x-y)   /* Float subtraction primitive      */
FloatFloat2Float(primMulFloat,x*y)     /* Float multiplication primitive   */
Float2Float(primNegFloat,-x)           /* Float negation primitive         */
FloatFloat2FloatPre(primDivFloat,x/y,y!=0)/* Float division primitive      */

#ifdef HAVE_LIBM
Float2Float(primSinFloat,sin(x))       /* Float sin (trig) primitive       */
Float2Float(primCosFloat,cos(x))       /* Float cos (trig) primitive       */
Float2Float(primTanFloat,tan(x))       /* Float tan (trig) primitive       */
Float2Float(primAsinFloat,asin(x))     /* Float arc sin (trig) primitive   */
Float2Float(primAcosFloat,acos(x))     /* Float arc cos (trig) primitive   */
Float2Float(primAtanFloat,atan(x))     /* Float arc tan (trig) primitive   */
#if 0 /* not used in current version of Prelude */
FloatFloat2Float(primAtan2Float,atan2(x,y)) /* Float arc tan with quadrant info*/
#endif
				       /*               (trig) primitive   */
Float2Float(primExpFloat,exp(x))       /* Float exponential primitive      */
Float2FloatPre(primLogFloat,log(x),x>0)/* Float logarithm primitive        */
Float2FloatPre(primSqrtFloat,sqrt(x),x>=0) /* Float square root primitive  */

#if 0 /* This was in Hugs 1.01 - not needed by prelude */
Float2FloatPre(primLog10Float,log10(x),x>0) /* Float logarithm (base 10) prim*/
#endif
/* Not used in Hugs prelude, rounds towards zero */
Float2Int(primFloatToInt,(Int) x)      /* Adhoc Float --> Int conversion   */

#if BIGNUMS
CAFBignum(primFloatRadix,bigInt(HUGS_RADIX)) /* Float radix primitive */
#else                                    
CAFInt(primFloatRadix,HUGS_RADIX)     /* from K&R2, I hope it's portable  */
#endif

CAFInt(primFloatDigits,HUGS_MANT_DIG)  /* Float sig. digits primitive      */
				       /* again, courtesy K&R2             */

CAFInt(primFloatMinExp,HUGS_MIN_EXP)   /* Float min exponent primitive     */
CAFInt(primFloatMaxExp,HUGS_MAX_EXP)   /* Float max exponent primitive     */

/* ToDo: GHC stole its decode code from Lennart - maybe we should too?     */
primFun(primFloatDecode) {             /* Float decode primitive           */
    double f;                          /*  :: Float -> (Integer,Int)       */
    Int    n;                          /* another gruesome hack            */
    FloatArg(f,1);
    f  = frexp((double)(f),&n);        /* 0.5   <= f < 1                   */
    f  = ldexp(f,HUGS_MANT_DIG);       /* 2^m-1 <= f < 2^m, m=HUGS_MANT_DIG*/
    n -= HUGS_MANT_DIG;
#if BIGNUMS
    push(bigDouble(f));
    updapRoot(ap(mkTuple(2),top()),mkInt(n));
#else
    push(mkInt((Int)f));
    updapRoot(ap(mkTuple(2),top()),mkInt(n));
#endif
}

primFun(primFloatEncode) {             /* Float encode primitive           */
    Int n;                             /*  :: Integer -> Int -> Float      */
    Float f;                           /* Ugly hack, don't use Hugs for    */
    IntArg(n,1);                       /* numerical work                   */
    eval(primArg(2));                  /* get integer                      */
#if DJGPP2                                     
    _fpreset();                        /* Get round a possible DJGPP bug?  */
#endif                                         
#if BIGNUMS                                    
    f = (Float)floatOf(bigToFloat(whnfHead)); /* and turn it into a float  */
#else                                          
    f = (Float) whnfInt;               /* and turn it into a float         */
#endif
    updateRoot(mkFloat(ldexp(f,n)));
}

#endif /* HAVE_LIBM */

/* --------------------------------------------------------------------------
 * Addr primitives:
 * ------------------------------------------------------------------------*/

CAFPtr(primNullAddr,0)                 /* Null pointer                     */
PtrInt2Ptr(primPlusAddr,(char*)x+y)    /* Pointer arithmetic               */
PtrPtr2Bool(primEqAddr,x==y)           /* Addr equality primitive          */
Ptr2Int(primAddrToInt,((Int)x))        /* geting the pointer               */

/* --------------------------------------------------------------------------
 * Comparison primitives:
 * ------------------------------------------------------------------------*/

IntInt2Bool(primEqInt32,x==y)          /* Int32 equality primitive         */
IntInt2Bool(primEqInt,x==y)            /* Integer equality primitive       */
WordWord2Bool(primEqWord,x==y)         /* Natural equality primitive       */
CharChar2Bool(primEqChar,x==y)         /* Character equality primitive     */
FloatFloat2Bool(primEqFloat, x==y)     /* Float equality primitive         */

primFun(primCmpInt32) {                /* Integer compare primitive        */
    Int x, y;
    IntArg(x,2);
    IntArg(y,1);
    updateRoot( x<y ? nameLT :
	      ( x>y ? nameGT : 
		      nameEQ ));
}

primFun(primCmpInt) {                  /* Integer compare primitive        */
    Int x, y;
    IntArg(x,2);
    IntArg(y,1);
    updateRoot( x<y ? nameLT :
	      ( x>y ? nameGT : 
		      nameEQ ));
}

primFun(primCmpWord) {                 /* Natural compare primitive        */
    Unsigned x, y;
    WordArg(x,2);
    WordArg(y,1);
    updateRoot( x<y ? nameLT :
	      ( x>y ? nameGT : 
		      nameEQ ));
}

primFun(primCmpChar) {                 /* Character compare primitive      */
    Char x, y;
    CharArg(x,2);
    CharArg(y,1);
    updateRoot( x<y ? nameLT :
	      ( x>y ? nameGT : 
		      nameEQ ));
}

primFun(primCmpFloat) {                /* Float compare primitive          */
    Float x, y;
    FloatArg(x,2);
    FloatArg(y,1);
    updateRoot( x<y ? nameLT :
	      ( x>y ? nameGT : 
		      nameEQ ));
}

/* --------------------------------------------------------------------------
 * Print primitives:
 * ------------------------------------------------------------------------*/

#include "printer.c"

/* --------------------------------------------------------------------------
 * Evaluate name, obtaining a C string from a Hugs string:
 * ------------------------------------------------------------------------*/

static String local evalName(es)        /* evaluate es :: [Char] and save  */
Cell es; {                              /* in char array... return ptr to  */
    static char buffer[FILENAME_MAX+1]; /* string or 0, if error occurs    */
    Int         pos    = 0;
    StackPtr    saveSp = sp;

    eval(es);
    while (whnfHead==nameCons && pos<FILENAME_MAX) {
	eval(pop());
	buffer[pos++] = (char)charOf(whnfHead);
	eval(pop());
    }
    if (whnfHead==nameNil) {
	buffer[pos] = '\0';
	return buffer;
    }
    sp = saveSp;                        /* stack pointer must be the same  */
    return 0;                           /* as it was on entry              */
}

/* --------------------------------------------------------------------------
 * Top-level printing mechanism:
 * ------------------------------------------------------------------------*/

Void outputString(fp)                   /* Evaluate string on top of stack */
FILE *fp; {                             /* and print it on fp              */
    StackPtr origSp = sp;
    for (;;) {
	Cell temp = evalWithNoError(pop());
	if (nonNull(temp)) {
	    sp = origSp;
	    top()  = printBadRedex((top()=temp),nameNil);
	}
	else if (whnfHead==nameCons) {
	    if (nonNull(temp=evalWithNoError(pop()))) {
		sp = origSp;
		onto(temp);
		pushed(1) = printBadRedex(pushed(0),pushed(1));
		drop();
	    }
	    else {
		putc(charOf(whnfHead),fp);
		fflush(fp);
	    }
	}
	else
	    return;
    }
}

/* --------------------------------------------------------------------------
 * IO monad implementation
 * ------------------------------------------------------------------------*/

#if IO_MONAD
#include "iomonad.c"
#endif

/* --------------------------------------------------------------------------
 * Time and CPUTime module implementations
 * ------------------------------------------------------------------------*/

#if TIME_MODULE
#include "timeprim.c"
#endif

/* --------------------------------------------------------------------------
 * Directory module implementation
 * ------------------------------------------------------------------------*/

#if DIRECTORY_MODULE
#include "dirprim.c"
#endif

/* --------------------------------------------------------------------------
 * Error catching primitives
 * (not standard Haskell but jolly useful)
 * ------------------------------------------------------------------------*/

#if INTERNAL_PRIMS
#include "interns.c"
#endif

/* --------------------------------------------------------------------------
 * ST monad implementation
 * ------------------------------------------------------------------------*/

#if LAZY_ST
#include "stmonad.c"
#endif

/* --------------------------------------------------------------------------
 * Observations & breakpoints
 * ------------------------------------------------------------------------*/

#if OBSERVATIONS
#define MAXTAGLENGTH 80
static char obsTag[MAXTAGLENGTH+1];

primFun(primObserve) {			/* the observe primitive for       */
    Cell exp, obsCell;			/* debugging purposes              */
    int i=0;				/*  :: String -> a -> a            */
    fflush(stdout);
    eval(pop());
    while (whnfHead==nameCons) {
	eval(pop());
	if (i<MAXTAGLENGTH) obsTag[i++]=charOf(whnfHead);
	eval(pop());
    }
    obsTag[i]=0;
    				/* create OBSERVE graph marker		   */
    exp  = pop();
    exp  = triple(OBSERVE,exp,0);
    updateRoot(exp);
    /* root is now an INDIRECT node which points to an OBSERVE node 	   */
    /* next create observation list cell for the expression		   */
    obsCell = addObsInstance(obsTag,stack(root),-1);
    /* finally update the OBSERVE node to point to the ons. list cell  	   */
    markedObs(snd(stack(root))) = obsCell;
}

primFun(primBkpt) {			/* check if break enabled          */
    Int i=0;                /* initiate dialogue               */
    fflush(stdout);
    eval(pop());
    while (whnfHead==nameCons) {
	eval(pop());
	if (i<MAXTAGLENGTH) obsTag[i++]=charOf(whnfHead);
	eval(pop());
    }
    obsTag[i]=0;

    if (breakNow(obsTag)) breakDialogue(obsTag);
    updateRoot(pop());
}

#if !LAZY_ST
#error primitive "setBkpt" unavailable as LAZY_ST not enabled 
#else
primFun(primSetBkpt) {			
    String s = evalName(IOArg(2));
    eval(IOArg(1));
    checkBool();
    setBreakpt(s, whnfHead == nameTrue);
    IOReturn(nameUnit);
}
#endif

#endif

/* --------------------------------------------------------------------------
 * Extensible records: (Gaster and Jones, 1996)
 * ------------------------------------------------------------------------*/

#if TREX
primFun(primRecExt) {                   /* :: Int -> a -> Rec ? -> Rec ?   */
    Int  n;
    Cell b = NIL;
    Cell r;
    eval(primArg(3));
    n = whnfInt;
    eval(primArg(1));
    for (r=arg(whnfHead); n>0; n--) {
	b = cons(fun(r),b);
	r = arg(r);
    }
    b = cons(primArg(2),b);
    updapRoot(RECORD,revOnto(b,r));
}

primFun(primRecBrk) {                   /* :: Int -> Rec ? -> (?, Rec ?)   */
    Int  n;
    Cell b = cons(RECORD,NIL);
    Cell r;
    eval(primArg(2));
    n = whnfInt;
    eval(primArg(1));
    for (r=arg(whnfHead); n>0; n--) {
	b = cons(fun(r),b);
	r = arg(r);
    }
    pushed(1) = revOnto(b,arg(r));
    pushed(0) = ap(mkTuple(2),fun(r));
    updapRoot(pushed(0),pushed(1));
}

primFun(primRecSel) {                   /* :: Int -> Rec ? -> ?            */
    Int  n;
    Cell r;
    eval(primArg(2));
    n = whnfInt;
    eval(primArg(1));
    for (r=arg(whnfHead); n>0; n--)
	r = arg(r);
    updateRoot(fun(r));
}

/* recShw :: primArg(5) Label l       ->
 *           primArg(4) ShowD a       ->
 *           primArg(3) Lacks_l r     ->
 *           primArg(2) ShowRecRowD r ->
 *           primArg(1) Rec (l::a|r)  -> [(String,ShowS)]
 * recShw l d e f r
 *    = case recBrk e r of
 *        (v,s) -> insertField l (showsPrec d 0 v) (showRecRow f s)
 */

primFun(primRecShw) {
    push(nameRecBrk);
    toparg(primArg(3));
    toparg(primArg(1));
    eval(pop());
    primArg(2) = ap(nameShowRecRow,primArg(2));
    primArg(4) = ap(nameShowsPrec,primArg(4));
    primArg(4) = ap(primArg(4),mkInt(0));
    primArg(5) = ap(nameInsFld,primArg(5));
    pushed(1)  = ap(primArg(2),pushed(1));
    pushed(0)  = ap(primArg(4),pushed(0));
    pushed(0)  = ap(primArg(5),pushed(0));
    updapRoot(pushed(0),pushed(1));
}

/* recEq :: primArg(6) Label l       ->
 *	    primArg(5) EqD a         ->
 *	    primArg(4) Lacks_x r     ->
 *	    primArg(3) EqRecRowD r   ->
 *	    primArg(2) Rec (l::a|r)  ->
 *	    primArg(1) Rec (l::a|r)  -> [(String,Bool)]
 * reqEq l eqa e eqr r1 r2
 *    = case recBrk e r1 of
 *	  (v,s1) -> case recBrk e r2 of
 *		      (w,s2) -> insertField l ((==) eqa v w)
 *						      (eqRecRow eqr s1 s2)
 */

primFun(primRecEq) {
    push(nameRecBrk);
    toparg(primArg(4));
    toparg(primArg(2));
    eval(pop());
    push(nameRecBrk);
    toparg(primArg(4));
    toparg(primArg(1));
    eval(pop());
    primArg(3) = ap(nameEqRecRow,primArg(3));
    primArg(3) = ap(primArg(3),pushed(3));
    primArg(3) = ap(primArg(3),pushed(1));
    primArg(5) = ap(nameEq,primArg(5));
    primArg(5) = ap(primArg(5),pushed(2));
    primArg(5) = ap(primArg(5),pushed(0));
    primArg(6) = ap(nameInsFld,primArg(6));
    primArg(6) = ap(primArg(6),primArg(5));
    updapRoot(primArg(6),primArg(3));
}
#endif

/* --------------------------------------------------------------------------
 * Auxilliary functions
 * ------------------------------------------------------------------------*/

static Cell local followInd(c)    /* follow chain of indirections and CAFs */
Cell c; {
    do {
	switch (whatIs(c)) {
	case INDIRECT : c = snd(c);
		break;
#if OBSERVATIONS
        case OBSERVE  : c = markedExpr(c);
                break;
#endif
	case NAME     : if (isCfun(c)
		|| name(c).arity != 0 
		|| isNull(name(c).defn)) {
		return c;
		}
		c = name(c).defn;
		break;
	default       : return c;
    }
    allowBreak();
    } while (1);
}
		   
/* --------------------------------------------------------------------------
 * Pointer equality
 * ------------------------------------------------------------------------*/

/* Pointer equality tests break referential transparency.
 * However, they can be useful in implementing referentially transparent
 * functions such as lazy memo-tables.
 *
 *   foo = cache sin
 *
 *   cache :: (a -> b) -> (a -> b)
 *   cache f = \x -> unsafePerformIO (check x)
 *    where
 *     ref = unsafePerformIO (newRef (error "cache", error "cache"))
 *     check x = derefRef ref >>= \ (x',a) ->
 *               if x `primPtrEq` x' then
 *                 return a
 *               else
 *                 let a = f x in
 *                 assignRef ref (x, a) >>
 *                 return a
 */

primFun(primPtrEq) {		       /* Unsafe pointer equality test     */
    Cell x = followInd(primArg(2));
    Cell y = followInd(primArg(1));
    updateRoot( (x==y) ? nameTrue : nameFalse );
}

/* Companion function for use when debugging uses of primPtrEq.
 * Converts a heap pointer to an Int so you can look at it.
 * I don't think there's any way of using this function that 
 * doesn't break the semantics - debugging use only.
 */
primFun(primPtrToInt) {
    updateRoot(mkInt(followInd(primArg(1))));
}

/*---------------------------------------------------------------------------
 * GreenCard entry points
 *
 * GreenCard generated code accesses Hugs data structures and functions 
 * (only) via these functions (which are stored in the virtual function
 * table hugsAPI4).
 *-------------------------------------------------------------------------*/

static void           getUnit        Args((void));
static HsInt          getInt         Args((void));
static HsWord         getWord        Args((void));
static HsAddr         getAddr        Args((void));
static float          getFloat       Args((void));
static double         getDouble      Args((void));
static HsChar         getChar        Args((void));
static HugsForeign    getForeign     Args((void));
static HsBool         getBool        Args((void));
static HsInt8         getInt8        Args((void));
static HsInt16        getInt16       Args((void));
static HsInt32        getInt32       Args((void));
static HsInt64        getInt64       Args((void));
static HsWord8        getWord8       Args((void));
static HsWord16       getWord16      Args((void));
static HsWord32       getWord32      Args((void));
static HsWord64       getWord64      Args((void));
static HsPtr          getPtr         Args((void));
static HsFunPtr       getFunPtr      Args((void));
static HsForeignPtr   getForeignPtr  Args((void));	      
static HugsStablePtr  getStablePtr   Args((void));
static HsStablePtr    getStablePtr4  Args((void));
static HugsStablePtr  makeStablePtr4 Args((void));
static HsFloat        getFloat4      Args((void));
static HsDouble       getDouble4     Args((void));

static void           putInt         Args((HsInt));
static void           putWord        Args((HsWord));
static void           putAddr        Args((HsAddr));
static void           putFloat       Args((double));
static void           putDouble      Args((double));
static void           putChar        Args((HsChar));
static void           putForeign     Args((HugsForeign, void (*)(void *)));
static void           putStablePtr   Args((HugsStablePtr));
static void           putStablePtr4  Args((HsStablePtr));
static void           putBool        Args((HsBool));
static void           putInt8        Args((HsInt8));
static void           putInt16       Args((HsInt16));
static void           putInt32       Args((HsInt32));
static void           putInt64       Args((HsInt64));
static void           putWord8       Args((HsWord8));
static void           putWord16      Args((HsWord16));
static void           putWord32      Args((HsWord32));
static void           putWord64      Args((HsWord64));
static void           putPtr         Args((HsPtr));
static void           putFunPtr      Args((HsFunPtr));
static void           putForeignPtr  Args((HsForeignPtr));
static void           putFloat4      Args((HsFloat));
static void           putDouble4     Args((HsDouble));

static void           freeStablePtr4 Args((HsStablePtr));

	      
static void           returnIO       Args((HugsStackPtr, int));
static void           returnId       Args((HugsStackPtr, int));
static int            runIO          Args((int));
static void           apMany         Args((int));

static void           getUnit()      { eval(pop()); }
static HsInt          getInt()       { eval(pop()); checkInt();   return whnfInt; }
static HsWord         getWord()      { eval(pop()); checkWord();  return (unsigned int) whnfInt; }
static HsAddr         getAddr()      { eval(pop()); checkPtr();   return ptrOf(whnfHead); }
static float          getFloat()     { eval(pop()); checkFloat(); return whnfFloat; }
static double         getDouble()    { eval(pop()); checkFloat(); return (double) whnfFloat; }
static HsChar         getChar()      { eval(pop()); checkChar();  return charOf(whnfHead); }
static HugsForeign    getForeign()   { eval(pop()); return derefMP(whnfHead); }
static HsBool         getBool()      { eval(pop()); checkBool();  return (whnfHead == nameTrue); }
static HsInt8         getInt8()      { eval(pop()); checkInt();   return whnfInt; } 
static HsInt16        getInt16()     { eval(pop()); checkInt();   return whnfInt; }
static HsInt32        getInt32()     { eval(pop()); checkInt();   return whnfInt; }
static HsInt64        getInt64()     { eval(pop()); return int64FromParts(intOf(fst(snd(whnfHead))), intOf(snd(snd(whnfHead)))); }
static HsWord8        getWord8()     { eval(pop()); checkWord();  return (unsigned int) whnfInt; } 
static HsWord16       getWord16()    { eval(pop()); checkWord();  return (unsigned int) whnfInt; } 
static HsWord32       getWord32()    { eval(pop()); checkWord();  return (unsigned int) whnfInt; } 
static HsWord64       getWord64()    { eval(pop()); return int64FromParts(intOf(fst(snd(whnfHead))), intOf(snd(snd(whnfHead)))); }
static HsPtr          getPtr()       { eval(pop()); checkPtr();   return ptrOf(whnfHead); }
static HsFunPtr       getFunPtr()    { eval(pop()); checkPtr();   return (HsFunPtr)ptrOf(whnfHead); }

static HsForeignPtr   getForeignPtr() {
    ERRMSG(0) "getForeignPtr: not implemented in Hugs"
    EEND;
    return 0;
}

static HugsStablePtr  getStablePtr() { 
    Cell c = mkStablePtr(pop());
    if (isNull(c)) {
	ERRMSG(0) "Stable pointer table full"
	EEND;
    }
    return c;
}

static HugsStablePtr  lookupName Args((String, String));
static HugsStablePtr  lookupName(q,n)
String q;
String n; { 
    Name nm = findQualFun(findText(q), findText(n));
    Cell c;
    
    if (isNull(nm)) {
	ERRMSG(0) "Can't find qualified name '%s.%s'", q, n
	EEND;
    }
    c = mkStablePtr(nm);
    if (isNull(c)) {
	ERRMSG(0) "Stable pointer table full"
	EEND;
    }
    return c;
}

static void putInt (HsInt  x) { push(mkInt(x)); }
static void putWord(HsWord x) { push(mkInt((int)x)); }
static void putAddr(HsAddr x) { push(mkPtr(x)); }
static void putChar(HsChar x) { push(mkChar(x)); }
static void putFloat (double x) { push(mkFloat(x)); }
static void putDouble(double x) { push(mkFloat(x)); }
static void putForeign(HugsForeign x, void (*f)(HugsForeign)) { push(mkMallocPtr(x,f)); }
static void putStablePtr   (HugsStablePtr x) { push(derefStablePtr(x)); }
static void putBool        (HsBool x)        { push(x?nameTrue:nameFalse); }
                                            
static void putInt8 (HsInt8  x) { push(mkInt(x)); }
static void putInt16(HsInt16 x) { push(mkInt(x)); }
static void putInt32(HsInt32 x) { push(mkInt(x)); }
static void putInt64(HsInt64 x) { push(pair(I64CELL,pair(part1Int64(x),part2Int64(x)))); }
static void putWord8 (HsWord8  x) { push(mkInt((int)x)); }
static void putWord16(HsWord16 x) { push(mkInt((int)x)); }
static void putWord32(HsWord32 x) { push(mkInt((int)x)); }
static void putWord64(HsWord64 x) { push(pair(I64CELL,pair(part1Int64(x),part2Int64(x)))); }
static void putPtr   (HsPtr    x) { push(mkPtr(x)); }
static void putFunPtr(HsFunPtr x) { push(mkPtr((Pointer)x)); }

static void putStablePtr4(HsStablePtr   x) {
    push((HugsStablePtr)x);
}

static HsStablePtr getStablePtr4(void) { 
    HugsStablePtr x = pop();
    return (HsStablePtr)x;
}

static Void freeStablePtr4(HsStablePtr x) {
    if (x) freeStablePtr((HugsStablePtr)x);
}

static HsFloat        getFloat4()    { eval(pop()); checkFloat(); return whnfFloat; }
static HsDouble       getDouble4()   { eval(pop()); checkFloat(); return whnfFloat; }

static void putFloat4(HsFloat x) {
  push(mkFloat(x));
}

static void putDouble4(HsDouble x) {
  push(mkFloat(x));
}

static void putForeignPtr(HsForeignPtr x) {
    ERRMSG(0) "putForeignPtr: not implemented in Hugs"
    EEND;
}

static void returnIO(root,n) /* return in IO monad */
HugsStackPtr root;
int          n; {
    /* There should be n return values on the top of the stack */
    if (n == 0) {
	push(nameUnit);
    } else if (n == 1) {
       /* do nothing */
    } else {
    int i;
    push(mkTuple(n));
    for(i=0; i<n; ++i) {
	pushed(1) = ap(pushed(0),pushed(1));
	drop();
    }
    }
    updapRoot(primArg(1),top());
}

static void returnId(root,n) /* return in identity monad */
HugsStackPtr root;
int          n; {
    /* There should be n return values on the top of the stack */
    if (n == 0) {
	push(nameUnit);
    } else if (n == 1) {
        /* do nothing, already there. */
    } else {
	int i;
	push(mkTuple(n));
	for(i=0; i<n; ++i) {
	    pushed(1) = ap(pushed(0),pushed(1));
	    drop();
	}
    }
    /*
     * Note: have to be a bit careful when returning, since we could
     * be returning from evaluating a CAF. In the non-CAF case,
     * 'root' points to the object we've entered, so we can just
     * go ahead and update it. No problem there.
     *
     * For CAFs, the evaluator leaves 'root' pointing at the Name
     * of the CAF, so updating it directly is not going to work.
     * Instead, we just leave the result at the top of the stack & 
     * let the evaluator do what it has always done; clean up & 
     * update the 'defn' field inside the CAF Name.
     * 
     *  [6/01 --sof]
     */
    if ( isPair(stack(root))
#if 0 || DEBUG   
	 || (whatIs(stack(root)) == AP)
#endif
	 ) {
      updateRoot(top());
    }
}

static int runIO(n)
int n; {
    /* stack = argn : ... : arg1 : fun : rest */
    StackPtr old_sp   = sp - n - 1;
    Cell     temp     = NIL;
    Int i;
    /* build application node */
    for(i=n-1; i >= 0; --i) {
	pushed(n) = ap(pushed(n), pushed(i));
    }
    sp -= n;

    /* evaluate it - should have type IO a */
    temp = evalWithNoError(ap(nameIORun,pop()));
    if (nonNull(temp)) {
	ERRMSG(0) "runIO: uncaught error"
	EEND;
    }
    if (sp != old_sp+1) {
	ERRMSG(0) "runIO: unbalanced stack (%d)", sp-old_sp
	EEND;
    }
    if (whnfHead == nameRight) {
	return 0;
    } else if (whnfHead != nameLeft) { /* Called "exit" */
	ERRMSG(0) "runIO: bad return value"
	EEND;
    }
    return 1;
}    

static void apMany(n)
int n; {
    /* stack = argn : ... : arg1 : fun : rest */
    Int i;
    /* build application node */
    for(i=n-1; i >= 0; --i) {
	pushed(n) = ap(pushed(n), pushed(i));
    }
    sp -= n;
    /* stack = ap(...(ap(fun,arg1),...),argn) : rest */
}

static int runId Args((int));

static int runId(n)
int n; {
    apMany(n);
    top() = ap(nameReturnIO,top());
    return runIO(0);
}

/* This allocates a small object and writes some machine code into it.  
 *
 * The code generated by the generated code is equivalent to:
 *  
 *   rty f(ty1 a1, ... tym am) {
 *     return (*app)(s,a1, ... am);
 *   }
 *
 * Where s is a stable pointer (an int).
 *
 * But, because this is machine code, we can do it without knowing
 * anything about the argument types.  This works because the C
 * calling convention on most machines has the stack looking something
 * like this (where leftmost = top of stack)
 *
 *   ret_addr : a1 : ... am : rest of stack
 *
 * If this is not the case for some architecture/calling convention,
 * the easiest thing to do might be to make the stable pointer be
 * the last argument (if that is easily found) or to put it in a callee
 * saves register - and then adapt the apply function generated by 
 * implementForeignExport.  
 *
 * thunk->{next,prev}: a doubly linked list (makes deletion easy)
 * thunk->stable:      the stable pointer (makes deletion easy)
 * 
 * At the end of execution, we run down the list freeing every thunk
 * that the user did not explicitly deallocate.
 */

struct thunk_data {
    struct thunk_data* next;
    struct thunk_data* prev;
    HugsStablePtr      stable;
#if defined(__i386__) || defined(_X86_)
    char               code[16];
#elif defined(__ppc__)
     char               code[13*4];
#elif defined(__sparc__) && defined(__GNUC__)
    char               code[44];
#else
    /* This is a placeholder intended to avoid compile-time warnings.
     * A runtime warning will be generated by mkThunk if an attempt is
     * make to use foreign wrappers
     */
    char               code[1];
#endif
};

struct thunk_data* foreignThunks = 0;

static void* mkThunk(void (*app)(void), HugsStablePtr s) {
    struct thunk_data* thunk 
        = (struct thunk_data*)malloc(sizeof(struct thunk_data));
    char* pc;
    if (!thunk) {
        /* ToDo: better cleanup */
        printf("Can't allocate thunk for foreign import wrapper\n");
        exit(1);
    }
    if (foreignThunks) { /* non-empty list */
        foreignThunks->prev = thunk;
    }        
    thunk->next = foreignThunks;
    thunk->prev = 0;
    foreignThunks = thunk;
    thunk->stable = s;
    pc = &thunk->code[0];
#if defined(__i386__) || defined(_X86_)
    /* 3 bytes: pushl (%esp) */
    *pc++ = (char)0xff; *pc++ = 0x34; *pc++ = 0x24;  

    /* 8 bytes: movl s,4(%esp) */
    *pc++ = (char)0xc7; *pc++ = 0x44; *pc++ = 0x24; *pc++ = 0x04; 
    *((HugsStablePtr*)pc)++ = s;

    /* 5 bytes: jmp app */
    *pc++ = (char)0xe9;
    *((int*)pc)++ = (char*)app - ((char*)&(thunk->code[16]));
#elif defined(__ppc__) && defined(__GNUC__)
     /* This is only for MacOS X.
      * It does not work on MacOS 9 because of the very strange
      * handling of function pointers in OS 9.
      * I don't know about LinuxPPC calling conventions.
      * Please note that it only works for up to 7 arguments.
      */

     {
         unsigned long *adj_code = (unsigned long*)pc;
         /* make room for extra arguments */
         adj_code[0] = 0x7d2a4b78;    /* mr r10,r9 */
         adj_code[1] = 0x7d094378;    /* mr r9,r8  */
         adj_code[2] = 0x7ce83b78;    /* mr r8,r7  */
         adj_code[3] = 0x7cc73378;    /* mr r7,r6  */
         adj_code[4] = 0x7ca62b78;    /* mr r6,r5  */
         adj_code[5] = 0x7c852378;    /* mr r5,r4  */
         adj_code[6] = 0x7c641b78;    /* mr r4,r3  */

         adj_code[7] = 0x3c000000; /* lis r0,hi(app) */
         adj_code[7] |= ((unsigned long)app) >> 16;

         adj_code[8] = 0x3c600000; /* lis r3,hi(s) */
         adj_code[8] |= ((unsigned long)s) >> 16;

         adj_code[9] = 0x60000000; /* ori r0,r0,lo(app) */
         adj_code[9] |= ((unsigned long)app) & 0xFFFF;

         adj_code[10] = 0x60630000; /* ori r3,r3,lo(s) */
         adj_code[10] |= ((unsigned long)s) & 0xFFFF;

         adj_code[11] = 0x7c0903a6; /* mtctr r0 */
         adj_code[12] = 0x4e800420; /* bctr */

         pc = (char*) &adj_code[13];

         /* Flush the Instruction cache: */
         /* MakeDataExecutable(adjustor,4*13); */
             /* This would require us to link with CoreServices.framework */
         { /* this should do the same: */
             int n = 13;
             unsigned long *p = adj_code;
             while(n--)
             {
                 __asm__ volatile ("dcbf 0,%0\n\tsync\n\ticbi 0,%0"
                             : : "g" (p));
                 p++;
             }
             __asm__ volatile ("sync\n\tisync");
         }
     }
#elif defined(__sparc__) && defined(__GNUC__)
     /* Mostly cut-n-pasted from GHC's Adjustor.c:

	<00>: 9C23A008   sub   %sp, 8, %sp     ! make room for %o4/%o5 in caller's frame
	<04>: DA23A060   st    %o5, [%sp + 96] ! shift registers by 2 positions
	<08>: D823A05C   st    %o4, [%sp + 92]
	<0C>: 9A10000B   mov   %o3, %o5
	<10>: 9810000A   mov   %o2, %o4
	<14>: 96100009   mov   %o1, %o3
	<18>: 94100008   mov   %o0, %o2
	<1C>: 13000000   sethi %hi(app), %o1   ! load up app (1 of 2)
	<20>: 11000000   sethi %hi(s), %o0     ! load up s (1 of 2)
	<24>: 81C26000   jmp   %o1 + %lo(app)  ! jump to app (load 2 of 2)
	<28>: 90122000   or    %o0, %lo(), %o0 ! load up s (2 of 2, delay slot)

	ccall'ing on SPARC is easy, because we are quite lucky to push a
	multiple of 8 bytes (1 word stable pointer + 1 word dummy arg) in front
	of the existing arguments (note that %sp must stay double-word aligned at
	all times, see ABI spec at http://www.sparc.org/standards/psABI3rd.pdf).
	To do this, we extend the *caller's* stack frame by 2 words and shift
	the output registers used for argument passing (%o0 - %o5, we are a
	*leaf* procedure because of the tail-jump) by 2 positions. This makes
	room in %o0 and %o1 for the additinal arguments, namely  the stable
	pointer and a dummy (used for destination addr of jump on SPARC). This
	shouldn't cause any problems for a C-like caller: alloca is implemented
	similarly, and local variables should be accessed via %fp, not %sp. In
	a nutshell: This should work! (Famous last words! :-)
     */
     {
	 unsigned long *adj_code = (unsigned long *)pc;
	 adj_code[ 0]  = 0x9C23A008UL;   /* sub   %sp, 8, %sp         */
	 adj_code[ 1]  = 0xDA23A060UL;   /* st    %o5, [%sp + 96]     */
	 adj_code[ 2]  = 0xD823A05CUL;   /* st    %o4, [%sp + 92]     */
	 adj_code[ 3]  = 0x9A10000BUL;   /* mov   %o3, %o5            */
	 adj_code[ 4]  = 0x9810000AUL;   /* mov   %o2, %o4            */
	 adj_code[ 5]  = 0x96100009UL;   /* mov   %o1, %o3            */
	 adj_code[ 6]  = 0x94100008UL;   /* mov   %o0, %o2            */
	 adj_code[ 7]  = 0x13000000UL;   /* sethi %hi(app), %o1       */
	 adj_code[ 7] |= ((unsigned long)app) >> 10;
	 adj_code[ 8]  = 0x11000000UL;   /* sethi %hi(s), %o0         */
	 adj_code[ 8] |= ((unsigned long)s) >> 10;
	 adj_code[ 9]  = 0x81C26000UL;   /* jmp   %o1 + %lo(app)      */
	 adj_code[ 9] |= ((unsigned long)app) & 0x000003FFUL;
	 adj_code[10]  = 0x90122000UL;   /* or    %o0, %lo(s), %o0    */
	 adj_code[10] |= ((unsigned long)s) & 0x000003FFUL;
	 
	 /* flush cache */
	 asm("flush %0" : : "r" (adj_code     ));
	 asm("flush %0" : : "r" (adj_code +  2));
	 asm("flush %0" : : "r" (adj_code +  4));
	 asm("flush %0" : : "r" (adj_code +  6));
	 asm("flush %0" : : "r" (adj_code + 10));

	 /* max. 5 instructions latency, and we need at >= 1 for returning */
	 asm("nop");
	 asm("nop");
	 asm("nop");
	 asm("nop");
      }
#else
    ERRMSG(0) "Foreign import wrapper is not supported on this architecture" 
    EEND;
#endif
    assert(pc <= &thunk->code[0] + sizeof(thunk->code));
    return &thunk->code; /* a pointer into the middle of the thunk */
}

static void freeThunkAux(struct thunk_data* thunk) {
    freeStablePtr(thunk->stable);
    if (thunk->prev) {
        assert(foreignThunks != thunk);
        thunk->prev->next = thunk->next;
    } else {        
        assert(foreignThunks == thunk);
        foreignThunks = thunk->next;
    }        
    if (thunk->next) {
        thunk->next->prev = 0;
    }
    free(thunk);
}

static void freeAllThunks(void) {
    while (foreignThunks) {
        freeThunkAux(foreignThunks);
    }
}

/* This frees the object allocated by mkThunk.
 * [A useful debugging mode would not deallocate the thunk but would
 *  overwrite the thunk with code which prints an error message.]
 */
static void freeHaskellFunctionPtr(void* t) {
    struct thunk_data* thunk = (struct thunk_data*)((char*)t - (char*)&((struct thunk_data*)0)->code);
    freeThunkAux(thunk);
}

primFun(primFreeHFunPtr) {
    Pointer x;
    PtrArg(x,3);
    freeHaskellFunctionPtr(x);
    IOReturn(nameUnit);
}

HugsAPI2* hugsAPI2() { /* build virtual function table */
    static HugsAPI2 api;
    static Bool initialised = FALSE;
    if (!initialised) {

	api.getInt        = getInt;
	api.getWord       = getWord;
	api.getAddr       = getAddr;
	api.getFloat      = getFloat;
	api.getDouble     = getDouble;
	api.getChar       = getChar;
	api.getForeign    = getForeign;
	api.getStablePtr  = getStablePtr;
	      
	api.putInt        = putInt;
	api.putWord       = putWord;
	api.putAddr       = putAddr;
	api.putFloat      = putFloat;
	api.putDouble     = putDouble;
	api.putChar       = putChar;
	api.putForeign    = putForeign;
	api.putStablePtr  = putStablePtr;
			  
	api.returnIO      = returnIO;
	api.returnId      = returnId;
	api.runIO         = runIO;

	api.freeStablePtr = freeStablePtr;

	api.registerPrims = registerPrims;

	api.garbageCollect= garbageCollect;
    }
    return &api;
}

HugsAPI3* hugsAPI3() { /* build virtual function table */
    static HugsAPI3 api;
    static Bool initialised = FALSE;
    if (!initialised) {

	api.getInt        = getInt;
	api.getWord       = getWord;
	api.getAddr       = getAddr;
	api.getFloat      = getFloat;
	api.getDouble     = getDouble;
	api.getChar       = getChar;
	api.getForeign    = getForeign;
	api.getStablePtr  = getStablePtr;
	      
	api.putInt        = putInt;
	api.putWord       = putWord;
	api.putAddr       = putAddr;
	api.putFloat      = putFloat;
	api.putDouble     = putDouble;
	api.putChar       = putChar;
	api.putForeign    = putForeign;
	api.putStablePtr  = putStablePtr;
			  
	api.returnIO      = returnIO;
	api.returnId      = returnId;
	api.runIO         = runIO;

	api.freeStablePtr = freeStablePtr;

	api.registerPrims = registerPrims;

	api.garbageCollect= garbageCollect;

        api.lookupName    = lookupName;
        api.ap            = apMany;
        api.getUnit       = getUnit;
        api.mkThunk       = mkThunk;
        api.freeThunk     = freeHaskellFunctionPtr;
	api.getBool       = getBool;
	api.putBool       = putBool;

    }
    return &api;
}

HugsAPI4* hugsAPI4() { /* build virtual function table */
    static HugsAPI4 api;
    static Bool initialised = FALSE;
    if (!initialised) {

	api.getInt        = getInt;
	api.getWord       = getWord;
	api.getAddr       = getAddr;
	api.getFloat      = getFloat4;
	api.getDouble     = getDouble4;
	api.getChar       = getChar;
	api.getForeign    = getForeign;
	api.getStablePtr  = getStablePtr;
	      
	api.putInt        = putInt;
	api.putWord       = putWord;
	api.putAddr       = putAddr;
	api.putFloat      = putFloat4;
	api.putDouble     = putDouble4;
	api.putChar       = putChar;
	api.putForeign    = putForeign;
	api.putStablePtr  = putStablePtr;
			  
	api.returnIO      = returnIO;
	api.returnId      = returnId;
	api.runIO         = runIO;

	api.freeStablePtr = freeStablePtr;

	api.registerPrims = registerPrims;

	api.garbageCollect= garbageCollect;

        api.lookupName    = lookupName;
        api.ap            = apMany;
        api.getUnit       = getUnit;
        api.mkThunk       = mkThunk;
        api.freeThunk     = freeHaskellFunctionPtr;
	api.getBool       = getBool;
	api.putBool       = putBool;

        api.getInt8       = getInt8;
        api.getInt16      = getInt16;
        api.getInt32      = getInt32;
        api.getInt64      = getInt64;
        api.getWord8      = getWord8;
        api.getWord16     = getWord16;
        api.getWord32     = getWord32;
        api.getWord64     = getWord64;
        api.getPtr        = getPtr;
        api.getFunPtr     = getFunPtr;
        api.getForeignPtr = getForeignPtr;

        api.putInt8       = putInt8;
        api.putInt16      = putInt16;
        api.putInt32      = putInt32;
        api.putInt64      = putInt64;
        api.putWord8      = putWord8;
        api.putWord16     = putWord16;
        api.putWord32     = putWord32;
        api.putWord64     = putWord64;
        api.putPtr        = putPtr;
        api.putFunPtr     = putFunPtr;
        api.putForeignPtr = putForeignPtr;

	api.makeStablePtr4 = getStablePtr;
	api.derefStablePtr4= putStablePtr;
	api.runId          = runId;

	api.getStablePtr4 = getStablePtr4;
	api.putStablePtr4 = putStablePtr4;
	api.freeStablePtr4= freeStablePtr4;
    }
    return &api;
}

void hs_perform_gc(void)
{
    garbageCollect();
}

void hs_free_stable_ptr(HsStablePtr x)
{
    freeStablePtr4(x);
}

/* --------------------------------------------------------------------------
 * Build array of character conses:
 * ------------------------------------------------------------------------*/

static Cell consCharArray[NUM_CHARS];

Cell consChar(c)                        /* return application (:) c        */
Char c; {
    if (c<0)
	c += NUM_CHARS;
    return consCharArray[c];
}

/* --------------------------------------------------------------------------
 * Built-in control:
 * ------------------------------------------------------------------------*/

/* Dummy entry */
static Void builtinControl Args((Int));
static Void builtinControl(what)
Int what; {
}
static struct primInfo builtinPrims = { builtinControl, builtinPrimTable, 0 };

Void builtIn(what)
Int what; {
    Int i;

    switch (what) {
	case MARK    : for (i=0; i<NUM_CHARS; ++i)
			   mark(consCharArray[i]);
		       break;

	case INSTALL : for (i=0; i<NUM_CHARS; ++i) {
			   consCharArray[i] = ap(nameCons,mkChar(i));
		       }
		       registerPrims(&builtinPrims);
		       registerPrims(&printerPrims);
#if HASKELL_ARRAYS
		       registerPrims(&arrayPrims);
#endif
#if BIGNUMS
		       registerPrims(&bignumPrims);
#endif
#if IO_MONAD
		       registerPrims(&iomonadPrims);
#endif
#if TIME_MODULE
		       registerPrims(&timePrims);
#endif
#if DIRECTORY_MODULE
		       registerPrims(&dirPrims);
#endif
#if INTERNAL_PRIMS
		       registerPrims(&internalPrims);
#endif
#if LAZY_ST
		       registerPrims(&stmonadPrims);
#endif
		       setCurrModule(modulePrelude);
#define pFun(n,s,t)    addPrim(0,n=newName(findText(s),NIL),t,modulePrelude,NIL)
		       pFun(nameFatbar,    "_FATBAR",  "fatbar");
		       pFun(nameFail,      "_FAIL",    "fail");
		       pFun(nameIf,        "_IF",      "if");
		       pFun(nameSel,       "_SEL",     "sel");

#if EVAL_INSTANCES
		       pFun(nameIStrict,   "_strict",  "strict");
		       pFun(nameISeq,      "_seq",     "seq");
#endif

		       pFun(nameConCmp,    "_concmp",  "conCmp");
		       pFun(nameEnRange,   "_range",   "enRange");
		       pFun(nameEnIndex,   "_index",   "enIndex");
		       pFun(nameEnInRng,   "_inRange", "enInRng");
		       pFun(nameEnToEn,    "_toEnum",  "enToEn");
		       pFun(nameEnFrEn,    "_frEnum",  "enFrEn");
		       pFun(nameEnFrom,    "_from",    "enFrom");
		       pFun(nameEnFrTo,    "_fromTo",  "enFrTo");
		       pFun(nameEnFrTh,    "_fromThen","enFrTh");

		       pFun(nameBlackHole, "_Gc Black Hole",    "gcBhole");
		       pFun(nameInd,	   "_indirect", "error");
		       name(nameInd).number = DFUNNAME;
#if    TREX
		       pFun(nameRecExt,	   "_recExt",	"recExt");
		       pFun(nameRecBrk,	   "_recBrk",	"recBrk");
		       pFun(nameRecSel,	   "_recSel",	"recSel");
		       pFun(nameRecShw,	   "_recShw",	"recShw");
		       pFun(nameRecEq,	   "_recEq",	"recEq");
		       pFun(nameAddEv,	   "_addEv",	"primPlusInt");
		       name(nameAddEv).number = DFUNNAME;
#endif
#undef pFun
#define predef(nm,str) nm=newName(findText(str),NIL); name(nm).defn=PREDEFINED
		       predef(nameNegate,       "negate");
		       predef(nameFlip,         "flip");
		       predef(nameFrom,         "enumFrom");
		       predef(nameFromThen,     "enumFromThen");
		       predef(nameFromTo,       "enumFromTo");
		       predef(nameFromThenTo,   "enumFromThenTo");
		       predef(nameFst,		"fst");
		       predef(nameSnd,		"snd");
		       predef(nameAnd,          "&&");
		       predef(nameOr,           "||");
		       predef(nameId,           "id");
		       predef(nameOtherwise,    "otherwise");
		       predef(namePrimThrow,    "primThrowException");
		       predef(nameError,        "error");
		       predef(nameUndefined,    "undefined");
		       predef(nameComp,         ".");
		       predef(nameApp,          "++");
		       predef(nameShowField,    "showField");
		       predef(nameShowParen,    "showParen");
		       predef(nameReadField,    "readField");
		       predef(nameReadParen,    "readParen");
		       predef(nameLex,          "lex");
		       predef(nameRangeSize,    "rangeSize");
		       predef(nameCompAux,      "primCompAux");
		       predef(namePmInt,        "primPmInt");
		       predef(namePmInteger,    "primPmInteger");
		       predef(namePmFlt,        "primPmFlt");
		       predef(nameReturnIO,     "primretIO");
#if NPLUSK
		       predef(namePmNpk,        "primPmNpk");
		       predef(namePmSub,        "primPmSub");
#endif
#if SHORT_CIRCUIT_COERCIONS
		       predef(nameRationalToFloat,  "rationalToFloat");
		       predef(nameFloatToRational,  "floatToRational");
		       predef(nameDoubleToRational, "doubleToRational");
		       predef(nameDoubleToRatio,    "doubleToRatio");
		       predef(nameIntToRatio,       "intToRatio");
		       predef(nameIntToFloat,       "primIntToFloat");
#endif
#undef  predef
		       break;

        case RESET   : freeAllThunks();
                       break;
    }
}

/*-------------------------------------------------------------------------*/

