/* --------------------------------------------------------------------------
 * Primitive functions, input output etc...
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the Oregon Graduate Institute of Science and
 * Technology, 1994-1999, All rights reserved.  It is distributed as
 * free software under the license in the file "License", which is
 * included in the distribution.
 *
 * $RCSfile: builtin.c,v $
 * $Revision: 1.10 $
 * $Date: 2001/01/31 02:52:13 $
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

Name nameFst,     nameSnd;          /* 2-tuple selector functions      */
Name nameAnd,     nameOr;               /* built-in logical connectives    */
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
PROTO_PRIM(primWordToInt);
PROTO_PRIM(primIntToWord);
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
#endif

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
#endif

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
  {"intToWord",         1, primIntToWord},
  {"wordToInt",         1, primWordToInt},
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

primFun(primCatchError) {           /* Error catching  primitive       */
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

primFun(primWordToInt) {               /* Word to integer primitive        */
    Unsigned x;
    WordArg(x,1);
    IntResult(x);
}

primFun(primIntToWord) {               /* Integer to word primitive        */
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

IntInt2Bool(primEqInt,x==y)            /* Integer equality primitive       */
WordWord2Bool(primEqWord,x==y)         /* Natural equality primitive       */
CharChar2Bool(primEqChar,x==y)         /* Character equality primitive     */
FloatFloat2Bool(primEqFloat, x==y)     /* Float equality primitive         */

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
    Cell exp, obsCell;                  /* initiate dialogue               */
    Int i=0;
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
 * table hugsAPI2.
 *-------------------------------------------------------------------------*/

static void           getUnit        Args((void));
static int            getInt         Args((void));
static unsigned int   getWord        Args((void));
static void*          getAddr        Args((void));
static float          getFloat       Args((void));
static double         getDouble      Args((void));
static char           getChar        Args((void));
static HugsForeign    getForeign     Args((void));
static HugsStablePtr  getStablePtr   Args((void));
static int            getBool        Args((void));
	      
static void           putInt         Args((int));
static void           putWord        Args((unsigned int));
static void           putAddr        Args((void*));
static void           putFloat       Args((double));
static void           putDouble      Args((double));
static void           putChar        Args((char));
static void           putForeign     Args((HugsForeign, void (*)(void *)));
static void           putStablePtr   Args((HugsStablePtr));
static void           putBool        Args((int));
	      
static void           returnIO       Args((HugsStackPtr, int));
static void           returnId       Args((HugsStackPtr, int));
static int            runIO          Args((int));
static void           apMany         Args((int));

static void           getUnit()      { eval(pop()); }
static int            getInt()       { eval(pop()); checkInt();   return whnfInt; }
static unsigned int   getWord()      { eval(pop()); checkWord();  return (unsigned int) whnfInt; }
static void*          getAddr()      { eval(pop()); checkPtr();   return ptrOf(whnfHead); }
static float          getFloat()     { eval(pop()); checkFloat(); return whnfFloat; }
static double         getDouble()    { eval(pop()); checkFloat(); return (double) whnfFloat; }
static char           getChar()      { eval(pop()); checkChar();  return charOf(whnfHead); }
static HugsForeign    getForeign()   { eval(pop()); return derefMP(whnfHead); }
static int            getBool()      { eval(pop()); checkBool();  return (whnfHead == nameTrue); }

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

static void putInt(x)    int    x; { push(mkInt(x)); }
static void putWord(x)   unsigned int x; { push(mkInt((int)x)); }
static void putAddr(x)   void*  x; { push(mkPtr(x)); }
#if HAVE_PROTOTYPES
/* Symantec C and Solaris acc object to old style function headings
 * for this one function.  We continue to prefer pre-ANSI headers
 * elsewhere - for portability reasons.
 */
static void putChar(char x)        { push(mkChar(x)); }
#else
static void putChar(x)   char   x; { push(mkChar(x)); }
#endif
static void putFloat(x)  double x; { push(mkFloat(x)); }
static void putDouble(x) double x; { push(mkFloat(x)); }
static void putForeign(x,f) HugsForeign x; void (*f)(HugsForeign); { push(mkMallocPtr(x,f)); }
static void putStablePtr(x) HugsStablePtr x; { push(derefStablePtr(x)); }
static void putBool(x)   int    x; { push(x?nameTrue:nameFalse); }

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
       /* do nothing */
    } else {
	int i;
	push(mkTuple(n));
	for(i=0; i<n; ++i) {
	    pushed(1) = ap(pushed(0),pushed(1));
	    drop();
	}
    }
    updateRoot(top());
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
    char               code[16];
};

struct thunk_data* foreignThunks = 0;

static void* mkThunk(void* app, HugsStablePtr s) {
    struct thunk_data* thunk 
        = (struct thunk_data*)malloc(sizeof(struct thunk_data));
    char* pc;
    if (!thunk) {
        /* ToDo: better cleanup */
        printf("Can't allocate thunk for foreign export dynamic\n");
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
#if defined(__i386__)
    /* 3 bytes: pushl (%esp) */
    *pc++ = 0xff; *pc++ = 0x34; *pc++ = 0x24;  

    /* 8 bytes: movl s,4(%esp) */
    *pc++ = 0xc7; *pc++ = 0x44; *pc++ = 0x24; *pc++ = 0x04; 
    *((HugsStablePtr*)pc)++ = s;

    /* 5 bytes: jmp app */
    *pc++ = 0xe9;
    *((int*)pc)++ = (char*)app - ((char*)&(thunk->code[16]));
#else
    ERRMSG(0) "Foreign export dynamic is not supported on this architecture" 
    EEND;
#endif
    assert(pc <= &thunk->code[16]);
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
		       pFun(nameEnToEn,    "_ToEnum",  "enToEn");
		       pFun(nameEnFrEn,    "_FrEnum",  "enFrEn");
		       pFun(nameEnFrom,    "_From",    "enFrom");
		       pFun(nameEnFrTo,    "_FromTo",  "enFrTo");
		       pFun(nameEnFrTh,    "_FromThen","enFrTh");

		       pFun(nameBlackHole, "_Gc Black Hole",    "gcBhole");
		       pFun(nameInd,	   "_indirect","error");
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

