/* --------------------------------------------------------------------------
 * Definition of the Hugs server API
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the Oregon Graduate Institute of Science and
 * Technology, 1994-1999, All rights reserved.  It is distributed as
 * free software under the license in the file "License", which is
 * included in the distribution.
 *
 * $RCSfile: server.h,v $
 * $Revision: 1.3 $
 * $Date: 1999/09/13 11:01:06 $
 * ------------------------------------------------------------------------*/

#ifndef Args
# if HAVE_PROTOTYPES
#  define Args(x) x
# else
#  define Args(x) ()
# endif
#endif /* !defined Args */

typedef int HVal;     /* Haskell values are represented by stable pointers */

typedef struct _HugsServerAPI {
    char* (*clearError     ) Args((void));
    void  (*setHugsArgs    ) Args((int, char**));
    int   (*getNumScripts  ) Args((void));
    void  (*reset          ) Args((int));
    void  (*setOutputEnable) Args((unsigned));
    void  (*changeDir      ) Args((char*));
    void  (*loadProject    ) Args((char*));
    void  (*loadFile       ) Args((char*));
    HVal  (*compileExpr    ) Args((char*,char*));

    void  (*lookupName     ) Args((char*,char*)); /* push values onto stack*/
    void  (*mkInt          ) Args((int));
    void  (*mkString       ) Args((char*));

    void  (*apply          ) Args((void));      /* manipulate top of stack */

    int   (*evalInt        ) Args((void));      /* evaluate top of stack   */
    char* (*evalString     ) Args((void));
    int   (*doIO           ) Args((void));

    HVal  (*popHVal        ) Args((void));      /* pop stack               */
    void  (*pushHVal       ) Args((HVal));      /* push back onto stack    */
    void  (*freeHVal       ) Args((HVal)); 
} HugsServerAPI;

/* type of "initHugsServer" function */
typedef HugsServerAPI *(*HugsServerInitFun) Args((int, char**));

/* ------------------------------------------------------------------------*/
