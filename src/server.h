/* --------------------------------------------------------------------------
 * Definition of the Hugs server API
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2002, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 * $RCSfile: server.h,v $
 * $Revision: 1.8 $
 * $Date: 2003/03/03 06:31:04 $
 * ------------------------------------------------------------------------*/

#ifndef Args
# if HAVE_PROTOTYPES
#  define Args(x) x
# else
#  define Args(x) ()
# endif
#endif /* !defined Args */

#ifdef __cplusplus 
extern "C" {
#endif

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
    void  (*loadFromBuffer ) Args((char*));
    void  (*setOptions     ) Args((char*));
    char* (*getOptions     ) Args((void));
    HVal  (*compileExpr    ) Args((char*,char*));
    void  (*garbageCollect ) Args((void));

    void  (*lookupName     ) Args((char*,char*)); /* push values onto stack*/

    void  (*mkInt          ) Args((int));
    void  (*mkAddr         ) Args((void*));
    void  (*mkString       ) Args((char*));

    void  (*apply          ) Args((void));      /* manipulate top of stack */

    int   (*evalInt        ) Args((void));      /* evaluate top of stack   */
    void* (*evalAddr       ) Args((void));
    char* (*evalString     ) Args((void));

    int   (*doIO           ) Args((void));
    int   (*doIO_Int       ) Args((int*));
    int   (*doIO_Addr      ) Args((void**));

    HVal  (*popHVal        ) Args((void));      /* pop stack               */
    void  (*pushHVal       ) Args((HVal));      /* push back onto stack    */
    void  (*freeHVal       ) Args((HVal)); 
} HugsServerAPI;

/* type of "initHugsServer" function */
typedef HugsServerAPI *(*HugsServerInitFun) Args((int, char**));
/* type of "shutdownHugsServer" function */
typedef void (*HugsServerShutdownFun) Args((HugsServerAPI*));

  //extern HugsServerAPI* initHugsServer Args((int, char**));
DLLEXPORT(HugsServerAPI*) initHugsServer Args((Int, String[]));
#ifdef __cplusplus 
};
#endif

/* ------------------------------------------------------------------------*/
