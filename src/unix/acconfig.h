/* acconfig.h

   Descriptive text for the C preprocessor macros that
   the Hugs configuration script can define.
   The current version may not use all of them; autoheader copies the ones
   your configure.in uses into your configuration header file templates.

   The entries are in sort -df order: alphabetical, case insensitive,
   ignoring punctuation (such as underscores).  Although this order
   can split up related entries, it makes it easier to check whether
   a given entry is in the file.

   Leave the following blank line there!!  Autoheader needs it.  */


/* The following symbols are defined in options.h:
 * 
 *   BYTECODE_PRIMS
 *   CHECK_TAGS
 *   DEBUG_CODE
 *   DEBUG_PRINTER
 *   DONT_PANIC
 *   GIMME_STACK_DUMPS
 *   HUGSDIR
 *   HUGSPATH
 *   HUGS_FOR_WINDOWS
 *   HUGS_VERSION
 *   HUGS_VERSION_STRING
 *   IGNORE_MODULES
 *   INTERNAL_PRIMS
 *   LARGE_HUGS
 *   PATH_CANONICALIZATION
 *   PROFILING
 *   REGULAR_HUGS
 *   SMALL_BANNER
 *   SMALL_HUGS
 *   USE_DOUBLE_PRECISION
 *   USE_PREPROCESSOR
 *   USE_READLINE
 *   WANT_TIMER
 *   HASKELL_98_ONLY
 */

/* Define if you have malloc.h and it defines _alloca - eg for Visual C++. */
#define HAVE__ALLOCA 0

/* Define if you have /bin/sh */
#define HAVE_BIN_SH 0

/* Define if you have the GetModuleFileName function.  */
#define HAVE_GETMODULEFILENAME 0

/* Define if heap profiler can (and should) automatically invoke hp2ps
 * to convert heap profile (in "profile.hp") to postscript.
 */
#define HAVE_HP2PS 0

/* Define if compiler supports gcc's "labels as values" (aka computed goto)
 * feature (which is used to speed up instruction dispatch in the interpreter).
 * Here's what typical code looks like:
 *
 * void *label[] = { &&l1, &&l2 };
 * ...
 * goto *label[i];
 * l1: ...
 * l2: ...
 * ...
 */
#define HAVE_LABELS_AS_VALUES 0

/* Define if compiler supports prototypes. */
#define HAVE_PROTOTYPES 0

/* Define if you have the WinExec function.  */
#define HAVE_WINEXEC 0

/* Define if jmpbufs can be treated like arrays.
 * That is, if the following code compiles ok:
 *
 * #include <setjmp.h>
 * 
 * int test1() {
 *     jmp_buf jb[1];
 *     jmp_buf *jbp = jb;
 *     return (setjmp(jb[0]) == 0);
 * }
 */
#define JMPBUF_ARRAY   0

/* Define if your C compiler inserts underscores before symbol names */
#undef LEADING_UNDERSCORE

/* Define if signal handlers have type void (*)(int)
 * (Otherwise, they're assumed to have type int (*)(void).)
 */
#define VOID_INT_SIGNALS 0
 

/* Leave that blank line there!!  Autoheader needs it.
   If you're adding to this file, keep in mind:
   The entries are in sort -df order: alphabetical, case insensitive,
   ignoring punctuation (such as underscores).  */


/* autoheader doesn't grok AC_CHECK_LIB_NOWARN so we have to add them
   manually.  */

@BOTTOM@

/* Define if you have the dl library (-ldl).  */
#undef HAVE_LIBDL

/* Define if you have the dld library (-ldld).  */
#undef HAVE_LIBDLD

/* Define if you have the m library (-lm).  */
#undef HAVE_LIBM

/* Define if you have the editline library (-leditline).  */
#undef HAVE_LIBREADLINE


