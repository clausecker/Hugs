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
 *   HUGSSUFFIXES
 *   HUGS_FOR_WINDOWS
 *   HUGS_VERSION
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

/* Define to alignment constraint on chars */
#undef ALIGNMENT_CHAR

/* Define to alignment constraint on doubles */
#undef ALIGNMENT_DOUBLE

/* Define to alignment constraint on floats */
#undef ALIGNMENT_FLOAT

/* Define to alignment constraint on ints */
#undef ALIGNMENT_INT

/* Define to alignment constraint on longs */
#undef ALIGNMENT_LONG

/* Define to alignment constraint on long longs */
#undef ALIGNMENT_LONG_LONG

/* Define to alignment constraint on shorts */
#undef ALIGNMENT_SHORT

/* Define to alignment constraint on unsigned chars */
#undef ALIGNMENT_UNSIGNED_CHAR

/* Define to alignment constraint on unsigned ints */
#undef ALIGNMENT_UNSIGNED_INT

/* Define to alignment constraint on unsigned longs */
#undef ALIGNMENT_UNSIGNED_LONG

/* Define to alignment constraint on unsigned long longs */
#undef ALIGNMENT_UNSIGNED_LONG_LONG

/* Define to alignment constraint on unsigned shorts */
#undef ALIGNMENT_UNSIGNED_SHORT

/* Define to alignment constraint on void pointers */
#undef ALIGNMENT_VOID_P

/* C compiler invocation use to build a dynamically loadable library.
 * Typical value: "gcc -shared"
 * Must evaluate to a literal C string.
 */
#define MKDLL_CMD ""

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

/* Define if C compiler supports long long types */
#undef HAVE_LONG_LONG

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

/* Define if time.h or sys/time.h define the altzone variable.  */
#undef HAVE_ALTZONE

/* Define if time.h or sys/time.h define the timezone variable.  */
#undef HAVE_TIMEZONE
 

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

/* Define to Haskell type for cc_t */
#undef HTYPE_CC_T

/* Define to Haskell type for char */
#undef HTYPE_CHAR

/* Define to Haskell type for clock_t */
#undef HTYPE_CLOCK_T

/* Define to Haskell type for dev_t */
#undef HTYPE_DEV_T

/* Define to Haskell type for signed double */
#undef HTYPE_DOUBLE

/* Define to Haskell type for float */
#undef HTYPE_FLOAT

/* Define to Haskell type for gid_t */
#undef HTYPE_GID_T

/* Define to Haskell type for GLbitfield */
#undef HTYPE_GLBITFIELD

/* Define to Haskell type for GLboolean */
#undef HTYPE_GLBOOLEAN

/* Define to Haskell type for GLbyte */
#undef HTYPE_GLBYTE

/* Define to Haskell type for GLclampd */
#undef HTYPE_GLCLAMPD
 
/* Define to Haskell type for GLclampf */
#undef HTYPE_GLCLAMPF

/* Define to Haskell type for GLdouble */
#undef HTYPE_GLDOUBLE

/* Define to Haskell type for GLenum */
#undef HTYPE_GLENUM

/* Define to Haskell type for GLfloat */
#undef HTYPE_GLFLOAT

/* Define to Haskell type for GLint */
#undef HTYPE_GLINT

/* Define to Haskell type for GLshort */
#undef HTYPE_GLSHORT

/* Define to Haskell type for GLsizei */
#undef HTYPE_GLSIZEI

/* Define to Haskell type for GLubyte */
#undef HTYPE_GLUBYTE

/* Define to Haskell type for GLuint */
#undef HTYPE_GLUINT

/* Define to Haskell type for GLushort */
#undef HTYPE_GLUSHORT

/* Define to Haskell type for int */
#undef HTYPE_INT

/* Define to Haskell type for ino_t */
#undef HTYPE_INO_T

/* Define to Haskell type for long */
#undef HTYPE_LONG

/* Define to Haskell type for long long */
#undef HTYPE_LONG_LONG

/* Define to Haskell type for mode_t */
#undef HTYPE_MODE_T

/* Define to Haskell type for nlink_t */
#undef HTYPE_NLINK_T

/* Define to Haskell type for off_t */
#undef HTYPE_OFF_T

/* Define to Haskell type for pid_t */
#undef HTYPE_PID_T

/* Define to Haskell type for ptrdiff_t */
#undef HTYPE_PTRDIFF_T

/* Define to Haskell type for short */
#undef HTYPE_SHORT

/* Define to Haskell type for sig_atomic_t */
#undef HTYPE_SIG_ATOMIC_T

/* Define to Haskell type for signed char */
#undef HTYPE_SIGNED_CHAR

/* Define to Haskell type for size_t */
#undef HTYPE_SIZE_T

/* Define to Haskell type for speed_t */
#undef HTYPE_SPEED_T

/* Define to Haskell type for ssize_t */
#undef HTYPE_SSIZE_T

/* Define to Haskell type for time_t */
#undef HTYPE_TIME_T

/* Define to Haskell type for tcflag_t */
#undef HTYPE_TCFLAG_T

/* Define to Haskell type for uid_t */
#undef HTYPE_UID_T

/* Define to Haskell type for unsigned char */
#undef HTYPE_UNSIGNED_CHAR

/* Define to Haskell type for unsigned int */
#undef HTYPE_UNSIGNED_INT

/* Define to Haskell type for unsigned long */
#undef HTYPE_UNSIGNED_LONG

/* Define to Haskell type for unsigned long long */
#undef HTYPE_UNSIGNED_LONG_LONG

/* Define to Haskell type for unsigned short */
#undef HTYPE_UNSIGNED_SHORT

/* Define to Haskell type for wchar_t */
#undef HTYPE_WCHAR_T

