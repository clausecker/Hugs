/* src/mac/config.h.  
 * Generated manually from src/config.h.in by Hans Aberg for Mac/Symantec C
 */

/* Define if using alloca.c.  */
#undef C_ALLOCA

/* Define to empty if the keyword does not work.  */
#undef const

/* Define to one of _getb67, GETB67, getb67 for Cray-2 and Cray-YMP systems.
   This function is required for alloca.c support on those systems.  */
#undef CRAY_STACKSEG_END

/* Define if you have alloca, as a function or macro.  */
#undef HAVE_ALLOCA

/* Define if you have <alloca.h> and it should be used (not on Ultrix).  */
#undef HAVE_ALLOCA_H

/* Define if you have <sys/wait.h> that is POSIX.1 compatible.  */
#undef HAVE_SYS_WAIT_H

/* Define if you need to in order for stat and other things to work.  */
#undef _POSIX_SOURCE

/* Define as the return type of signal handlers (int or void).  */
#define RETSIGTYPE int

/* If using the C implementation of alloca, define if you know the
   direction of stack growth for your system; otherwise it will be
   automatically deduced at run-time.
	STACK_DIRECTION > 0 => grows toward higher addresses
	STACK_DIRECTION < 0 => grows toward lower addresses
	STACK_DIRECTION = 0 => direction of growth unknown
 */
#undef STACK_DIRECTION

/* Define if you have the ANSI C header files.  */
#define STDC_HEADERS 1

/* Define if you can safely include both <sys/time.h> and <time.h>.  */
#undef TIME_WITH_SYS_TIME

/* Define if your <sys/time.h> declares struct tm.  */
#undef TM_IN_SYS_TIME

/* The following symbols are defined in options.h:
 * 
 *   CHECK_TAGS
 *   DEBUG_CODE
 *   DEBUG_PRINTER
 *   HUGSDIR
 *   HUGSPATH
 *   HUGS_FOR_WINDOWS
 *   HUGS_VERSION
 *   HUGS_VERSION_STRING
 *   IGNORE_MODULES
 *   LARGE_HUGS
 *   PATH_CANONICALIZATION
 *   PROFILING
 *   REGULAR_HUGS
 *   SMALL_BANNER
 *   SMALL_HUGS
 *   TREX
 *   IPARAM
 *   USE_READLINE
 *   USE_DOUBLE_PRECISION
 *   WANT_TIMER
 *   INTERNAL_PRIMS
 *   BYTECODE_PRIMS
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
#define HAVE_PROTOTYPES 1

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

/* Define if signal handlers have type void (*)(int)
 * (Otherwise, they're assumed to have type int (*)(void).)
 */
#define VOID_INT_SIGNALS 1

/* The number of bytes in a double.  */
#define SIZEOF_DOUBLE 8

/* The number of bytes in a float.  */
#define SIZEOF_FLOAT 4

/* The number of bytes in a int.  */
#define SIZEOF_INT 4

/* The number of bytes in a int*.  */
#define SIZEOF_INTP 4

/* Define if you have the _fullpath function.  */
#undef HAVE__FULLPATH

/* Define if you have the _snprintf function.  */
#undef HAVE__SNPRINTF

/* Define if you have the _stricmp function.  */
#undef HAVE__STRICMP

/* Define if you have the _vsnprintf function.  */
#undef HAVE__VSNPRINTF

/* Define if you have the access function.  */
#undef HAVE_ACCESS

/* Define if you have the farcalloc function.  */
#undef HAVE_FARCALLOC

/* Define if you have the fgetpos function.  */
#define HAVE_FGETPOS 1

/* Define if you have the fseek function.  */
#define HAVE_FSEEK 1

/* Define if you have the fsetpos function.  */
#define HAVE_FSETPOS 1

/* Define if you have the ftell function.  */
#define HAVE_FTELL 1

/* Define if you have the poly function.  */
#undef HAVE_POLY

/* Define if you have the realpath function.  */
#undef HAVE_REALPATH

/* Define if you have the snprintf function.  */
#undef HAVE_SNPRINTF

/* Define if you have the stime function.  */
#undef HAVE_STIME

/* Define if you have the strcasecmp function.  */
#undef HAVE_STRCASECMP

/* Define if you have the strcmp function.  */
#define HAVE_STRCMP 1

/* Define if you have the stricmp function.  */
#undef HAVE_STRICMP

/* Define if you have the valloc function.  */
#undef HAVE_VALLOC

/* Define if you have the vsnprintf function.  */
#undef HAVE_VSNPRINTF

/* Define if you have the <assert.h> header file.  */
#define HAVE_ASSERT_H 1

/* Define if you have the <conio.h> header file.  */
#undef HAVE_CONIO_H

/* Define if you have the <ctype.h> header file.  */
#define HAVE_CTYPE_H 1

/* Define if you have the <ddraw.h> header file.  */
#undef HAVE_DDRAW_H

/* Define if you have the <dl.h> header file.  */
#undef HAVE_DL_H

/* Define if you have the <dlfcn.h> header file.  */
#undef HAVE_DLFCN_H

/* Define if you have the <dos.h> header file.  */
#undef HAVE_DOS_H

/* Define if you have the <dsound.h> header file.  */
#undef HAVE_DSOUND_H

/* Define if you have the <fcntl.h> header file.  */
#define HAVE_FCNTL_H 1

/* Define if you have the <float.h> header file.  */
#define HAVE_FLOAT_H 1

/* Define if you have the <io.h> header file.  */
#define HAVE_IO_H 1

/* Define if you have the <sgtty.h> header file.  */
#undef HAVE_SGTTY_H

/* Define if you have the <signal.h> header file.  */
#define HAVE_SIGNAL_H 1

/* Define if you have the <std.h> header file.  */
#undef HAVE_STD_H

/* Define if you have the <stdarg.h> header file.  */
#define HAVE_STDARG_H 1

/* Define if you have the <stdlib.h> header file.  */
#define HAVE_STDLIB_H 1

/* Define if you have the <string.h> header file.  */
#define HAVE_STRING_H 1

/* Define if you have the <sys/ioctl.h> header file.  */
#undef HAVE_SYS_IOCTL_H

/* Define if you have the <sys/param.h> header file.  */
#undef HAVE_SYS_PARAM_H

/* Define if you have the <sys/resource.h> header file.  */
#undef HAVE_SYS_RESOURCE_H

/* Define if you have the <sys/stat.h> header file.  */
#undef HAVE_SYS_STAT_H

/* Define if you have the <sys/time.h> header file.  */
#undef HAVE_SYS_TIME_H

/* Define if you have the <sys/types.h> header file.  */
#undef HAVE_SYS_TYPES_H

/* Define if you have the <termio.h> header file.  */
#undef HAVE_TERMIO_H

/* Define if you have the <termios.h> header file.  */
#undef HAVE_TERMIOS_H

/* Define if you have the <time.h> header file.  */
#define HAVE_TIME_H 1

/* Define if you have the <unistd.h> header file.  */
#undef HAVE_UNISTD_H

/* Define if you have the <values.h> header file.  */
#undef HAVE_VALUES_H

/* Define if you have the <windows.h> header file.  */
#undef HAVE_WINDOWS_H

/* Define if you have the editline library (-leditline).  */
#undef HAVE_LIBEDITLINE

/* Define if you have the dl library (-ldl).  */
#undef HAVE_LIBDL

/* Define if you have the dld library (-ldld).  */
#undef HAVE_LIBDLD

/* Define if you have the m library (-lm).  */
#define HAVE_LIBM 1

/* Define if you have the editline library (-leditline).  */
#undef HAVE_LIBREADLINE

/* --------------------------------------------------------------------------
 * Machintosh Symantec C/C++ v8r5 features.
 * These are likely to disappear/change in future versions and should not
 * be used by most people..
 * ------------------------------------------------------------------------*/

/* Define if you want a Mac version of chdir().  */
#define HAVE_PBHSETVOLSYNC 1

/* Define if you have the Symantec C <console.h> header file.  */
#define HAVE_CONSOLE_H 1

/* Define if you have the Symantec C <pascal.h> header file.  */
#define HAVE_PASCAL_H 1

/* Define if you have the MacOS <Files.h> header file.  */
#define HAVE_FILES_H 1

/* Define if you have the Std C <errno.h> header file.  */
#define HAVE_ERRNO_H 1

/* Define if you have the Symantec C <unix.h> header file.  */
#define HAVE_UNIX_H 1

/* Define if you want a Mac version of file access checks.  */
#define HAVE_GETFINFO 1

/* Define if you want to use a Mac version macsystem() of the UNIX system(). */
#define HAVE_MACSYSTEM 1

/* ----------------------------------------------------------------------- */

