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
 *   INTERNAL_PRIMS
 *   LARGE_HUGS
 *   PATH_CANONICALIZATION
 *   PROFILING
 *   REGULAR_HUGS
 *   SMALL_BANNER
 *   SMALL_HUGS
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

/* Define if we want to use Apple's OpenGL for the Quartz Display System on Mac OS X (instead of X11) */   
#undef USE_QUARTZ_OPENGL


/* Leave that blank line there!!  Autoheader needs it.
   If you're adding to this file, keep in mind:
   The entries are in sort -df order: alphabetical, case insensitive,
   ignoring punctuation (such as underscores).  */


/* autoheader doesn't grok AC_CHECK_LIB_NOWARN so we have to add them
   manually.  */

@BOTTOM@

/* Define if netinet/in.h defines the in_addr type.  */
#undef HAVE_IN_ADDR_T

/* Define if you have the dl library (-ldl).  */
#undef HAVE_LIBDL

/* Define if you have the dld library (-ldld).  */
#undef HAVE_LIBDLD

/* Define if you have the m library (-lm).  */
#undef HAVE_LIBM

/* Define if you have the editline library (-leditline).  */
#undef HAVE_LIBREADLINE

/* Define if struct msghdr contains msg_accrights field */
#undef HAVE_MSGHDR_MSG_ACCRIGHTS
 
/* Define if struct msghdr contains msg_control field */
#undef HAVE_MSGHDR_MSG_CONTROL

/* Host cpu architecture */
#undef HOST_ARCH

/* Host operating system */
#undef HOST_OS

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

/* Define to Haskell type for rlim_t */
#undef HTYPE_RLIM_T

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

/* The value of E2BIG.  */
#undef CONST_E2BIG

/* The value of EACCES.  */
#undef CONST_EACCES

/* The value of EADDRINUSE.  */
#undef CONST_EADDRINUSE

/* The value of EADDRNOTAVAIL.  */
#undef CONST_EADDRNOTAVAIL

/* The value of EADV.  */
#undef CONST_EADV

/* The value of EAFNOSUPPORT.  */
#undef CONST_EAFNOSUPPORT

/* The value of EAGAIN.  */
#undef CONST_EAGAIN

/* The value of EALREADY.  */
#undef CONST_EALREADY

/* The value of EBADF.  */
#undef CONST_EBADF

/* The value of EBADMSG.  */
#undef CONST_EBADMSG

/* The value of EBADRPC.  */
#undef CONST_EBADRPC

/* The value of EBUSY.  */
#undef CONST_EBUSY

/* The value of ECHILD.  */
#undef CONST_ECHILD

/* The value of ECOMM.  */
#undef CONST_ECOMM

/* The value of ECONNABORTED.  */
#undef CONST_ECONNABORTED

/* The value of ECONNREFUSED.  */
#undef CONST_ECONNREFUSED

/* The value of ECONNRESET.  */
#undef CONST_ECONNRESET

/* The value of EDEADLK.  */
#undef CONST_EDEADLK

/* The value of EDESTADDRREQ.  */
#undef CONST_EDESTADDRREQ

/* The value of EDIRTY.  */
#undef CONST_EDIRTY

/* The value of EDOM.  */
#undef CONST_EDOM

/* The value of EDQUOT.  */
#undef CONST_EDQUOT

/* The value of EEXIST.  */
#undef CONST_EEXIST

/* The value of EFAULT.  */
#undef CONST_EFAULT

/* The value of EFBIG.  */
#undef CONST_EFBIG

/* The value of EFTYPE.  */
#undef CONST_EFTYPE

/* The value of EHOSTDOWN.  */
#undef CONST_EHOSTDOWN

/* The value of EHOSTUNREACH.  */
#undef CONST_EHOSTUNREACH

/* The value of EIDRM.  */
#undef CONST_EIDRM

/* The value of EILSEQ.  */
#undef CONST_EILSEQ

/* The value of EINPROGRESS.  */
#undef CONST_EINPROGRESS

/* The value of EINTR.  */
#undef CONST_EINTR

/* The value of EINVAL.  */
#undef CONST_EINVAL

/* The value of EIO.  */
#undef CONST_EIO

/* The value of EISCONN.  */
#undef CONST_EISCONN

/* The value of EISDIR.  */
#undef CONST_EISDIR

/* The value of ELOOP.  */
#undef CONST_ELOOP

/* The value of EMFILE.  */
#undef CONST_EMFILE

/* The value of EMLINK.  */
#undef CONST_EMLINK

/* The value of EMSGSIZE.  */
#undef CONST_EMSGSIZE

/* The value of EMULTIHOP.  */
#undef CONST_EMULTIHOP

/* The value of ENAMETOOLONG.  */
#undef CONST_ENAMETOOLONG

/* The value of ENETDOWN.  */
#undef CONST_ENETDOWN

/* The value of ENETRESET.  */
#undef CONST_ENETRESET

/* The value of ENETUNREACH.  */
#undef CONST_ENETUNREACH

/* The value of ENFILE.  */
#undef CONST_ENFILE

/* The value of ENOBUFS.  */
#undef CONST_ENOBUFS

/* The value of ENODATA.  */
#undef CONST_ENODATA

/* The value of ENODEV.  */
#undef CONST_ENODEV

/* The value of ENOENT.  */
#undef CONST_ENOENT

/* The value of ENOEXEC.  */
#undef CONST_ENOEXEC

/* The value of ENOLCK.  */
#undef CONST_ENOLCK

/* The value of ENOLINK.  */
#undef CONST_ENOLINK

/* The value of ENOMEM.  */
#undef CONST_ENOMEM

/* The value of ENOMSG.  */
#undef CONST_ENOMSG

/* The value of ENONET.  */
#undef CONST_ENONET

/* The value of ENOPROTOOPT.  */
#undef CONST_ENOPROTOOPT

/* The value of ENOSPC.  */
#undef CONST_ENOSPC

/* The value of ENOSR.  */
#undef CONST_ENOSR

/* The value of ENOSTR.  */
#undef CONST_ENOSTR

/* The value of ENOSYS.  */
#undef CONST_ENOSYS

/* The value of ENOTBLK.  */
#undef CONST_ENOTBLK

/* The value of ENOTCONN.  */
#undef CONST_ENOTCONN

/* The value of ENOTDIR.  */
#undef CONST_ENOTDIR

/* The value of ENOTEMPTY.  */
#undef CONST_ENOTEMPTY

/* The value of ENOTSOCK.  */
#undef CONST_ENOTSOCK

/* The value of ENOTTY.  */
#undef CONST_ENOTTY

/* The value of ENXIO.  */
#undef CONST_ENXIO

/* The value of EOPNOTSUPP.  */
#undef CONST_EOPNOTSUPP

/* The value of EPERM.  */
#undef CONST_EPERM

/* The value of EPFNOSUPPORT.  */
#undef CONST_EPFNOSUPPORT

/* The value of EPIPE.  */
#undef CONST_EPIPE

/* The value of EPROCLIM.  */
#undef CONST_EPROCLIM

/* The value of EPROCUNAVAIL.  */
#undef CONST_EPROCUNAVAIL

/* The value of EPROGMISMATCH.  */
#undef CONST_EPROGMISMATCH

/* The value of EPROGUNAVAIL.  */
#undef CONST_EPROGUNAVAIL

/* The value of EPROTO.  */
#undef CONST_EPROTO

/* The value of EPROTONOSUPPORT.  */
#undef CONST_EPROTONOSUPPORT

/* The value of EPROTOTYPE.  */
#undef CONST_EPROTOTYPE

/* The value of ERANGE.  */
#undef CONST_ERANGE

/* The value of EREMCHG.  */
#undef CONST_EREMCHG

/* The value of EREMOTE.  */
#undef CONST_EREMOTE

/* The value of EROFS.  */
#undef CONST_EROFS

/* The value of ERPCMISMATCH.  */
#undef CONST_ERPCMISMATCH

/* The value of ERREMOTE.  */
#undef CONST_ERREMOTE

/* The value of ESHUTDOWN.  */
#undef CONST_ESHUTDOWN

/* The value of ESOCKTNOSUPPORT.  */
#undef CONST_ESOCKTNOSUPPORT

/* The value of ESPIPE.  */
#undef CONST_ESPIPE

/* The value of ESRCH.  */
#undef CONST_ESRCH

/* The value of ESRMNT.  */
#undef CONST_ESRMNT

/* The value of ESTALE.  */
#undef CONST_ESTALE

/* The value of ETIME.  */
#undef CONST_ETIME

/* The value of ETIMEDOUT.  */
#undef CONST_ETIMEDOUT

/* The value of ETOOMANYREFS.  */
#undef CONST_ETOOMANYREFS

/* The value of ETXTBSY.  */
#undef CONST_ETXTBSY

/* The value of EUSERS.  */
#undef CONST_EUSERS

/* The value of EWOULDBLOCK.  */
#undef CONST_EWOULDBLOCK

/* The value of EXDEV.  */
#undef CONST_EXDEV

/* The value of O_BINARY.  */
#undef CONST_O_BINARY
