dnl--------------------------------------------------------------------
dnl
dnl Hugs98 libraries configure script template.
dnl Process ../configure.ac with autoreconf to produce a configure script.
dnl
dnl--------------------------------------------------------------------

AC_INIT([Hugs98 libraries], [1.0], [hugs-bugs@haskell.org], [hugslibs])
AC_CONFIG_SRCDIR([include/MachDeps.h])

dnl We need 2.50 for AC_HELP_STRING.
AC_PREREQ([2.50])

dnl Named to match the fptools configuration header file
AC_CONFIG_HEADER(include/ghcconfig.h)
AH_TOP([/* platform-specific defines */
#include "platform.h"])

# necessary evil
AC_CONFIG_COMMANDS([ultra-evil], [echo '
#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION' >>include/ghcconfig.h])

dnl --------------------------------------------------
dnl ### program checking ###
dnl --------------------------------------------------

AC_PROG_CC
AC_PROG_GCC_TRADITIONAL

dnl ** figure out how to invoke cpp directly (gcc -E is no good)
AC_PROG_CPP
ICE_PROG_CPP_TRADITIONAL

AC_PROG_MAKE_SET

dnl Look for right versions of 'find' and 'sort' (win32 only, really.)
FP_PROG_FIND
FP_PROG_SORT

dnl--------------------------------------------------------------------
dnl ### compiler characteristics ###
dnl--------------------------------------------------------------------

AC_C_CONST               dnl can we use "const"?
AC_C_INLINE              dnl can we use "inline"?
AC_C_INLINE_ONLY         dnl should we use "inline" or "inline extern"?

dnl --------------------------------------------------
dnl * Platform header file and syscall feature tests
dnl ### checking the state of the local header files and syscalls ###

dnl ** check for full ANSI header (.h) files
AC_HEADER_STDC

dnl checks for sys/wait.h
AC_HEADER_SYS_WAIT

dnl ** check for specific header (.h) files that we are interested in
AC_CHECK_HEADERS(
  ctype.h dirent.h errno.h fcntl.h grp.h limits.h pwd.h signal.h \
  stdlib.h string.h \
  sys/resource.h sys/stat.h sys/timers.h sys/types.h sys/utsname.h \
  termios.h unistd.h utime.h vfork.h
  )

dnl Windows include files
AC_CHECK_HEADERS(windows.h winsock.h)

AC_HEADER_TIME
AC_CHECK_HEADERS(time.h sys/time.h sys/timeb.h sys/times.h)

dnl ** check for OpenGL/GLUT include paths and libraries
FP_CHECK_GLUT

dnl ** check for OpenGL include files
if test x"$enable_opengl" != x"no"; then
  fp_save_cppflags="$CPPFLAGS"
  CPPFLAGS="$CPPFLAGS $X_CFLAGS"
  if test x"$use_quartz_opengl" = xyes ; then
    AC_CHECK_HEADERS(OpenGL/gl.h)
  else
    AC_CHECK_HEADERS(GL/gl.h)
  fi
  CPPFLAGS="$fp_save_cppflags"
fi

LibsWithOpenGL=NO
LibsWithGLUT=NO
if test "$enable_opengl" != "no"; then
  if test "$no_GLU" != yes; then
    LibsWithOpenGL=YES
    if test "$no_GLUT" != yes; then
      LibsWithGLUT=YES
    fi
  fi
fi
AC_SUBST([LibsWithOpenGL])
AC_SUBST([LibsWithGLUT])

dnl The OpenGL test calls FP_PATH_XTRA, which checks for X11
if test "$no_x" = yes; then
  LibsWithX11=NO
else
  LibsWithX11=YES
fi
AC_SUBST([LibsWithX11])

dnl ** check for POSIX regex
HavePosixRegex=NO
AC_CHECK_HEADERS([regex.h], [AC_CHECK_FUNCS(regcomp, [HavePosixRegex=YES])])
AC_SUBST(HavePosixRegex)

dnl--------------------------------------------------------------------
dnl ### types 
dnl--------------------------------------------------------------------

dnl ** do we have long longs?
AC_CHECK_TYPES([long long])

dnl ** what are the sizes of various types
AC_CHECK_SIZEOF(char,               1)
AC_CHECK_SIZEOF(double,             8)
AC_CHECK_SIZEOF(float,              4)
AC_CHECK_SIZEOF(int,                4)
AC_CHECK_SIZEOF(long,               4)
if test "$ac_cv_type_long_long" = yes; then
AC_CHECK_SIZEOF(long long,          8)
fi
AC_CHECK_SIZEOF(short,              2)
AC_CHECK_SIZEOF(unsigned char,      1)
AC_CHECK_SIZEOF(unsigned int,       4)
AC_CHECK_SIZEOF(unsigned long,      4)
if test "$ac_cv_type_long_long" = yes; then
AC_CHECK_SIZEOF(unsigned long long, 8)
fi
AC_CHECK_SIZEOF(unsigned short,     2)
AC_CHECK_SIZEOF(void *,             4)

dnl ** what are alignment constraints on various types
FP_CHECK_ALIGNMENT(char)
FP_CHECK_ALIGNMENT(double)
FP_CHECK_ALIGNMENT(float)
FP_CHECK_ALIGNMENT(int)
FP_CHECK_ALIGNMENT(long)
if test "$ac_cv_type_long_long" = yes; then
FP_CHECK_ALIGNMENT(long long)
fi
FP_CHECK_ALIGNMENT(short)
FP_CHECK_ALIGNMENT(unsigned char)
FP_CHECK_ALIGNMENT(unsigned int)
FP_CHECK_ALIGNMENT(unsigned long)
if test "$ac_cv_type_long_long" = yes; then
FP_CHECK_ALIGNMENT(unsigned long long)
fi
FP_CHECK_ALIGNMENT(unsigned short)
FP_CHECK_ALIGNMENT(void *)

dnl ** Map OpenGL data types to Haskell types
if test x"$enable_opengl" != x"no"; then
FPTOOLS_CHECK_HTYPE(GLboolean)
FPTOOLS_CHECK_HTYPE(GLbyte)
FPTOOLS_CHECK_HTYPE(GLubyte)
FPTOOLS_CHECK_HTYPE(GLshort)
FPTOOLS_CHECK_HTYPE(GLushort)
FPTOOLS_CHECK_HTYPE(GLint)
FPTOOLS_CHECK_HTYPE(GLuint)
FPTOOLS_CHECK_HTYPE(GLsizei)
FPTOOLS_CHECK_HTYPE(GLenum)
FPTOOLS_CHECK_HTYPE(GLbitfield)
FPTOOLS_CHECK_HTYPE(GLfloat)
FPTOOLS_CHECK_HTYPE(GLclampf)
FPTOOLS_CHECK_HTYPE(GLdouble)
FPTOOLS_CHECK_HTYPE(GLclampd)
fi

dnl ** can we open files in binary mode?
FP_CHECK_CONST([O_BINARY], [#include <fcntl.h>], [0])

dnl ** check for more functions
AC_CHECK_FUNCS(ftime)
AC_CHECK_FUNCS(getclock)
AC_CHECK_FUNCS(getrusage)
AC_CHECK_FUNCS(gettimeofday)
AC_CHECK_FUNCS(gmtime)
AC_CHECK_FUNCS(localtime)
AC_CHECK_FUNCS(lstat)
AC_CHECK_FUNCS(readdir_r)
AC_CHECK_FUNCS(setenv)
AC_CHECK_FUNCS(time)
AC_CHECK_FUNCS(times)
AC_CHECK_FUNCS(unsetenv)

AC_CHECK_LIB([dl], [dlopen])

dnl Definitions computed by ../configure, for insertion into Makefile
MkDefs=../MkDefs
AC_SUBST_FILE(MkDefs)  

AC_CONFIG_FILES(Makefile tools/config tools/hugs-package)

AC_OUTPUT
