# FP_COMPUTE_INT(EXPRESSION, VARIABLE, INCLUDES, IF-FAILS)
# --------------------------------------------------------
# Assign VARIABLE the value of the compile-time EXPRESSION using INCLUDES for
# compilation. Execute IF-FAILS when unable to determine the value. Works for
# cross-compilation, too.
#
# Implementation note: We are lazy and use an internal autoconf macro, but it
# is supported in autoconf versions 2.50 up to the actual 2.57, so there is
# little risk.
AC_DEFUN([FP_COMPUTE_INT],
[_AC_COMPUTE_INT([$1], [$2], [$3], [$4])[]dnl
])# FP_COMPUTE_INT


# FP_CHECK_ALIGNMENT(TYPE, [IGNORED], [INCLUDES = DEFAULT-INCLUDES])
# ------------------------------------------------------------------
# A variation of AC_CHECK_SIZEOF for computing the alignment restrictions of a
# given type. Defines ALIGNMENT_TYPE.
AC_DEFUN([FP_CHECK_ALIGNMENT],
[AS_LITERAL_IF([$1], [],
               [AC_FATAL([$0: requires literal arguments])])[]dnl
AC_CHECK_TYPE([$1], [], [], [$3])[]dnl
m4_pushdef([fp_Cache], [AS_TR_SH([fp_cv_alignment_$1])])[]dnl
AC_CACHE_CHECK([alignment of $1], [fp_Cache],
[if test "$AS_TR_SH([ac_cv_type_$1])" = yes; then
  FP_COMPUTE_INT([(long) (&((struct { char c; $1 ty; } *)0)->ty)],
                 [fp_Cache],
                 [AC_INCLUDES_DEFAULT([$3])],
                 [AC_MSG_ERROR([cannot compute alignment ($1)
See `config.log' for more details.], [77])])
else
  fp_Cache=0
fi])[]dnl
AC_DEFINE_UNQUOTED(AS_TR_CPP(alignment_$1), $fp_Cache, [The alignment of a `$1'.])[]dnl
m4_popdef([fp_Cache])[]dnl
])# FP_CHECK_ALIGNMENT


# FP_CHECK_CONST(EXPRESSION, [INCLUDES = DEFAULT-INCLUDES], [VALUE-IF-FAIL = -1])
# -------------------------------------------------------------------------------
# Defines CONST_EXPRESSION to the value of the compile-time EXPRESSION, using
# INCLUDES. If the value cannot be determined, use VALUE-IF-FAIL.
AC_DEFUN([FP_CHECK_CONST],
[AS_VAR_PUSHDEF([fp_Cache], [fp_cv_const_$1])[]dnl
AC_CACHE_CHECK([value of $1], fp_Cache,
[FP_COMPUTE_INT([$1], fp_check_const_result, [AC_INCLUDES_DEFAULT([$2])],
                [fp_check_const_result=m4_default([$3], ['-1'])])
AS_VAR_SET(fp_Cache, [$fp_check_const_result])])[]dnl
AC_DEFINE_UNQUOTED(AS_TR_CPP([CONST_$1]), AS_VAR_GET(fp_Cache), [The value of $1.])[]dnl
AS_VAR_POPDEF([fp_Cache])[]dnl
])# FP_CHECK_CONST


# FP_CHECK_CONSTS_TEMPLATE(EXPRESSION...)
# ---------------------------------------
# autoheader helper for FP_CHECK_CONSTS
m4_define([FP_CHECK_CONSTS_TEMPLATE],
[AC_FOREACH([fp_Const], [$1],
  [AH_TEMPLATE(AS_TR_CPP(CONST_[]fp_Const),
               [The value of ]fp_Const[.])])[]dnl
])# FP_CHECK_CONSTS_TEMPLATE


# FP_CHECK_CONSTS(EXPRESSION..., [INCLUDES = DEFAULT-INCLUDES], [VALUE-IF-FAIL = -1])
# -----------------------------------------------------------------------------------
# List version of FP_CHECK_CONST
AC_DEFUN(FP_CHECK_CONSTS,
[FP_CHECK_CONSTS_TEMPLATE([$1])dnl
for fp_const_name in $1
do
FP_CHECK_CONST([$fp_const_name], [$2], [$3])
done
])# FP_CHECK_CONSTS


# FP_CHECK_PROG(VARIABLE, PROG-TO-CHECK-FOR,
#               [VALUE-IF-NOT-FOUND], [PATH], [REJECT])
# -----------------------------------------------------
# HACK: A small wrapper around AC_CHECK_PROG, setting VARIABLE to the full path
# of PROG-TO-CHECK-FOR when found.
AC_DEFUN([FP_CHECK_PROG],
[AC_CHECK_PROG([$1], [$2], [$as_dir/$ac_word$ac_exec_ext], [$3], [$4], [$5])][]dnl
)# FP_CHECK_PROC


# FP_PROG_FIND
# ------------
# Find a non-WinDoze version of the "find" utility.
AC_DEFUN([FP_PROG_FIND],
[AC_PATH_PROG([fp_prog_find], [find])
echo foo > conftest.txt
$fp_prog_find conftest.txt -print > conftest.out 2>&1
if grep '^conftest.txt$' conftest.out > /dev/null 2>&1 ; then
  # OK, looks like a real "find".
  FindCmd="$fp_prog_find"
else
  # Found a poor WinDoze version of "find", ignore it.
  AC_MSG_WARN([$fp_prog_find looks like a non-*nix find, ignoring it])
  FP_CHECK_PROG([FindCmd], [find], [], [], [$fp_prog_find])
fi
rm -f conftest.txt conftest.out
AC_SUBST([FindCmd])[]dnl
])# FP_PROG_FIND


# FP_PROG_SORT
# ------------
# Find a non-WinDoze version of the "sort" utility.
AC_DEFUN([FP_PROG_SORT],
[AC_PATH_PROG([fp_prog_sort], [sort])
echo foo > conftest.txt
$fp_prog_sort -u conftest.txt > conftest.out 2>&1
if grep '^foo$' conftest.out > /dev/null 2>&1 ; then
  # OK, looks like a real "sort".
  SortCmd="$fp_prog_sort"
else
  # Found a poor WinDoze version of "sort", ignore it.
  AC_MSG_WARN([$fp_prog_sort looks like a non-*nix sort, ignoring it])
  FP_CHECK_PROG([SortCmd], [sort], [], [], [$fp_prog_sort])
fi
rm -f conftest.txt conftest.out
AC_SUBST([SortCmd])[]dnl
])# FP_PROG_SORT


dnl ** Map an arithmetic C type to a Haskell type.
dnl    Based on autconf's AC_CHECK_SIZEOF.

dnl FPTOOLS_CHECK_HTYPE(TYPE [, DEFAULT_VALUE, [, VALUE-FOR-CROSS-COMPILATION])
AC_DEFUN(FPTOOLS_CHECK_HTYPE,
[changequote(<<, >>)dnl
dnl The name to #define.
define(<<AC_TYPE_NAME>>, translit(htype_$1, [a-z *], [A-Z_P]))dnl
dnl The cache variable name.
define(<<AC_CV_NAME>>, translit(fptools_cv_htype_$1, [ *], [_p]))dnl
define(<<AC_CV_NAME_supported>>, translit(fptools_cv_htype_sup_$1, [ *], [_p]))dnl
changequote([, ])dnl
AC_MSG_CHECKING(Haskell type for $1)
AC_CACHE_VAL(AC_CV_NAME,
[AC_CV_NAME_supported=yes
fp_check_htype_save_cppflags="$CPPFLAGS"
CPPFLAGS="$CPPFLAGS $X_CFLAGS"
AC_RUN_IFELSE([AC_LANG_SOURCE([[#include <stdio.h>
#include <stddef.h>

#if HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif

#if HAVE_UNISTD_H
# include <unistd.h>
#endif

#if HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif

#if HAVE_FCNTL_H
# include <fcntl.h>
#endif

#if HAVE_SIGNAL_H
# include <signal.h>
#endif

#if HAVE_TIME_H
# include <time.h>
#endif

#if HAVE_TERMIOS_H
# include <termios.h>
#endif

#if HAVE_STRING_H
# include <string.h>
#endif

#if HAVE_CTYPE_H
# include <ctype.h>
#endif

#if defined(HAVE_GL_GL_H)
# include <GL/gl.h>
#elif defined(HAVE_OPENGL_GL_H)
# include <OpenGL/gl.h>
#endif

#if defined(HAVE_AL_ALC_H)
# include <AL/alc.h>
#elif defined(HAVE_OPENAL_ALC_H)
# include <OpenAL/alc.h>
#endif

#if HAVE_SYS_RESOURCE_H
# include <sys/resource.h>
#endif

typedef $1 testing;

main() {
  FILE *f=fopen("conftestval", "w");
  if (!f) exit(1);
  if (((testing)((int)((testing)1.4))) == ((testing)1.4)) {
    fprintf(f, "%s%d\n",
           ((testing)(-1) < (testing)0) ? "Int" : "Word",
           sizeof(testing)*8);
  } else {
    fprintf(f,"%s\n",
           (sizeof(testing) >  sizeof(double)) ? "LDouble" :
           (sizeof(testing) == sizeof(double)) ? "Double"  : "Float");
  }
  fclose(f);
  exit(0);
}]])],[AC_CV_NAME=`cat conftestval`],
[ifelse([$2], , [AC_CV_NAME=NotReallyAType; AC_CV_NAME_supported=no], [AC_CV_NAME=$2])],
[ifelse([$3], , [AC_CV_NAME=NotReallyATypeCross; AC_CV_NAME_supported=no], [AC_CV_NAME=$3])])]) dnl
CPPFLAGS="$fp_check_htype_save_cppflags"
if test "$AC_CV_NAME_supported" = yes; then
  AC_MSG_RESULT($AC_CV_NAME)
  AC_DEFINE_UNQUOTED(AC_TYPE_NAME, $AC_CV_NAME, [Define to Haskell type for $1])
else
  AC_MSG_RESULT([not supported])
fi
undefine([AC_TYPE_NAME])dnl
undefine([AC_CV_NAME])dnl
undefine([AC_CV_NAME_supported])dnl
])


# FP_CHECK_WIN32
# --------------
# If Windows is the target platform (e.g. MinGW/MSYS or Cygwin with
# -mno-cygwin), the variable "is_win32" is set to "yes", otherwise (e.g. *nix
# systems or plain Cygwin) it is set to "no".
AC_DEFUN([FP_CHECK_WIN32],
[AC_CACHE_CHECK([for Windows environment], [fp_cv_is_win32],
  [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([], [
#if !_WIN32
   syntax error;
#endif
])], [fp_cv_is_win32=yes], [fp_cv_is_win32=no])])
is_win32="$fp_cv_is_win32"[]dnl
])# FP_CHECK_WIN32


# FP_PATH_X
# ---------
# Same as AC_PATH_X, but works even for broken Cygwins which try to include the
# non-existant <gl/mesa_wgl.h> header when -mno-cygwin is used.
AC_DEFUN([FP_PATH_X],
[AC_REQUIRE([FP_CHECK_WIN32])
if test x"$is_win32" = xyes; then
  no_x=yes
else
  AC_PATH_X
fi
])# FP_PATH_X


# FP_PATH_XTRA
# ------------
# Same as AC_PATH_XTRA, but works even for broken Cygwins which try to include
# the non-existant <gl/mesa_wgl.h> header when -mno-cygwin is used.
AC_DEFUN([FP_PATH_XTRA],
[AC_REQUIRE([FP_CHECK_WIN32])
if test x"$is_win32" = xyes; then
  no_x=yes
else
  AC_PATH_XTRA
fi
])# FP_PATH_XTRA


# FP_CHECK_GL_HELPER(LIBNAME, LIBS, INCLUDES, FUNCTION-BODY)
# ----------------------------------------------------------
# Try each library in LIBS to successfully link INCLUDES plus FUNCTION-BODY,
# setting LIBNAME_CFLAGS and LIBNAME_LIBS to the corresponding values. Sets
# no_LIBNAME to "yes" if no suitable library was found. (LIBNAME_CFLAGS0
# contains the value of LIBNAME_CFLAGS without CPPFLAGS, and LIBNAME_LIBS0
# contains the value of LIBNAME_LIBS without LDFLAGS, but these are only
# used internally.)
AC_DEFUN([FP_CHECK_GL_HELPER],
[AC_CACHE_CHECK([for $1 library], [fp_cv_check_$1_lib],
  [fp_cv_check_$1_lib="no"
  fp_save_CPPFLAGS="$CPPFLAGS"
  CPPFLAGS="$CPPFLAGS ${$1_CFLAGS}"
  fp_save_LIBS="$LIBS"
  for fp_try_lib in $2; do
    # transform "-lfoo" to "foo.lib" when using cl
    if test x"$CC" = xcl; then
      fp_try_lib=`echo $fp_try_lib | sed -e 's/^-l//' -e 's/$/.lib/'`
    fi
    LIBS="$fp_try_lib ${$1_LIBS} $fp_save_LIBS"
    AC_LINK_IFELSE([AC_LANG_PROGRAM([$3], [$4])], [fp_cv_check_$1_lib="$fp_try_lib ${$1_LIBS}"; break])
  done
  LIBS="$fp_save_LIBS"
  CPPFLAGS="$fp_save_CPPFLAGS"])

  if test x"$fp_cv_check_$1_lib" = xno; then
    no_$1=yes
    $1_CFLAGS=
    $1_LIBS=
  else
    $1_CFLAGS0="${$1_CFLAGS}"
    $1_CFLAGS="$CPPFLAGS ${$1_CFLAGS0}"
    $1_LIBS0="$fp_cv_check_$1_lib"
    $1_LIBS="$LDFLAGS ${$1_LIBS0}"
  fi
])# FP_CHECK_GL_HELPER


# FP_CHECK_GL
# -----------
AC_DEFUN([FP_CHECK_GL],
[AC_REQUIRE([FP_PATH_X])
AC_REQUIRE([AC_CANONICAL_TARGET])

AC_ARG_ENABLE([hopengl],
  [AC_HELP_STRING([--enable-hopengl],
    [build a Haskell binding for OpenGL (GL/GLU).  To use X11 instead
     of the "native" libraries on Mac OS X, use --enable-hopengl=x11])],
  [enable_opengl=$enableval], [enable_opengl=no])

if test x"$enable_opengl" = xno; then
   no_GL=yes
else
  use_quartz_opengl=no
  case $target_os in
  darwin*)
    if test x"$enable_opengl" != xx11; then
      AC_DEFINE([USE_QUARTZ_OPENGL], [1],
                [Define to 1 if native OpenGL should be used on Mac OS X])
      use_quartz_opengl=yes
    fi
    ;;
  esac

  if test x"$use_quartz_opengl" != xyes; then
    AC_CHECK_LIB([m], [atan], [GL_LIBS="-lm $GL_LIBS"])

    if test x"$no_x" != xyes; then
      test -n "$x_includes" && GL_CFLAGS="-I$x_includes $GL_CFLAGS"
      test -n "$x_libraries" && GL_LIBS="-L$x_libraries -lX11 $GL_LIBS"
    fi

    FP_CHECK_GL_HELPER([GL], [-lGL -lopengl32], [@%:@include <GL/gl.h>], [glEnd()])

    if test x"$no_GL" != xyes; then
      # Ugly: To get wglGetProcAddress on Windows, we have to link with
      # opengl32.dll, too, even when we are using Cygwin with X11.
      case "$GL_LIBS" in
        *-lopengl32*|*opengl32.lib*) ;;
        *) fp_save_LIBS="$LIBS"
           LIBS="$LIBS -lopengl32"
           AC_LINK_IFELSE([AC_LANG_PROGRAM([[@%:@include <GL/gl.h>]], [[glEnd()]])],
             [GL_LIBS="$GL_LIBS -lopengl32"; GL_LIBS0="$GL_LIBS0 -lopengl32"])
           LIBS="$fp_save_LIBS"
           ;;
      esac
    fi
  fi
fi

AC_SUBST([GL_CFLAGS])
AC_SUBST([GL_LIBS])
])# FP_CHECK_GL


# FP_CHECK_GLU
# ------------
AC_DEFUN([FP_CHECK_GLU],
[AC_REQUIRE([FP_CHECK_GL])dnl
GLU_CFLAGS="$GL_CFLAGS0"
GLU_LIBS="$GL_LIBS0"

if test x"$enable_opengl" = xno; then
   no_GLU=yes
elif test x"$use_quartz_opengl" != xyes; then
  FP_CHECK_GL_HELPER([GLU], [-lglu32 -lGLU], [@%:@include <GL/glu.h>], [gluNewQuadric()])
fi

AC_SUBST([GLU_CFLAGS])
AC_SUBST([GLU_LIBS])
])# FP_CHECK_GLU


# FP_CHECK_GLUT
# -------------
AC_DEFUN([FP_CHECK_GLUT],
[AC_REQUIRE([FP_CHECK_GLU])
FP_PATH_XTRA

if test x"$enable_opengl" = xno; then
   no_GLUT=yes
elif test x"$use_quartz_opengl" != xyes; then
  GLUT_CFLAGS="$GLU_CFLAGS0"
  GLUT_LIBS="$GLU_LIBS0"

  if test x"$no_x" != xyes; then
    GLUT_LIBS="$X_PRE_LIBS -lXmu -lXi $X_EXTRA_LIBS $GLUT_LIBS"
  fi

  AC_CHECK_HEADERS([windows.h GL/glut.h])
  # Note 1: On Cygwin with X11, GL/GLU functions use the "normal" calling
  # convention, but GLUT functions use stdcall. To get this right, it is
  # necessary to include <windows.h> first.
  # Note 2: MinGW/MSYS comes without a GLUT header, so we use Cygwin's one in
  # that case.
  FP_CHECK_GL_HELPER([GLUT], [-lglut32 -lglut], [
#if HAVE_WINDOWS_H
#include <windows.h>
#endif
#if HAVE_GL_GLUT_H
#include <GL/glut.h>
#else
#include "glut_local.h"
#endif
    ], [glutMainLoop()])
fi

AC_SUBST([GLUT_CFLAGS])
AC_SUBST([GLUT_LIBS])
])# FP_CHECK_GLUT

dnl AC_C_INLINE_ONLY
dnl ----------------
dnl Define the preprocessor symbol INLINE_ONLY to the specifier(s)
dnl that ensure that functions are inlined but not externally defined,
dnl so that a file using them can be linked with another containing their
dnl external definitions.  (This is `inline' for C99 and `inline extern'
dnl for gcc, with `static inline' as a fallback that should be OK even if
dnl `inline' is defined to be empty by AC_C_INLINE.)
dnl
AC_DEFUN([AC_C_INLINE_ONLY],
[AC_REQUIRE([AC_PROG_CC])
 AC_REQUIRE([AC_C_INLINE])
 AC_CACHE_CHECK([syntax for pure inlines], [ac_cv_c_inline_only],
[
  cat >conftest.h <<EOF
INLINE int foo(int n) { return n+1; }
EOF

  cat >conftest_fn.c <<EOF
#define INLINE
#include "conftest.h"
EOF

  cat >conftest.c <<EOF
#define INLINE INLINE_ONLY
#include "conftest.h"
int main() { return foo(2); }
EOF

  ac_cv_c_inline_only=''
  for inline_only in "$ac_cv_c_inline" "extern $ac_cv_c_inline" "static $ac_cv_c_inline"; do
    if $CC $CFLAGS -DINLINE_ONLY="$inline_only" conftest.c conftest_fn.c >&5 2>&5; then
      ac_cv_c_inline_only="$inline_only"
      break
    fi
  done
])
if test "$ac_cv_c_inline_only"; then
  AC_DEFINE_UNQUOTED([INLINE_ONLY], $ac_cv_c_inline_only,
    [Specifier(s) for functions that should be inlined,
     but not generate an external definition.
     This will be `inline' for C99 and `extern inline' for gcc.])
fi
])

dnl External macros

builtin([include],../ac_macros/ice_prog_cpp_traditional.m4)
