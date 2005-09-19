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
