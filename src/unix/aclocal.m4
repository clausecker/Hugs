dnl ################################################################
dnl Macros
dnl (hard-core autoconf hackers only)
dnl ################################################################

dnl Like AC_SUBST but with a default value in case var is undefined
dnl typically usage from cshell:  env EXE=".exe" ./configure

dnl AC_SUBST_DEF(varname,defaultvalue)

AC_DEFUN(AC_SUBST_DEF,[
$1=${$1=$2}
AC_SUBST($1)
])

dnl Generalisation of AC_CHECK_TYPE which let's us specify which
dnl header files to search.

dnl AC_CHECK_TYPE_IN(TYPE, DEFAULT, HEADERS)

AC_DEFUN(AC_CHECK_TYPE_IN,
[dnl
AC_MSG_CHECKING(for $1)
AC_CACHE_VAL(ac_cv_type_$1,
[AC_EGREP_CPP($1, [$3], ac_cv_type_$1=yes, ac_cv_type_$1=no)])dnl
AC_MSG_RESULT($ac_cv_type_$1)
if test $ac_cv_type_$1 = no; then
  AC_DEFINE($1, $2)
fi
])


dnl Based on AC_TRY_LINK - run iftrue if links cleanly with no warning

dnl AC_TRY_LINK_NOWARN(flags,main?,iftrue,iffalse)

AC_DEFUN(AC_TRY_LINK_NOWARN,
[
ac_save_LIBS="$LIBS"
LIBS=[$1];
cat > conftest.$ac_ext <<EOF
dnl This sometimes fails to find confdefs.h, for some reason.
dnl [#]line __oline__ "[$]0"
[#]line __oline__ "configure"
#include "confdefs.h"
[$2]
int t() { return 0; }
EOF
if AC_TRY_EVAL(ac_link); then
  ifelse([$3], , :, [
    LIBS="$ac_save_LIBS"
    rm -rf conftest*
    $3])
  ifelse([$4], , , [else
    LIBS="$ac_save_LIBS"
    rm -rf conftest*
    $4
])dnl
fi
rm -f conftest*
]
)

dnl Loosely based on AC_CHECK_LIB in acgeneral.m4 in autoconf distribution

dnl AC_CHECK_FLAG_NOWARN(NAME, FLAG, CODE, iftrue, iffalse)

AC_DEFUN(AC_CHECK_FLAG_NOWARN,
[AC_MSG_CHECKING([for $1])
 AC_CACHE_VAL(ac_cv_flag_$1,
   [AC_TRY_LINK_NOWARN("$2", [main() { $3; exit(0); } ],
     eval "ac_cv_flag_$1=yes",
     eval "ac_cv_flag_$1=no"
   )]
 )
if eval "test \"`echo '$ac_cv_flag_'$1`\" = yes"; then
  AC_MSG_RESULT(yes)
  LIBS="$2 $LIBS"
  $4
else
  AC_MSG_RESULT(no)
  $5
fi
])

dnl AC_CHECK_LIB_NOWARN(LIBRARY, FUNCTION)

AC_DEFUN(AC_CHECK_LIB_NOWARN,
[AC_CHECK_FLAG_NOWARN([function_$2],[],[extern char $2(); $2();],
[changequote(, )dnl
  ac_tr_lib=HAVE_LIB`echo $1 | tr 'abcdefghijklmnopqrstuvwxyz' 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'`
 changequote([, ])dnl
 AC_DEFINE_UNQUOTED($ac_tr_lib)
],
[AC_CHECK_FLAG_NOWARN([library_$1],[-l$1],[extern char $2(); $2();],
[changequote(, )dnl
  ac_tr_lib=HAVE_LIB`echo $1 | tr 'abcdefghijklmnopqrstuvwxyz' 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'`
 changequote([, ])dnl
 AC_DEFINE_UNQUOTED($ac_tr_lib)
],
[]
)])]
)


dnl copied from acspecific.m4 (part of autoconf distribution)
dnl
dnl AC_DEFUN(AC_STACK_DIRECTION,
dnl [AC_CACHE_CHECK(stack direction for C alloca, ac_cv_c_stack_direction,
dnl [AC_TRY_RUN([find_stack_direction ()
dnl {
dnl   static char *addr = 0;
dnl   auto char dummy;
dnl   if (addr == 0)
dnl 	{
dnl 	  addr = &dummy;
dnl 	  return find_stack_direction ();
dnl 	}
dnl   else
dnl 	return (&dummy > addr) ? 1 : -1;
dnl }
dnl main ()
dnl {
dnl   exit (find_stack_direction() < 0);
dnl }], ac_cv_c_stack_direction=1, ac_cv_c_stack_direction=-1,
dnl   ac_cv_c_stack_direction=0)])
dnl AC_DEFINE_UNQUOTED(STACK_DIRECTION, $ac_cv_c_stack_direction)
dnl ])


dnl On some machines, you cannot take the address of a jmp_buf
dnl
AC_DEFUN(AC_C_JMPBUF_ARRAY,
[AC_CACHE_CHECK(for arrays of jmp_bufs, ac_cv_c_jmp_buf_array,
[AC_TRY_COMPILE([
#include <setjmp.h>
int test1() {
    jmp_buf jb[1];
    jmp_buf *jbp = jb;
    return (setjmp(jb[0]) == 0);
}
],
[int i;], 
ac_cv_c_jmp_buf_array=yes,
ac_cv_c_jmp_buf_array=no)])
if test "$ac_cv_c_jmp_buf_array" = yes; then
AC_DEFINE(JMPBUF_ARRAY)
fi
])


dnl POSIX systems prefer "diff -C 1"; SunOS4 prefers "diff -c1".
dnl
AC_DEFUN(AC_PROG_DIFF,
[AC_PATH_PROG(DIFF,diff)
AC_CACHE_CHECK(whether to use "diff -c1" or "diff -C 1", CONTEXT_DIFF,
if AC_TRY_COMMAND(diff -C 1 config.log config.log); then
  CONTEXT_DIFF="$DIFF -C 1"
else
  if AC_TRY_COMMAND(diff -c1 config.log config.log); then
    CONTEXT_DIFF="$DIFF -c1"
  else
    CONTEXT_DIFF="$DIFF"
  fi
fi
)
AC_SUBST(CONTEXT_DIFF)
])

dnl check for prototypes
dnl
AC_DEFUN(AC_C_PROTOTYPES,
[AC_CACHE_CHECK([prototypes], ac_cv_prototypes,
[AC_TRY_COMPILE([
void foo(int);
void foo(i)
int i; { 
return;
}
],
[int i;], 
ac_cv_prototypes=yes,
ac_cv_prototypes=no)])
if test "$ac_cv_prototypes" = yes; then
AC_DEFINE(HAVE_PROTOTYPES)
fi
])

dnl check for gcc's "labels as values" feature
AC_DEFUN(AC_C_LABELS_AS_VALUES,
[AC_CACHE_CHECK([labels as values], ac_cv_labels_as_values,
[AC_TRY_COMPILE([
int foo(int);
int foo(i)
int i; { 
static void *label[] = { &&l1, &&l2 };
goto *label[i];
l1: return 1;
l2: return 2;
}
],
[int i;], 
ac_cv_labels_as_values=yes,
ac_cv_labels_as_values=no)])
if test "$ac_cv_labels_as_values" = yes; then
AC_DEFINE(HAVE_LABELS_AS_VALUES)
fi
])

dnl ** check for leading underscores in symbol names
dnl 
dnl Test for determining whether symbol names have a leading
dnl underscore.
dnl 
dnl We assume that they _haven't_ if anything goes wrong.
dnl
dnl Some nlist implementations seem to try to be compatible by ignoring
dnl a leading underscore sometimes (eg. FreeBSD).  We therefore have
dnl to work around this by checking for *no* leading underscore first.
dnl Sigh.  --SDM
dnl
AC_DEFUN(FPTOOLS_UNDERSCORE,
[AC_CHECK_LIB(elf, nlist, LIBS="-lelf $LIBS")dnl
AC_CACHE_CHECK([leading underscore in symbol names], fptools_cv_lead_uscore,

dnl
dnl Hack!: nlist() under Digital UNIX insist on there being an _,
dnl but symbol table listings shows none. What is going on here?!?
dnl
dnl Another hack: cygwin doesn't come with nlist.h , so we hardwire
dnl the underscoredness of that "platform"
changequote(<<, >>)dnl
<<
case $HostPlatform in
alpha-dec-osf*) fptools_cv_lead_uscore='no';;
*cygwin32) fptools_cv_lead_uscore='yes';;
*mingw32) fptools_cv_lead_uscore='yes';;
*) >>
changequote([, ])dnl
AC_TRY_RUN([#ifdef HAVE_NLIST_H
#include <nlist.h>
changequote(<<, >>)dnl
<<
struct nlist xYzzY1[] = {{"xYzzY1", 0},{0}};
struct nlist xYzzY2[] = {{"_xYzzY2", 0},{0}};
#endif

main(argc, argv)
int argc;
char **argv;
{
#ifdef HAVE_NLIST_H
    if(nlist(argv[0], xYzzY1) == 0 && xYzzY1[0].n_value != 0)
        exit(1);
    if(nlist(argv[0], xYzzY2) == 0 && xYzzY2[0].n_value != 0)
        exit(0);>>
changequote([, ])dnl
#endif
    exit(1);
}], fptools_cv_lead_uscore=yes, fptools_cv_lead_uscore=no, fptools_cv_lead_uscore=NO)
;;
esac);
LeadingUnderscore=`echo $fptools_cv_lead_uscore | sed 'y/yesno/YESNO/'`
AC_SUBST(LeadingUnderscore)
case $LeadingUnderscore in
YES) AC_DEFINE(LEADING_UNDERSCORE);;
esac
])

dnl *** Is altzone available? ***
dnl 
dnl (copied from the fptools/ configure script)
AC_DEFUN(FPTOOLS_HAVE_ALTZONE,
[AC_CACHE_CHECK([altzone], fptools_cv_altzone,
[AC_TRY_LINK([#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif
], [return altzone;], 
fptools_cv_altzone=yes, fptools_cv_altzone=no)])
if test "$fptools_cv_altzone" = yes; then
  AC_DEFINE(HAVE_ALTZONE)
fi
])

dnl
dnl Is timezone around? (in a header file)
dnl 
dnl (copied from the fptools/ configure script)
AC_DEFUN(FPTOOLS_HAVE_TIMEZONE,
[AC_CACHE_CHECK([timezone], fptools_cv_have_timezone,
[AC_TRY_COMPILE([#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif
], [return timezone/1;], 
fptools_cv_have_timezone=yes, fptools_cv_have_timezone=no)])
if test "$fptools_cv_have_timezone" = yes; then
  AC_DEFINE(HAVE_TIMEZONE)
fi
])
