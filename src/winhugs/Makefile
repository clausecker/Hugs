# Generated automatically from Makefile.in by configure.
# --------------------------------------------------------------------------
# Makefile for Hugs
#
# Hugs 98 is Copyright (c) Mark P Jones, Alastair Reid and the Yale
# Haskell Group 1994-99, and is distributed as Open Source software
# under the Artistic License; see the file "Artistic" that is included
# in the distribution for details.
# --------------------------------------------------------------------------

# Targets:
#
# <default>:   	 make winhugs.exe
# install:	 make and install programs/libraries
# winhugs.exe: 	 make minimal working system
# runhugs.exe:	 make batch-mode version of Hugs
# clean:       	 delete files not required in running system
# distclean:     delete files that can be regenerated using C compiler
# veryclean:     delete all machine generated files 
#                  (you need perl, bison/yacc, etc to rebuild these files)
# check:	 run regression tests
#                  (you need perl and /bin/sh to run these)
# TAGS:          build emacs TAGS table

# This rule goes first to make it the default choice
default		:: winhugs.exe 

CC		= cl /nologo
VERSTR          = -DMONTH_YEAR="\"Nov 2003\""
CFLAGS		= -MD -I. $(VERSTR)
#Debugging options:
#CFLAGS		= -Zi -MDd -I. $(VERSTR)
OPTFLAGS	= -O2
RC		= rc

LD		= ld
LDFLAGS    	=   
DLL_FLAGS	= /LD
PIC_FLAGS	= 
LIBS		= kernel32.lib user32.lib gdi32.lib comdlg32.lib shell32.lib advapi32.lib

YACC		= bison
RM		= -del
CP		= copy
OBJ		= .obj


.SUFFIXES	:
.SUFFIXES	: .c .h .obj
HFILES		= prelude.h config.h options.h storage.h connect.h \
		  errors.h command.h server.h HsFFI.h module.h machdep.h \
		  script.h opts.h strutil.h evaluator.h goal.h observe.h
CFILES		= hugs.c storage.c input.c static.c type.c subst.c \
		  output.c compiler.c machine.c interp.c builtin.c \
		  server.c ffi.c module.c machdep.c script.c opts.c \
		  strutil.c errors.c evaluator.c goal.c observe.c \
		  Winhugs.c

INCFILES	= parser.c preds.c bignums.c scc.c timer.c \
		  printer.c iomonad.c interns.c array.c
YFILES		= parser.y
SOURCES		= $(HFILES) $(CFILES) $(INCFILES) $(YFILES) 
OBJECTS		= storage.obj input.obj static.obj type.obj subst.obj \
                  compiler.obj plugin.obj builtin.obj machine.obj output.obj \

WOBJECTS        = winhugs.obj winframe.obj wintext.obj  winhint.obj \
                  winstln.obj wintoolb.obj winutils.obj 

OBJECTS		= storage.obj input.obj static.obj type.obj subst.obj \
		  compiler.obj plugin.obj builtin.obj machine.obj output.obj \
		  ffi.obj version.obj module.obj machdep.obj script.obj \
		  opts.obj strutil.obj errors.obj evaluator.obj goal.obj \
		  $(WOBJECTS)

IOBJECTS	= hugs.obj edit.obj observe.obj $(OBJECTS)
PRELUDE		= config.h options.h prelude.h script.h "winhugs\wintext.h"

################################################################
# Hugs interpreter and standalone evaluator
################################################################

winhugs.exe	: $(IOBJECTS)  winhugs.res
		  $(CC) $(LDFLAGS) $(IOBJECTS) winhugs.res  $(LIBS) -o winhugs.exe -link /subsystem:windows 


SERVER_OBJECTS	= server.obj $(OBJECTS) 
runhugs.exe	: runhugs.obj $(SERVER_OBJECTS)
		  $(CC) $(LDFLAGS) runhugs.obj $(SERVER_OBJECTS) $(LIBS) -o runhugs.exe

#
# Create all the FFI extension DLLs.
#
FFIHUGS=..\ffihugs
FFIHUGS_OPTS=+G -98 -P

.PHONY: ffi-dlls

ffi-dlls:
	$(FFIHUGS) $(FFIHUGS_OPTS) +L../libraries/Hugs/Storable_aux.c Hugs.Storable
	$(FFIHUGS) $(FFIHUGS_OPTS) Foreign.Marshal.Alloc
	$(FFIHUGS) $(FFIHUGS_OPTS) Foreign.Marshal.Utils
	$(FFIHUGS) $(FFIHUGS_OPTS) +L../libraries/Foreign/C/errno.c Foreign.C.Error
	$(FFIHUGS) $(FFIHUGS_OPTS) +L../libraries/Network/initWinSock.c +L../libraries/Network/winSockErr.c +Lwsock32.lib Network.Socket
	$(FFIHUGS) $(FFIHUGS_OPTS) +Lwsock32.lib Network.BSD

################################################################
# Clean, distclean, veryclean, TAGS
################################################################

clean		::
		  $(RM) *.o
		  $(RM) *.O
		  $(RM) *.obj
		  $(RM) *.OBJ
		  $(RM) *.LIB
		  $(RM) *.DEF
		  $(RM) *.RES
		  $(RM) *.EXP
		  $(RM) *.ILK
		  $(RM) *.PDB
		  $(RM) *.TD2
		  $(RM) *.MAP
		  $(RM) *.CSM
		  $(RM) *.TR2
		  $(RM) *.DSW
		  $(RM) *.aux
		  $(RM) *.hp
distclean	:: clean
distclean	::
		  $(RM) winhugs.exe
		  $(RM) runhugs.exe
		  $(RM) *.pdf
		  $(RM) TAGS
		  $(RM) *~
veryclean	:: distclean
veryclean	::
		  $(RM) options.h
		  $(RM) config.h
TAGS		::
		  etags *.[ych]

################################################################
# C and Yacc rules
################################################################

.c.obj	:
		  $(CC) -c $(CFLAGS) $(OPTFLAGS) $<

# These are compiled with less optimisation to avoid optimisation bugs in
# certain compilers.  This may be overly conservative on some compilers.
compiler.obj	: compiler.c
		  $(CC) -c $(CFLAGS) compiler.c
static.obj	: static.c
		  $(CC) -c $(CFLAGS) static.c

# parser.c	: parser.y
# 		  -$(YACC) parser.y
# 		  mv y.tab.c parser.c
# veryclean	::
# 		  $(RM) parser.c

#dependencies for these files are incomplete

wintext.obj	: $(PRELUDE) "winhugs\wintext.c"
		  $(CC) -c $(CFLAGS) $(OPTFLAGS)  "winhugs\wintext.c" 

winframe.obj    : $(PRELUDE) "winhugs\winframe.c"
	          $(CC) -c $(CFLAGS) $(OPTFLAGS) "winhugs\winframe.c" 

winhint.obj	: $(PRELUDE) "winhugs\winhint.c"
		  $(CC) -c $(CFLAGS) $(OPTFLAGS) "winhugs\winhint.c" 

winstln.obj	: $(PRELUDE) "winhugs\winstln.c"
	          $(CC) -c $(CFLAGS) $(OPTFLAGS) "winhugs\winstln.c" 

wintoolb.obj    : $(PRELUDE) "winhugs\wintoolb.c"
	          $(CC) -c $(CFLAGS) $(OPTFLAGS) "winhugs\wintoolb.c" 

winutils.obj    : $(PRELUDE) "winhugs\winutils.c"
	          $(CC) -c $(CFLAGS) $(OPTFLAGS) "winhugs\winutils.c" 

winhugs.obj    : $(PRELUDE) "winhugs\winhugs.c"
	          $(CC) -c $(CFLAGS) $(OPTFLAGS) "winhugs\winhugs.c" 

winhugs.res     : "winhugs\winhugs.rc"
                  $(RC) -fowinhugs.res "winhugs\winhugs.rc"

################################################################
# Object dependencies (Don't change or delete this line)
################################################################

hugs$(OBJ): hugs.c prelude.h config.h options.h storage.h HsFFI.h command.h \
  connect.h errors.h script.h opts.h strutil.h evaluator.h machdep.h \
  output.h module.h
runhugs$(OBJ): runhugs.c prelude.h config.h options.h storage.h HsFFI.h \
  connect.h machdep.h observe.h builtin.h evaluator.h errors.h server.h \
  HugsAPI.h
server$(OBJ): server.c prelude.h config.h options.h storage.h HsFFI.h \
  connect.h script.h machdep.h evaluator.h opts.h strutil.h errors.h \
  server.h HugsAPI.h
edit$(OBJ): edit.c prelude.h config.h options.h storage.h HsFFI.h connect.h \
  errors.h opts.h strutil.h machdep.h
observe$(OBJ): observe.c prelude.h config.h options.h storage.h HsFFI.h \
  connect.h command.h errors.h machdep.h builtin.h output.h observe.h
builtin$(OBJ): builtin.c prelude.h config.h options.h storage.h HsFFI.h \
  connect.h errors.h machdep.h builtin.h bignums.c printer.c iomonad.c \
  timeprim.c dirprim.c array.c
compiler$(OBJ): compiler.c prelude.h config.h options.h storage.h HsFFI.h \
  connect.h errors.h goal.h output.h opts.h
errors$(OBJ): errors.c prelude.h config.h options.h storage.h HsFFI.h \
  connect.h errors.h output.h opts.h goal.h evaluator.h
evaluator$(OBJ): evaluator.c prelude.h config.h options.h storage.h HsFFI.h \
  connect.h errors.h script.h output.h strutil.h opts.h machdep.h \
  evaluator.h
ffi$(OBJ): ffi.c prelude.h config.h options.h storage.h HsFFI.h connect.h \
  errors.h output.h strutil.h
goal$(OBJ): goal.c prelude.h config.h options.h storage.h HsFFI.h connect.h \
  errors.h machdep.h opts.h goal.h
input$(OBJ): input.c prelude.h config.h options.h storage.h HsFFI.h connect.h \
  command.h errors.h module.h script.h opts.h goal.h machdep.h parser.c
machdep$(OBJ): machdep.c prelude.h config.h options.h storage.h HsFFI.h \
  connect.h errors.h opts.h strutil.h machdep.h evaluator.h
machine$(OBJ): machine.c prelude.h config.h options.h storage.h HsFFI.h \
  connect.h errors.h opts.h
module$(OBJ): module.c prelude.h config.h options.h storage.h HsFFI.h \
  connect.h errors.h module.h output.h
opts$(OBJ): opts.c prelude.h config.h options.h storage.h HsFFI.h connect.h \
  errors.h machdep.h strutil.h opts.h
output$(OBJ): output.c prelude.h config.h options.h storage.h HsFFI.h \
  connect.h errors.h output.h
plugin$(OBJ): plugin.c prelude.h config.h options.h storage.h HsFFI.h \
  connect.h
script$(OBJ): script.c prelude.h config.h options.h storage.h HsFFI.h \
  connect.h errors.h machdep.h opts.h strutil.h script.h
static$(OBJ): static.c prelude.h config.h options.h storage.h HsFFI.h \
  connect.h machdep.h errors.h output.h subst.h module.h opts.h goal.h \
  scc.c
storage$(OBJ): storage.c prelude.h config.h options.h storage.h HsFFI.h \
  connect.h opts.h errors.h machdep.h evaluator.h strutil.h output.h
strutil$(OBJ): strutil.c prelude.h config.h options.h storage.h HsFFI.h \
  connect.h errors.h strutil.h
subst$(OBJ): subst.c prelude.h config.h options.h storage.h HsFFI.h connect.h \
  errors.h output.h opts.h subst.h
type$(OBJ): type.c prelude.h config.h options.h storage.h HsFFI.h connect.h \
  errors.h output.h subst.h goal.h opts.h preds.c scc.c
version$(OBJ): version.c prelude.h config.h options.h

################################################################
# End of object dependencies (Don't change or delete this line)
################################################################

################################################################
# Regression tests (Unix only).  Run "make install" first
# 
# Uses runstdtest (from ghc-0.26/ghc/glafp-utils/scripts), perl 5
# and /bin/sh (Bourne shell).
#
# "make check" generates a lot of output to explain what is going on
# and reassure you that progress is being made.  This is great if you've
# never run these tests before - but if you just want to reassure yourself
# that nothing has broken since the last release, you might prefer to
# run this command which removes all the explanations and success
# stories - leaving just the useful output.
#
#  make check | grep -v "^--" -
#
################################################################

check		: hugs
		  - /bin/sh Tests/testScript static
		  - /bin/sh Tests/testScript tcheck
		  - /bin/sh Tests/testScript rts
		  - /bin/sh Tests/testScript libs
		  - /bin/sh Tests/testScript demos
clean		::
		  $(RM) testFile




################################################################
# End of Makefile
################################################################
