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
CFLAGS		= -ML 
OPTFLAGS	= -O2
RC		= rc

LD		= ld
LDFLAGS    	=   
DLL_FLAGS	= /LD
PIC_FLAGS	= 
LIBS		= kernel32.lib user32.lib gdi32.lib comdlg32.lib shell32.lib advapi32.lib

YACC		= byacc
RM		= -del
CP		= copy



.SUFFIXES	:
.SUFFIXES	: .c .h .obj
HFILES		= prelude.h config.h options.h storage.h connect.h \
		  errors.h command.h server.h
CFILES		= hugs.c storage.c input.c static.c type.c subst.c \
		  output.c compiler.c machine.c interp.c builtin.c \
		  server.c ffi.c
INCFILES	= parser.c preds.c bignums.c scc.c machdep.c timer.c \
		  printer.c iomonad.c interns.c stmonad.c array.c
YFILES		= parser.y
SOURCES		= $(HFILES) $(CFILES) $(INCFILES) $(YFILES) 
OBJECTS		= storage.obj input.obj static.obj type.obj subst.obj \
                  compiler.obj plugin.obj builtin.obj machine.obj output.obj \
                  winframe.obj wintext.obj  winhint.obj \
                  winstln.obj wintoolb.obj winutils.obj ffi.obj
IOBJECTS	= hugs.obj $(OBJECTS)
PRELUDE		= config.h options.h prelude.h "winhugs\wintext.h"

################################################################
# Hugs interpreter and standalone evaluator
################################################################

winhugs.exe	: $(IOBJECTS)  winhugs.res
		  $(CC) $(LDFLAGS) $(IOBJECTS) winhugs.res  $(LIBS) -o winhugs.exe -link /subsystem:windows 


SERVER_OBJECTS	= server.obj $(OBJECTS) 
runhugs.obj	: $(PRELUDE) hugs.c machdep.c timer.c runhugs.c
runhugs.exe	: runhugs.obj $(SERVER_OBJECTS)
		  $(CC) $(LDFLAGS) runhugs.obj $(SERVER_OBJECTS) $(LIBS) -o runhugs.exe

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
# Dependencies
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

server.obj	: $(PRELUDE) storage.h connect.h errors.h version.h \
		  command.h machdep.c timer.c hugs.c server.c server.h
hugs.obj	: $(PRELUDE) storage.h connect.h errors.h version.h \
		  command.h machdep.c timer.c winhugs\winhugs.c winhugs\winbrows.c
ffi.obj		: $(PRELUDE) storage.h connect.h errors.h
storage.obj	: $(PRELUDE) storage.h connect.h errors.h
input.obj	: $(PRELUDE) storage.h connect.h errors.h \
		  parser.c command.h
subst.obj	: $(PRELUDE) storage.h connect.h errors.h subst.h
static.obj	: $(PRELUDE) storage.h connect.h errors.h subst.h \
                  scc.c
type.obj	: $(PRELUDE) storage.h connect.h errors.h subst.h \
		  scc.c preds.c
output.obj	: $(PRELUDE) storage.h connect.h errors.h
compiler.obj	: $(PRELUDE) storage.h connect.h errors.h 
machine.obj	: $(PRELUDE) storage.h connect.h errors.h
plugin.obj	: $(PRELUDE) storage.h connect.h errors.h
builtin.obj	: $(PRELUDE) storage.h connect.h errors.h \
		  bignums.c printer.c iomonad.c interns.c stmonad.c array.c


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

winhugs.res     : "winhugs\winhugs.rc"
                  $(RC) -fowinhugs.res "winhugs\winhugs.rc"

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
