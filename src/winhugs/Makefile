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
PRELUDE		= config.h options.h prelude.h script.h platform.h
HFILES		= $(PRELUDE) storage.h connect.h \
		  errors.h command.h server.h HsFFI.h module.h machdep.h \
		  opts.h strutil.h evaluator.h goal.h observe.h char.h
CFILES		= hugs.c storage.c input.c static.c type.c subst.c \
		  output.c compiler.c machine.c interp.c builtin.c \
		  server.c ffi.c module.c machdep.c script.c opts.c \
		  strutil.c errors.c evaluator.c goal.c observe.c char.c \
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
		  opts.obj strutil.obj errors.obj evaluator.obj goal.obj char.obj \
		  $(WOBJECTS)

IOBJECTS	= hugs.obj edit.obj observe.obj $(OBJECTS)

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

wintext.obj	: $(HFILES) "winhugs\wintext.c"
		  $(CC) -c $(CFLAGS) $(OPTFLAGS)  "winhugs\wintext.c" 

winframe.obj    : $(HFILES) "winhugs\winframe.c"
	          $(CC) -c $(CFLAGS) $(OPTFLAGS) "winhugs\winframe.c" 

winhint.obj	: $(HFILES) "winhugs\winhint.c"
		  $(CC) -c $(CFLAGS) $(OPTFLAGS) "winhugs\winhint.c" 

winstln.obj	: $(HFILES) "winhugs\winstln.c"
	          $(CC) -c $(CFLAGS) $(OPTFLAGS) "winhugs\winstln.c" 

wintoolb.obj    : $(HFILES) "winhugs\wintoolb.c"
	          $(CC) -c $(CFLAGS) $(OPTFLAGS) "winhugs\wintoolb.c" 

winutils.obj    : $(HFILES) "winhugs\winutils.c"
	          $(CC) -c $(CFLAGS) $(OPTFLAGS) "winhugs\winutils.c" 

winhugs.obj    : $(HFILES) "winhugs\winhugs.c"
	          $(CC) -c $(CFLAGS) $(OPTFLAGS) "winhugs\winhugs.c" 

winhugs.res     : "winhugs\winhugs.rc"
                  $(RC) -fowinhugs.res "winhugs\winhugs.rc"

################################################################
# Object dependencies (Don't change or delete this line)
################################################################

# The dependencies could be made tighter by not using HFILES throughout,
# but individually naming the header files needed by each file. Elect
# not to do this due to the overhead of having to keep it up-to-date.

hugs$(OBJ): hugs.c $(HFILES)
runhugs$(OBJ): runhugs.c $(HFILES) HugsAPI.h
server$(OBJ): server.c $(HFILES) HugsAPI.h
edit$(OBJ): edit.c $(HFILES)
observe$(OBJ): observe.c $(HFILES)
builtin$(OBJ): builtin.c $(HFILES) bignums.c printer.c iomonad.c timeprim.c dirprim.c array.c
char$(OBJ): char.c $(HFILES)
compiler$(OBJ): compiler.c $(HFILES)
errors$(OBJ): errors.c $(HFILES)
evaluator$(OBJ): evaluator.c $(HFILES)
ffi$(OBJ): ffi.c $(HFILES)
goal$(OBJ): goal.c $(HFILES)
input$(OBJ): input.c $(HFILES) parser.c
machdep$(OBJ): machdep.c $(HFILES)
machine$(OBJ): machine.c $(HFILES)
module$(OBJ): module.c $(HFILES)
opts$(OBJ): opts.c $(HFILES)
output$(OBJ): output.c $(HFILES)
plugin$(OBJ): plugin.c $(HFILES)
script$(OBJ): script.c $(HFILES)
static$(OBJ): static.c $(HFILES) scc.c
storage$(OBJ): storage.c $(HFILES)
strutil$(OBJ): strutil.c $(HFILES)
subst$(OBJ): subst.c $(HFILES)
type$(OBJ): type.c $(HFILES) preds.c scc.c
version$(OBJ): version.c $(PRELUDE)

# WinHugs modules
Winhugs$(OBJ): $(HFILES) winhugs/winhugs.c winhugs/menusbm.c winhugs/WinBrows.c winhugs/WinFrame.h winhugs/WinToolB.h winhugs/WinSTLN.h winhugs/WinUtils.h winhugs/WinHugs.h

winframe$(OBJ): $(HFILES) winhugs/winframe.c winhugs/WinFrame.h winhugs/WinSTLN.h winhugs/WinToolB.h winhugs/WinUtils.h

winhint$(OBJ): $(HFILES) winhugs/winhint.c  winhugs/WinHint.h winhugs/WinUtils.h
winstln$(OBJ): $(HFILES) winhugs/winstln.c  winhugs/WinSTLN.h winhugs/WinUtils.h
wintoolb$(OBJ): $(HFILES) winhugs/wintoolb.c winhugs/WinToolB.h winhugs/WinHint.h winhugs/WinUtils.h
winutils$(OBJ): $(HFILES) winhugs/winutils.c winhugs/WinUtils.h


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
