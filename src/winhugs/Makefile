# Generated automatically from Makefile.in by configure.
# --------------------------------------------------------------------------
# Makefile for Hugs
#
# The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
# Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
# 1994-2004, All rights reserved.  It is distributed as
# free software under the license in the file "License", which is
# included in the distribution.
# --------------------------------------------------------------------------

# Targets:
#
# <default>:   	  make winhugs.exe
# install:	  make and install programs/libraries
# winhugs.exe: 	  make minimal working system
# runhugs.exe:	  make batch-mode version of Hugs
# clean:       	  delete files not required in running system
# distclean:      delete files that can be regenerated using C compiler
# veryclean:      delete all machine generated files 
#                   (you need perl, bison/yacc, etc to rebuild these files)
# TAGS:           build emacs TAGS table

# This rule goes first to make it the default choice
default		:: winhugs.exe 

CC		= cl /nologo
VERSTR          = -DMONTH_YEAR="\"Nov 2003\""
CFLAGS		= -MD -I. $(VERSTR)
# When debugging:
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
EXEEXT		= .exe
OBJEXT		= obj


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

WOBJECTS        = winhugs.obj winframe.obj wintext.obj winhint.obj \
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
		  $(RM) ffihugs.exe
		  $(RM) *.pdf
		  $(RM) TAGS
		  $(RM) *~
veryclean	:: distclean
veryclean	::
		  $(RM) config.h
		  $(RM) options.h
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
# Generated object dependencies (Don't change or delete this line)
################################################################

hugs.$(OBJEXT): hugs.c prelude.h config.h platform.h options.h storage.h HsFFI.h \
  command.h connect.h errors.h script.h opts.h strutil.h evaluator.h \
  machdep.h output.h module.h timer.c
runhugs.$(OBJEXT): runhugs.c prelude.h config.h platform.h options.h storage.h \
  HsFFI.h connect.h machdep.h observe.h builtin.h evaluator.h errors.h \
  server.h HugsAPI.h
server.$(OBJEXT): server.c prelude.h config.h platform.h options.h storage.h \
  HsFFI.h connect.h script.h machdep.h evaluator.h opts.h strutil.h \
  errors.h server.h HugsAPI.h
edit.$(OBJEXT): edit.c prelude.h config.h platform.h options.h storage.h HsFFI.h \
  connect.h errors.h opts.h strutil.h machdep.h
observe.$(OBJEXT): observe.c prelude.h config.h platform.h options.h storage.h \
  HsFFI.h connect.h command.h errors.h machdep.h builtin.h output.h \
  observe.h
builtin.$(OBJEXT): builtin.c prelude.h config.h platform.h options.h storage.h \
  HsFFI.h connect.h errors.h machdep.h char.h builtin.h bignums.c \
  printer.c iomonad.c timeprim.c dirprim.c interns.c array.c
char.$(OBJEXT): char.c prelude.h config.h platform.h options.h storage.h HsFFI.h \
  connect.h errors.h char.h unitable.c
compiler.$(OBJEXT): compiler.c prelude.h config.h platform.h options.h storage.h \
  HsFFI.h connect.h errors.h goal.h char.h output.h opts.h
errors.$(OBJEXT): errors.c prelude.h config.h platform.h options.h storage.h \
  HsFFI.h connect.h errors.h output.h opts.h goal.h char.h evaluator.h
evaluator.$(OBJEXT): evaluator.c prelude.h config.h platform.h options.h \
  storage.h HsFFI.h connect.h errors.h script.h output.h strutil.h opts.h \
  machdep.h evaluator.h
ffi.$(OBJEXT): ffi.c prelude.h config.h platform.h options.h storage.h HsFFI.h \
  connect.h errors.h output.h strutil.h
goal.$(OBJEXT): goal.c prelude.h config.h platform.h options.h storage.h HsFFI.h \
  connect.h errors.h machdep.h opts.h goal.h
input.$(OBJEXT): input.c prelude.h config.h platform.h options.h storage.h \
  HsFFI.h connect.h command.h errors.h module.h script.h opts.h goal.h \
  machdep.h char.h parser.c
machdep.$(OBJEXT): machdep.c prelude.h config.h platform.h options.h storage.h \
  HsFFI.h connect.h errors.h opts.h strutil.h machdep.h char.h \
  evaluator.h
machine.$(OBJEXT): machine.c prelude.h config.h platform.h options.h storage.h \
  HsFFI.h connect.h errors.h char.h opts.h
module.$(OBJEXT): module.c prelude.h config.h platform.h options.h storage.h \
  HsFFI.h connect.h errors.h module.h output.h
opts.$(OBJEXT): opts.c prelude.h config.h platform.h options.h storage.h HsFFI.h \
  connect.h errors.h machdep.h strutil.h opts.h char.h
output.$(OBJEXT): output.c prelude.h config.h platform.h options.h storage.h \
  HsFFI.h connect.h errors.h output.h char.h
plugin.$(OBJEXT): plugin.c prelude.h config.h platform.h options.h storage.h \
  HsFFI.h connect.h
script.$(OBJEXT): script.c prelude.h config.h platform.h options.h storage.h \
  HsFFI.h connect.h errors.h machdep.h opts.h strutil.h script.h
static.$(OBJEXT): static.c prelude.h config.h platform.h options.h storage.h \
  HsFFI.h connect.h machdep.h errors.h output.h subst.h module.h opts.h \
  goal.h scc.c
storage.$(OBJEXT): storage.c prelude.h config.h platform.h options.h storage.h \
  HsFFI.h connect.h opts.h errors.h machdep.h evaluator.h strutil.h \
  output.h
strutil.$(OBJEXT): strutil.c prelude.h config.h platform.h options.h storage.h \
  HsFFI.h connect.h errors.h strutil.h
subst.$(OBJEXT): subst.c prelude.h config.h platform.h options.h storage.h \
  HsFFI.h connect.h errors.h output.h opts.h subst.h
type.$(OBJEXT): type.c prelude.h config.h platform.h options.h storage.h HsFFI.h \
  connect.h errors.h output.h subst.h goal.h opts.h preds.c scc.c
version.$(OBJEXT): today version.c prelude.h config.h platform.h options.h

################################################################
# End of generated object dependencies (Don't change or delete this line)
################################################################

version.$(OBJEXT): echodate.h

echodate.h: echodate.exe
	$(PWD)/echodate.exe > echodate.h

echodate.exe: msc/echodate.c
	$(CC) $(LDFLAGS) msc/echodate.c -o $@

# WinHugs modules
#
# The dependencies could be made tighter by not using HFILES throughout,
# but individually naming the header files needed by each file. Elect
# not to do this due to the overhead of having to keep it up-to-date.
winhugs.$(OBJEXT): $(HFILES) \
  winhugs/winhugs.c winhugs/menusbm.c winhugs/WinBrows.c winhugs/WinHugs.h \
  winhugs/WinFrame.h winhugs/WinToolB.h winhugs/WinSTLN.h winhugs/WinUtils.h
winframe.$(OBJEXT): $(HFILES) winhugs/winframe.c \
  winhugs/WinFrame.h winhugs/WinSTLN.h winhugs/WinToolB.h winhugs/WinUtils.h
winhint.$(OBJEXT): $(HFILES) winhugs/winhint.c \
  winhugs/WinHint.h winhugs/WinUtils.h
winstln.$(OBJEXT): $(HFILES) winhugs/winstln.c \
  winhugs/WinSTLN.h winhugs/WinUtils.h
wintoolb.$(OBJEXT): $(HFILES) winhugs/wintoolb.c \
  winhugs/WinToolB.h winhugs/WinHint.h winhugs/WinUtils.h
winutils.$(OBJEXT): $(HFILES) winhugs/winutils.c winhugs/WinUtils.h

################################################################
# End of Makefile
################################################################
