# Generated automatically from Makefile.in by configure.
# --------------------------------------------------------------------------
# Makefile for Hugs
#
# The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
# Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
# 1994-2003, All rights reserved.  It is distributed as
# free software under the license in the file "License", which is
# included in the distribution.
# --------------------------------------------------------------------------

# Targets:
#
# <default>:   	  make hugs.exe and runhugs.exe
# install:	  make and install programs/libraries
# hugs.exe:    	  make minimal working system
# runhugs.exe:	  make batch-mode version of Hugs
# hugsscript.dll: make DLL version of Hugs
# clean:       	  delete files not required in running system
# distclean:      delete files that can be regenerated using C compiler
# veryclean:      delete all machine generated files 
#                   (you need perl, bison/yacc, etc to rebuild these files)
# check:	  run regression tests
# TAGS:           build emacs TAGS table

# This rule goes first to make it the default choice
default		:: hugs.exe runhugs.exe hugsscript.dll

CC		= cl /nologo
CFLAGS		= -clr -DDOTNET -DMONTH_YEAR="\"Nov 2003\""
#CFLAGS		= -ML -Zi -DMONTH_YEAR="\"Nov 2003""
OPTFLAGS	= -O2

LD		= $(CC) -clr
LDFLAGS    	= 
# When debugging:
#LDFLAGS	= -Zi
DLL_FLAGS	= /LD
PIC_FLAGS	= 
LIBS		=  kernel32.lib advapi32.lib

YACC		= bison
RM		= -del
CP		= copy
OBJ		= .obj


.SUFFIXES	:
.SUFFIXES	: .c .h .obj
HFILES		= prelude.h config.h options.h storage.h connect.h \
		  errors.h command.h server.h HsFFI.h module.h machdep.h \
		  script.h opts.h strutil.h evaluator.h goal.h observe.h
DOTNET_FILES    = dotnet\Invoker.cpp dotnet\prim.cpp dotnet\HugsServ.cpp dotnet\HugsWrapper.cpp
CFILES		= hugs.c storage.c input.c static.c type.c subst.c \
		  output.c compiler.c machine.c interp.c builtin.c \
		  server.c ffi.c module.c machdep.c script.c opts.c \
		  strutil.c errors.c evaluator.c goal.c observe.c \
		  $(DOTNET_FILES)
INCFILES	= parser.c preds.c bignums.c scc.c timer.c \
		  printer.c iomonad.c interns.c array.c
YFILES		= parser.y
SOURCES		= $(HFILES) $(CFILES) $(INCFILES) $(YFILES) 
OBJECTS		= storage.obj input.obj static.obj type.obj subst.obj \
	          compiler.obj plugin.obj builtin.obj machine.obj output.obj \
		  ffi.obj version.obj module.obj machdep.obj script.obj \
		  opts.obj strutil.obj errors.obj evaluator.obj goal.obj

DOBJECTS        = dotnet\Invoker.obj dotnet\HugsServ.obj dotnet\HugsWrapper.obj dotnet\prim.obj 
		  
IOBJECTS	= hugs.obj edit.obj server.obj observe.obj hugs.res $(OBJECTS) $(DOBJECTS)
PRELUDE		= config.h options.h prelude.h

################################################################
# Hugs interpreter and standalone evaluator
################################################################

hugs.exe	: $(IOBJECTS) 
		  $(LD) $(LDFLAGS) $(IOBJECTS)  $(LIBS) -o hugs.exe  $(LINK_FLAGS)
		  

SERVER_OBJECTS	= runhugs.res $(OBJECTS) server.obj $(DOBJECTS)
runhugs.obj	: $(PRELUDE) hugs.c machdep.h timer.c runhugs.c
runhugs.exe	: runhugs.obj $(SERVER_OBJECTS)
		  $(LD) $(LDFLAGS) runhugs.obj $(SERVER_OBJECTS) $(LIBS) -o runhugs.exe $(LINK_FLAGS)

ffihugs.exe	: ffihugs.obj $(SERVER_OBJECTS)
		  $(CC) $(LDFLAGS) ffihugs.obj $(SERVER_OBJECTS) $(LIBS) -o ffihugs.exe $(LINK_FLAGS)

ffihugs.obj	: runhugs.obj
		  $(CC) -c $(CFLAGS) $(OPTFLAGS) -DFFI_COMPILER runhugs.c /Foffihugs.obj

hugsscript.obj  : hugsscript.c
hugsscript.dll  : $(SERVER_OBJECTS) hugsscript.obj
		  $(CC) $(DLL_FLAGS) $(LDFLAGS) $(SERVER_OBJECTS) hugsscript.obj $(LIBS) -o hugsscript.dll

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
	$(FFIHUGS) $(FFIHUGS_OPTS) +L../libraries/Network/initWinSock.c +Lwsock32.lib Network.Socket
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
		  $(RM) *.RES
		  $(RM) *.aux
		  $(RM) *.hp
		  $(RM) hugs.exe
		  $(RM) runhugs.exe
		  $(RM) ffihugs.exe

distclean	:: clean
distclean	::
		  $(RM) hugs.exe
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

dotnet\prim.obj	: dotnet\prim.cpp
		  $(CC) -c $(CFLAGS) $(OPTFLAGS) -I. dotnet\prim.cpp /Fodotnet\prim.obj

dotnet\Invoker.obj : dotnet\Invoker.cpp
		  $(CC) -c $(CFLAGS) $(OPTFLAGS) -I. dotnet\Invoker.cpp /Fodotnet\Invoker.obj

dotnet\HugsServ.obj : dotnet\HugsServ.cpp
		  $(CC) -c $(CFLAGS) $(OPTFLAGS) -I. dotnet\HugsServ.cpp /Fodotnet\HugsServ.obj

dotnet\HugsServ.cpp : dotnet\HugsServ.h

dotnet\HugsWrapper.obj : dotnet\HugsWrapper.cpp
		  $(CC) -c $(CFLAGS) $(OPTFLAGS) -I. dotnet\HugsWrapper.cpp /Fodotnet\HugsWrapper.obj

dotnet\HugsWrapper.cpp : dotnet\HugsWrapper.h

dotnet\prim.obj : $(PRELUDE) dotnet\prim.cpp dotnet\Invoker.h dotnet\Invoker.cpp

# Old & unused, but keep it around for now.
#dotnet\HugsWrapper.dll : hugs.exe dotnet\HugsWrapper.cs
#		  csc /t:library /unsafe /out:dotnet\HugsWrapper.dll dotnet\HugsWrapper.cs  /r:hugs.exe
#dotnet\HugsWrapper.dll: $(PRELUDE) dotnet\HugsWrapper.cs

# These are compiled with less optimisation to avoid optimisation bugs in
# certain compilers.  This may be overly conservative on some compilers.
compiler.obj	: compiler.c
		  $(CC) -c $(CFLAGS) compiler.c
static.obj	: static.c
		  $(CC) -c $(CFLAGS) static.c

parser.c	: parser.y
		  -$(YACC) parser.y
		  mv parser.tab.c parser.c

# veryclean	::
# 		  $(RM) parser.c

hugs.res	: msc\hugs.rc ..\icons\hugsicon.ico
		rc /imsc /i..\icons /r /fo hugs.res msc\hugs.rc 

runhugs.res	: msc\runhugs.rc ..\icons\hsxicon.ico
		rc /imsc /I..\icons /r /fo runhugs.res msc\runhugs.rc 

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
# Regression tests (none supplied)
################################################################

check		:


################################################################
# End of Makefile
################################################################
