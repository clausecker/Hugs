# src/Makefile.  Generated from Makefile.in by configure.
# --------------------------------------------------------------------------
# Makefile for Hugs interpreter
# (this should be a POSIX 1003.2-1992 Makefile)
#
# The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
# Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
# 1994-2004, All rights reserved.  It is distributed as
# free software under the license in the file "License", which is
# included in the distribution.
# --------------------------------------------------------------------------

# Targets:
#
# <default>:   	  make hugs and runhugs
# install:	  make and install programs
# hugs:	  make minimal working system
# runhugs: make batch-mode version of Hugs
# clean:       	  delete files not required in running system
# distclean:      delete files that can be regenerated using C compiler
# veryclean:      delete all machine generated files 
#                   (you need perl, bison/yacc, etc to rebuild these files)
# TAGS:           build emacs TAGS table

CC		= gcc
CFLAGS		= -DNDEBUG=1 -g 
OPTFLAGS	= -O2

LD		= ld
LDFLAGS    	=   
LIBS		= -lreadline -lncurses -lm -ldl 

YACC		= bison -y
RM		= /bin/rm -f
CP		= /bin/cp
EXEEXT		= 
OBJEXT		= o
STRIP		= 

# These variables determine where various parts of the Hugs system are
# installed.  (They are ignored in Windows or DOS.)
# Binaries are installed in $(bindir); libraries go in $(hugsdir)/libraries
prefix		= /HUGS
exec_prefix	= ${prefix}
bindir		= ${exec_prefix}/bin
libdir		= ${exec_prefix}/lib
datarootdir	= ${prefix}/share
datadir		= ${datarootdir}
mandir          = ${datarootdir}/man
hugsdir		= ${libdir}/hugs

.SUFFIXES	:
.SUFFIXES	: .c .h .$(OBJEXT)

HFILES		= HsFFI.h builtin.h char.h command.h config.h connect.h \
		  errors.h evaluator.h goal.h machdep.h module.h observe.h \
		  options.h opts.h output.h prelude.h script.h server.h \
		  storage.h strutil.h subst.h
CFILES		= hugs.c runhugs.c server.c edit.c observe.c \
		  builtin.c char.c compiler.c errors.c evaluator.c ffi.c \
		  goal.c input.c machdep.c machine.c module.c opts.c \
		  output.c plugin.c script.c static.c storage.c strutil.c \
		  subst.c type.c version.c
INCFILES	= array.c bignums.c dirprim.c interns.c iomonad.c \
		  preds.c printer.c scc.c timeprim.c timer.c
YFILES		= parser.y
SOURCES		= $(HFILES) $(CFILES) $(INCFILES) $(YFILES) 
OBJECTS		= builtin.$(OBJEXT) char.$(OBJEXT) compiler.$(OBJEXT) \
		  errors.$(OBJEXT) evaluator.$(OBJEXT) ffi.$(OBJEXT) \
		  goal.$(OBJEXT) input.$(OBJEXT) machdep.$(OBJEXT) \
		  machine.$(OBJEXT) module.$(OBJEXT) opts.$(OBJEXT) \
		  output.$(OBJEXT) plugin.$(OBJEXT) script.$(OBJEXT) \
		  static.$(OBJEXT) storage.$(OBJEXT) strutil.$(OBJEXT) \
		  subst.$(OBJEXT) type.$(OBJEXT) version.$(OBJEXT)
IOBJECTS	= hugs.$(OBJEXT) edit.$(OBJEXT) observe.$(OBJEXT) $(OBJECTS)
WOBJECTS	= 
RC_FILES	= 

################################################################
# Default target
################################################################

# This rule goes first to make it the default choice
default		:: all

all		:: hugs$(EXEEXT) runhugs$(EXEEXT) ffihugs$(EXEEXT)

################################################################
# Hugs interpreter and standalone evaluator
################################################################

hugs$(EXEEXT)	: $(IOBJECTS) $(WOBJECTS) $(RC_FILES)
		  $(CC) $(LDFLAGS) $(IOBJECTS) $(WOBJECTS) $(LIBS) -o hugs$(EXEEXT) 
		  $(STRIP)

version.$(OBJEXT) : version.c $(SOURCES)
		  @$(CC) \
		      -DMONTH_YEAR='"'"`date +'%B %Y' 2> /dev/null`"'"' \
		      -DYYYYMMDD='"'"`date +'%Y%m%d' 2> /dev/null`"'"' \
		      -c $(CFLAGS) $(OPTFLAGS) $<

SERVER_OBJECTS	= server.$(OBJEXT) $(OBJECTS) 
runhugs$(EXEEXT) : runhugs.$(OBJEXT) $(SERVER_OBJECTS)
		  $(CC) $(LDFLAGS) runhugs.$(OBJEXT) $(SERVER_OBJECTS) $(LIBS) -o runhugs$(EXEEXT)

ffihugs$(EXEEXT) : ffihugs.$(OBJEXT) $(SERVER_OBJECTS)
		  $(CC) $(LDFLAGS) ffihugs.$(OBJEXT) $(SERVER_OBJECTS) $(LIBS) -o ffihugs$(EXEEXT)

ffihugs.$(OBJEXT) : runhugs.$(OBJEXT)
		  $(CC) -c $(CFLAGS) $(OPTFLAGS) -DFFI_COMPILER runhugs.c -o ffihugs.$(OBJEXT)

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
		  $(RM) hugs$(EXEEXT)
		  $(RM) runhugs$(EXEEXT)
		  $(RM) ffihugs$(EXEEXT)
		  $(RM) Makefile
		  $(RM) config.h
		  $(RM) options.h
		  $(RM) platform.h
		  $(RM) *.pdf
		  $(RM) TAGS tags
		  $(RM) *~
veryclean	:: distclean
veryclean	::
TAGS		::
		  etags *.[ych]

################################################################
# C and Yacc rules
################################################################

.c.$(OBJEXT)	:
		  $(CC) -c $(CFLAGS) $(OPTFLAGS) $<

# Modules to be compiled without optimization.
# (old comment: to avoid optimisation bugs in certain compilers.
#               This may be overly conservative on some compilers.)
# (The following explanation is based on a posting by Alastair Reid.)
# These modules allocate cells on the Hugs heap and assume a conservative
# garbage collector.  On some (especially RISC) architectures, the
# optimizer may identify a pointer to a Cell as a common subexpression,
# and hold that instead of the Cell.  This would then be missed by the
# conservative garbage collector's simplistic scan of the C stack.
# Modules associated with evaluation are safe because they don't assume
# conservative GC (see IMPORTANT NOTICE in builtin.c).
compiler.$(OBJEXT) : compiler.c
		  $(CC) -c $(CFLAGS) compiler.c
module.$(OBJEXT) : module.c
		  $(CC) -c $(CFLAGS) module.c
subst.$(OBJEXT)	: subst.c
		  $(CC) -c $(CFLAGS) subst.c
static.$(OBJEXT) : static.c
		  $(CC) -c $(CFLAGS) static.c
type.$(OBJEXT)	: type.c
		  $(CC) -c $(CFLAGS) type.c

parser.c	: parser.y
		  -$(YACC) parser.y
		  mv y.tab.c parser.c
# veryclean	::
# 		  $(RM) parser.c

################################################################
# Generating object dependencies (requires gcc)
################################################################

HUGS_MAKEFILES = Makefile dotnet/Makefile msc/Makefile winhugs/Makefile

# optional definitions that govern include files
EXTRA_DEFINES = -DINTERNAL_PRIMS -DUNICODE_CHARS -DWANT_TIMER

depend: $(SOURCES) parser.c
	gcc -MM $(EXTRA_DEFINES) $(CFILES) | sed 's/\.o:/.$$(OBJEXT):/' >TMP
	if cmp -s TMP MkDepend.in; then : ; else \
		$(CP) TMP MkDepend.in; \
		for mfile in $(HUGS_MAKEFILES);\
		do	(echo '/^# Generated object dependencies/+++,/^# End of generated object dependencies/---d';\
			 echo '-r MkDepend.in';\
			 echo w) | ed - $$mfile; \
		done;\
	fi
	rm TMP

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
version.$(OBJEXT): version.c prelude.h config.h platform.h options.h

################################################################
# End of generated object dependencies (Don't change or delete this line)
################################################################

################################################################
# Installation (Unix only)
################################################################

INSTALL		= ../install-sh

install		:: hugs$(EXEEXT) runhugs$(EXEEXT) ffihugs$(EXEEXT)
		$(INSTALL) -d $(DESTDIR)$(bindir)
		$(INSTALL) -c hugs$(EXEEXT) $(DESTDIR)$(bindir)
		$(INSTALL) -c runhugs$(EXEEXT) $(DESTDIR)$(bindir)
		$(INSTALL) -c ffihugs$(EXEEXT) $(DESTDIR)$(bindir)

################################################################
# End of Makefile
################################################################
