#
# A (GNU) Makefile for checking out libraries + buildings RPMs
#
include Defs.mk

CVS=cvs

all: fptools src/Makefile
	cd src; $(MAKE)

src/Makefile: configure src/config.h.in
	$(RM) -r config.cache autom4te.cache
	LIBS=$(GNULIBS) ./configure $(EXTRA_CONFIGURE_OPTS)

configure: configure.ac
	-autoconf

src/config.h.in: configure.ac
	-autoheader

fptools:
	-mkdir fptools
	$(CVS) -d ${CVSROOT} export -r${HSLIBSTAG} $(addprefix fptools/hslibs/,${HSLIBSDIRS})
	$(CVS) -d ${CVSROOT} export -r${LIBRARIESTAG} $(addprefix fptools/libraries/,${LIBRARIESDIRS})

include RPM.mk
