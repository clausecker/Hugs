# Top-level Makefile for Hugs
# (mostly portable, except for the use of include)

# include isn't 1003.2-1992, but is widely understood.
# If necessary, replace the line with the contents of the file.
include Defs.mk

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
	cvs -d `cat CVS/Root` get -r${HSLIBSTAG} `for d in ${HSLIBSDIRS}; do echo fptools/hslibs/$$d; done`
	cvs -d `cat CVS/Root` get -r${LIBRARIESTAG} `for d in ${LIBRARIESDIRS}; do echo fptools/hslibs/$$d; done`

clean:
	-cd src; if test -f Makefile; then $(MAKE) veryclean; fi
	-cd docs; if test -f Makefile; then $(MAKE) veryclean; fi
	-rm -f *.tar.gz *.rpm

tar:
	$(MAKE) -f RPM.mk tar

rpm:
	$(MAKE) -f RPM.mk rpm
