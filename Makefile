# Top-level Makefile for Hugs
# (this should be a POSIX 1003.2-1992 Makefile)

# Start of general settings (leave this line unchanged)

NAME = hugs98

# Set to 0 if a snapshot release.
MAJOR_RELEASE = 0

# Release number of RPM.
RELEASE = 1

TAG = HEAD
HSLIBSTAG = HEAD
LIBRARIESTAG = HEAD

HSLIBSDIRS = concurrent data hssource lang net text util posix
LIBRARIESDIRS = base haskell98 haskell-src network parsec QuickCheck unix \
	GLUT OpenGL fgl

# End of general settings (leave this line unchanged)

# General targets:
#
# all:		(default) build a system that can be run in-place.
# install:	all + install in $(prefix) as supplied to configure.
# clean:	delete files generated in building, except those needed
#		for in-place use.
# distclean:	delete files created by configuring or building.
# veryclean:	delete most things that can be regenerated (though this
#		require additional tools).

all: fptools src/Makefile
	cd src; $(MAKE) all
	cd docs; $(MAKE) all

install: fptools src/Makefile
	cd src; $(MAKE) install
	cd docs; $(MAKE) install

clean:
	cd src; if test -f Makefile; then $(MAKE) clean; fi
	cd docs; if test -f Makefile; then $(MAKE) clean; fi

distclean:
	$(RM) *.tar.gz *.rpm
	$(RM) -r config.log config.cache autom4te.cache
	cd src; if test -f Makefile; then $(MAKE) distclean; fi
	cd docs; if test -f Makefile; then $(MAKE) distclean; fi

veryclean:
	$(RM) *.tar.gz *.rpm
	$(RM) -r config.log config.cache autom4te.cache
	cd src; if test -f Makefile; then $(MAKE) veryclean; fi
	cd docs; if test -f Makefile; then $(MAKE) veryclean; fi

# Building distributions

tar: Defs.mk
	$(MAKE) -f RPM.mk tar

rpm: Defs.mk
	$(MAKE) -f RPM.mk rpm

Defs.mk: Makefile
	( echo '# Automatically extracted from Makefile (so edit that instead)';\
	  sed -n '/^# Start of general settings/,/^# End of general settings/p' Makefile;\
	) >$@

# Build phases:

# configuration

src/Makefile: configure src/config.h.in
	$(RM) -r config.cache autom4te.cache
	LIBS=$(GNULIBS) ./configure $(EXTRA_CONFIGURE_OPTS)

configure: configure.ac
	-autoconf

src/config.h.in: configure.ac
	-autoheader

# fetching library sources

fptools:
	-mkdir fptools
	cvs -d `cat CVS/Root` get -r${HSLIBSTAG} `for d in ${HSLIBSDIRS}; do echo fptools/hslibs/$$d; done`
	cvs -d `cat CVS/Root` get -r${LIBRARIESTAG} `for d in ${LIBRARIESDIRS}; do echo fptools/hslibs/$$d; done`
