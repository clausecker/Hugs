# Top-level Makefile for Hugs
# (this should be a POSIX 1003.2-1992 Makefile)

# Start of general settings (leave this line unchanged)

NAME = hugs98

# Set to 0 if a snapshot release.
MAJOR_RELEASE = 0

# Release number of RPM.
RELEASE = 1

TAG = HEAD

CVS_ROOT = :pserver:anoncvs@cvs.haskell.org:/cvs

DARCS_ROOT = http://darcs.haskell.org
LIBRARIESDIRS = ALUT array base bytestring Cabal containers directory fgl \
	filepath GLUT haskell98 haskell-src HaXml HGL HUnit mtl network \
	old-locale old-time OpenAL OpenGL parallel parsec pretty process \
	QuickCheck random regex-base regex-compat regex-posix stm time \
	unix Win32 X11 xhtml
DARCS_CPPHS = http://www.cs.york.ac.uk/fp/darcs/cpphs

# End of general settings (leave this line unchanged)

PACKAGES	= packages/base/base.cabal
DARCS_GET	= darcs get --partial

# General targets:
#
# all:		(default) build a system that can be run in-place.
# install:	all + install in $(prefix) as supplied to configure.
# clean:	delete files generated in building, except those needed
#		for in-place use.
# distclean:	delete files created by configuring or building.
# veryclean:	delete most things that can be regenerated (though this
#		will require additional tools).
# check:	run regression tests

# Note: we have to build in src before building in libraries, as the
# latter uses the binaries in src.  See libraries/Makefile.in for
# details of the bootstrapping of the libraries.

all: $(PACKAGES) src/Makefile
	cd src; $(MAKE) all
	cd libraries; $(MAKE) all
	cd docs; $(MAKE) all

# We install the standard libraries and the simple demos.
# We don't install things which don't work on Unix (e.g. Win32).

install: install_all_but_docs
	cd docs; $(MAKE) install

# Install everything except documentation, which is installed differently
# by some packagers (e.g. rpm)

install_all_but_docs: $(PACKAGES) src/Makefile
	cd src; $(MAKE) install
	cd libraries; $(MAKE) install
	cd demos; $(MAKE) install

clean: clean_root
	cd src; if test -f Makefile; then $(MAKE) clean; fi
	cd libraries; if test -f Makefile; then $(MAKE) clean; fi
	cd docs; if test -f Makefile; then $(MAKE) clean; fi
	cd demos; if test -f Makefile; then $(MAKE) clean; fi

distclean: distclean_root
	cd src; if test -f Makefile; then $(MAKE) distclean; fi
	cd libraries; if test -f Makefile; then $(MAKE) distclean; fi
	cd docs; if test -f Makefile; then $(MAKE) distclean; fi
	cd demos; if test -f Makefile; then $(MAKE) distclean; fi

veryclean: veryclean_root
	cd src; if test -f Makefile; then $(MAKE) veryclean; fi
	cd libraries; if test -f Makefile; then $(MAKE) veryclean; fi
	cd docs; if test -f Makefile; then $(MAKE) veryclean; fi
	cd demos; if test -f Makefile; then $(MAKE) veryclean; fi

clean_root:
	$(RM) *.tar.gz *.rpm Defs.mk
	$(RM) *~

distclean_root: clean_root
	$(RM) -r config.status config.log config.cache autom4te.cache
	$(RM) MkDefs tests/config

veryclean_root: distclean_root

################################################################
# Regression tests (Unix only)
#
# Uses runstdtest (from ghc-0.26/ghc/glafp-utils/scripts), perl 5
# and /bin/sh (Bourne shell).
#
# "make verbosecheck" generates a lot of output to explain what is going on
# and reassure you that progress is being made.  This is great if you've
# never run these tests before - but if you just want to reassure yourself
# that nothing has broken since the last release, you might prefer to
# run "make check" which removes all the explanations and success
# stories - leaving just the errors (if any).
#
################################################################

check: all
	cd tests && sh testScript | if egrep -v '^--( |-----)'; then false; else true; fi

verbosecheck: all
	cd tests && sh testScript

# Building distributions

tarplus: Defs.mk
	$(MAKE) -f RPM.mk tar

tar: Defs.mk
	$(MAKE) PKGNAME=$(NAME) LIBRARIESDIRS='base Cabal directory filepath haskell98 old-locale old-time pretty process random' -f RPM.mk tar

rpm: Defs.mk
	$(MAKE) -f RPM.mk rpm

Defs.mk: Makefile
	( echo '# Automatically extracted from Makefile (so edit that instead)';\
	  sed -n '/^# Start of general settings/,/^# End of general settings/p' Makefile;\
	) >$@

# Build phases:

# configuration

src/Makefile: configure
	$(RM) -r config.cache autom4te.cache
	LIBS=$(GNULIBS) ./configure $(EXTRA_CONFIGURE_OPTS)

configure: configure.ac aclocal.m4 $(PACKAGES)
	for dir in packages/*; do if test -f $$dir/configure.ac; \
		then (cd $$dir; autoreconf); fi; done
	-autoreconf

# fetching library sources and utility programs

$(PACKAGES):
	mkdir packages
	for lib in $(LIBRARIESDIRS); do $(DARCS_GET) --repo-name=packages/$$lib $(DARCS_ROOT)/packages/$$lib; done
	$(DARCS_GET) $(DARCS_CPPHS)
	$(DARCS_GET) $(DARCS_ROOT)/hsc2hs
