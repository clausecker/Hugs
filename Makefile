#
# A (GNU) Makefile for checking out libraries + buildings RPMs
#
include Defs.mk

all: fptools pp-fptools src/Makefile
	cd src; make

src/Makefile:
	cd src/unix; make config EXTRA_CONFIGURE_OPTS=$(EXTRA_CONFIGURE_OPTS)

#
# Utilities needed to check out and process fptools. To override
# these, set them on the command-line when invoking 'make':
#
#  foo$ make FIND=/usr/bin/find HAPPY=c:/happy/happy-1.15/bin/happy ...
#
# (You'll find 'hsc2hs' included in a GHC distribution.)
#
FIND=find
HAPPY=happy
HSC2HS=hsc2hs
CVS=cvs

fptools:
	-mkdir fptools
	$(CVS) -d ${CVSROOT} export -r${HSLIBSTAG} $(addprefix fptools/hslibs/,${HSLIBSDIRS})
	$(CVS) -d ${CVSROOT} export -r${LIBRARIESTAG} $(addprefix fptools/libraries/,${LIBRARIESDIRS})

.PHONY: pp-fptools

# Preprocess fptools checkout, so the package can be built
# without happy & ghc changes here should be reflected also
# in RPM.mk (sorry)
pp-fptools: fptools fptools/stamp-fptools

fptools/stamp-fptools:
	$(FIND) fptools/libraries -name "*.ly" -o -name "*.y" |\
		xargs -l $(HAPPY)
ifneq "$(USING_AN_OLDER_HSC2HS)" "YES"
	$(FIND) fptools/libraries -name "*.hsc" |\
		xargs -l $(HSC2HS) --no-compile --template=template-hsc.h
else
	$(FIND) fptools/libraries -name "*.hsc" |\
		xargs -l $(HSC2HS) --no-compile
	$(FIND) fptools/libraries -name "*_hsc_make.c" |\
		xargs src/unix/hsc_kludge
endif
	@touch fptools/stamp-fptools

include RPM.mk
