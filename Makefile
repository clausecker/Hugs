include Defs.mk

all: fptools src/Makefile
	cd src; make

src/Makefile:
	cd src/unix; make config

fptools:
	-mkdir fptools
	cvs -d ${CVSROOT} export -r${HSLIBSTAG} $(addprefix fptools/hslibs/,${HSLIBSDIRS})
	cvs -d ${CVSROOT} export -r${LIBRARIESTAG} $(addprefix fptools/libraries/,${LIBRARIESDIRS})
	# preprocess these, so the package can be built without happy & ghc
	# changes here should be reflected also in RPM.mk (sorry)
	find fptools/libraries -name "*.ly" -o -name "*.y" |\
		xargs -l happy
	find fptools/libraries -name "*.hsc" |\
		xargs -l hsc2hs --no-compile
	find fptools/libraries -name "*_hsc_make.c" |\
		xargs src/unix/hsc_kludge

include RPM.mk
