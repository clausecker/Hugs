include Defs.mk

# does an in-place install (mainly because src/unix; make config does that)
# FIXME
all: src/Makefile fptools
	cd src; make && make libraries

src/Makefile:
	cd src/unix; make config

fptools:
	-mkdir fptools
	cvs -d ${CVSROOT} export -r${HSLIBSTAG} $(addprefix fptools/,${HSLIBSDIRS})

include RPM.mk
