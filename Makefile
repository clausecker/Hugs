include Defs.mk

# does an in-place install (mainly because src/unix; make config does that)
all: libraries src/Makefile
	cd src; make

libraries: fptools
	-mkdir fptools
	cvs -d ${CVSROOT} export -r${HSLIBSTAG} fptools/hslibs
	cvs -d ${CVSROOT} export -r${HSLIBSTAG} fptools/libraries
	cd src/unix; ./convert_hslibs ../../fptools
	cd src/unix; ./convert_libraries ../../fptools

src/Makefile:
	cd src/unix; make config

include RPM.mk
