# this Makefile is for building RPM distributions, and is, not surprisingly,
# somewhat redhat specific (and rpm version 3 specific - i.e. RedHat 6.0
# or greater).

MONTH_YEAR = ${shell date +"%B %Y"}
MON_YEAR = ${shell date +"%b%Y"}
YEAR_MONTH_DAY = ${shell date +"%Y%m%d"}

NAME=hugs98
# convention: a release uses the MON_YEAR form of version,
# while a snapshot uses the YEAR_MONTH_DAY form.
# this should be sync'd with src/version.h
# VERSION=${MON_YEAR}
VERSION=${YEAR_MONTH_DAY}
RELEASE=1

RPMTMP = /tmp/rpm
# probably should be uniqueified
TARTMP = /tmp/mktar

SPECFILE=${NAME}.spec
PACKAGE=${NAME}-${VERSION}

RPMDEFS = --define "_topdir ${RPMTMP}" \
	  --define "name ${NAME}" \
	  --define "version ${VERSION}" \
	  --define "release ${RELEASE}" \

# TAG=Dec2001
# HSLIBSTAG=hugs-Dec2001
TAG=HEAD
HSLIBSTAG=HEAD

all:
	@echo "Make what?"

# unless you've just unpacked, `make clean' first
tar: ${PACKAGE}.tar.gz

CVSROOT = ${shell cat CVS/Root}

${PACKAGE}.tar.gz:
	-rm -rf ${TARTMP}
	-mkdir -p ${TARTMP}
	cd ${TARTMP}; cvs -d ${CVSROOT} export -r${TAG} hugs98
	cd ${TARTMP}; cvs -d ${CVSROOT} export -r${HSLIBSTAG} fptools/hslibs
	# using `make parser.c' would be best, but by default yacc
	# will be used, and yacc is, for some reason, incompatible
	cp ${TARTMP}/hugs98/src/version.h /tmp/mktar
	cd ${TARTMP}/hugs98/src; sed -e "s/MONTH_YEAR/${MONTH_YEAR}/" -e "s/YYYYMMDD/${YEAR_MONTH_DAY}/" < ${TARTMP}/version.h > ${TARTMP}/hugs98/src/version.h
	cd ${TARTMP}/hugs98/src; bison -y parser.y; mv y.tab.c parser.c
	cd ${TARTMP}/hugs98/src/unix; ./convert_hslibs ${TARTMP}/fptools
	# Siggy deren't like these in distros
	cd ${TARTMP}/hugs98; rm -rf tests
	cd ${TARTMP}/hugs98/src/unix; autoconf; autoheader
	mv ${TARTMP}/hugs98 ${TARTMP}/${PACKAGE}
	cd ${TARTMP}; tar cf /tmp/${NAME}.tar ${PACKAGE}
	gzip -9 /tmp/${NAME}.tar
	mv /tmp/${NAME}.tar.gz ${PACKAGE}.tar.gz

rpm-dirs:
	-mkdir ${RPMTMP}
	-mkdir ${RPMTMP}/BUILD
	-mkdir ${RPMTMP}/RPMS
	-mkdir ${RPMTMP}/RPMS/i386
	-mkdir ${RPMTMP}/SOURCES
	-mkdir ${RPMTMP}/SPECS
	-mkdir ${RPMTMP}/SRPMS

rpm: tar rpm-dirs
	cp ${PACKAGE}.tar.gz ${RPMTMP}/SOURCES
	rpm ${RPMDEFS} -ba ${SPECFILE}
	mv ${RPMTMP}/RPMS/i386/${PACKAGE}-${RELEASE}.i386.rpm .
	mv ${RPMTMP}/SRPMS/${PACKAGE}-${RELEASE}.src.rpm .

clean:
	-cd src; if test -f Makefile; then make veryclean; fi
	-cd docs; if test -f Makefile; then make veryclean; fi
	-rm -f ${PACKAGE}.tar.gz
	-rm -f ${PACKAGE}-*.rpm
