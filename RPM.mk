# A (GNU) Makefile for building source and RPM distributions.

include Defs.mk

MONTH_YEAR = ${shell date +"%B %Y"}
MON_YEAR = ${shell date +"%b%Y"}
YEAR_MONTH_DAY = ${shell date +"%Y%m%d"}

# convention: a release uses the MON_YEAR form of version,
# while a snapshot uses the YEAR_MONTH_DAY form.
# this should be sync'd with src/version.c
ifeq "$(MAJOR_RELEASE)" "1"
VERSION=${MON_YEAR}
else
VERSION=${YEAR_MONTH_DAY}
endif

PACKAGE=${NAME}-${VERSION}

# Starting with Red Hat 8.0, the build functionality was removed from rpm, so
# one has to use rpmbuild instead. SuSE didn't follow this change, so there is
# no rpmbuild on SuSE and rpm still does the job. I like such changes...
RPMBUILD = rpmbuild

TMP    = /tmp
RPMTMP = ${TMP}/rpm
# probably should be uniqueified
TARTMP = ${TMP}/mktar

SPECFILE=${NAME}.spec

RPMDEFS = --define "_topdir ${RPMTMP}" \
	  --define "name ${NAME}" \
	  --define "version ${VERSION}" \
	  --define "release ${RELEASE}" \

VERSION_SUBSTS = \
    -e "s/define MAJOR_RELEASE.*/define MAJOR_RELEASE ${MAJOR_RELEASE}/" \
    -e "s/VERSION_STRING MONTH_YEAR/VERSION_STRING \"${MONTH_YEAR}\"/" \
    -e "s/VERSION_STRING YYYYMMDD/VERSION_STRING \"${YEAR_MONTH_DAY}\"/"

RC_STRING = RC1
RC_VERSION_SUBSTS = \
    -e "s/define MAJOR_RELEASE.*/define MAJOR_RELEASE ${MAJOR_RELEASE}/" \
    -e "s/VERSION_STRING MONTH_YEAR/VERSION_STRING \"${MON_YEAR} ${RC_STRING}\"/"

tar: ${PACKAGE}.tar.gz

# Utilities needed to pre-process fptools. To override
# these, set them on the command-line when invoking 'make':
#
#  foo$ make FIND=/usr/bin/find HAPPY=c:/happy/happy-1.15/bin/happy ...
#
# (You'll find 'hsc2hs' included in a GHC distribution.)
#
FIND=find
HAPPY=happy
HSC2HS=hsc2hs

${PACKAGE}.tar.gz:
	-rm -rf ${TARTMP}
	-mkdir -p ${TARTMP}
# Note: The following line will not work correctly for "make -C blah".
	CVSROOT=`cat CVS/Root`; export CVSROOT; \
	  cd ${TARTMP}; \
	  cvs export -r ${TAG} hugs98; \
	  cd hugs98; \
	  cvs export -r ${HSLIBSTAG} `for lib in $(HSLIBSDIRS); do echo fptools/hslibs/$$lib; done`; \
	  cvs export -r ${LIBRARIESTAG} `for lib in $(LIBRARIESDIRS) aclocal.m4 configure.ac libraries-footer.txt; do echo fptools/libraries/$$lib; done`
# Unused, and the pathnames in there are too long for portable tar
	cd ${TARTMP}/hugs98; rm -rf fptools/libraries/parsec/examples
# preprocess these, so the package can be built without happy & ghc
	$(FIND) ${TARTMP}/hugs98/fptools/libraries -name "*.ly" -o -name "*.y" | \
		xargs -l $(HAPPY)
	if test x"$(USING_AN_OLDER_HSC2HS)" != x"YES"; then \
	  $(FIND) ${TARTMP}/hugs98/fptools/libraries -name "*.hsc" | \
	    xargs -l $(HSC2HS) --no-compile --template=template-hsc.h; \
	else \
	  $(FIND) ${TARTMP}/hugs98/fptools/libraries -name "*.hsc" | \
	    xargs -l $(HSC2HS) --no-compile; \
	  $(FIND) ${TARTMP}/hugs98/fptools/libraries -name "*_hsc_make.c" | \
	    xargs libraries/tools/hsc_kludge; \
	fi
	cp ${TARTMP}/hugs98/src/version.c ${TARTMP}
	cd ${TARTMP}/hugs98/src; sed ${VERSION_SUBSTS} < ${TARTMP}/version.c > ${TARTMP}/hugs98/src/version.c
# using `make parser.c' would be best, but by default yacc
# will be used, and yacc is, for some reason, incompatible
	cd ${TARTMP}/hugs98/src; bison -y parser.y; mv y.tab.c parser.c
# Siggy deren't like these in distros
	cd ${TARTMP}/hugs98; rm -rf tests
	cd ${TARTMP}/hugs98; autoreconf
	echo timestamp for config.h.in >${TARTMP}/hugs98/src/stamp-h.in
	mv ${TARTMP}/hugs98 ${TARTMP}/${PACKAGE}
	cd ${TARTMP}; tar cf ${TMP}/${NAME}.tar ${PACKAGE}
	gzip -9 ${TMP}/${NAME}.tar
	mv ${TMP}/${NAME}.tar.gz ${PACKAGE}.tar.gz

rpm-dirs:
	-mkdir ${RPMTMP}
	-mkdir ${RPMTMP}/BUILD
	-mkdir ${RPMTMP}/RPMS
	-mkdir ${RPMTMP}/SOURCES
	-mkdir ${RPMTMP}/SPECS
	-mkdir ${RPMTMP}/SRPMS

rpm: tar rpm-dirs
	cp ${PACKAGE}.tar.gz ${RPMTMP}/SOURCES
	${RPMBUILD} ${RPMDEFS} -ba ${SPECFILE}
	mv ${RPMTMP}/RPMS/i?86/${PACKAGE}-${RELEASE}.i?86.rpm .
	mv ${RPMTMP}/SRPMS/${PACKAGE}-${RELEASE}.src.rpm .

rc-rpm:
	${MAKE} VERSION_SUBSTS='${RC_VERSION_SUBSTS}' rpm
