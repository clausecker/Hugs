# this Makefile is for building RPM distributions, and is, not surprisingly,
# somewhat redhat specific (and rpm version 3 specific - i.e. RedHat 6.0
# or greater).

RPMTMP = /tmp/rpm
# probably should be uniqueified
TARTMP = /tmp/mktar

SPECFILE=${NAME}.spec

RPMDEFS = --define "_topdir ${RPMTMP}" \
	  --define "name ${NAME}" \
	  --define "version ${VERSION}" \
	  --define "release ${RELEASE}" \

# unless you've just unpacked, `make clean' first
tar: ${PACKAGE}.tar.gz

${PACKAGE}.tar.gz:
	-rm -rf ${TARTMP}
	-mkdir -p ${TARTMP}
	cd ${TARTMP}; cvs -d ${CVSROOT} export -r${TAG} hugs98
	cd ${TARTMP}/hugs98; cvs -d ${CVSROOT} export -r${HSLIBSTAG} $(addprefix fptools/hslibs/,${HSLIBSDIRS})
	cd ${TARTMP}/hugs98; cvs -d ${CVSROOT} export -r${LIBRARIESTAG} $(addprefix fptools/libraries/,${LIBRARIESDIRS})
	cd ${TARTMP}/hugs98/fptools/libraries/haskell-src/Language/Haskell; happy Parser.ly
	# using `make parser.c' would be best, but by default yacc
	# will be used, and yacc is, for some reason, incompatible
	cp ${TARTMP}/hugs98/src/version.h.in /tmp/mktar
	cd ${TARTMP}/hugs98/src; sed -e "s/MONTH_YEAR/${MONTH_YEAR}/" -e "s/YYYYMMDD/${YEAR_MONTH_DAY}/" < ${TARTMP}/version.h.in > ${TARTMP}/hugs98/src/version.h.in
	cd ${TARTMP}/hugs98/src; bison -y parser.y; mv y.tab.c parser.c
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
