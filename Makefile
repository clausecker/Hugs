# this Makefile is for building RPM distributions, and is, not surprisingly,
# somewhat redhat specific (and rpm version 3 specific - i.e. RedHat 6.0
# or greater).

NAME=$(shell egrep ^Name: hugs98.spec  | colrm 1 14)
VERSION=$(shell egrep ^Version: hugs98.spec  | colrm 1 14)
RELEASE=${NAME}-${VERSION}
PATCHLEVEL=$(shell egrep ^Release: hugs98.spec  | colrm 1 14)

# TAG=Dec2001
# HSLIBSTAG=hugs-Dec2001
TAG=HEAD
HSLIBSTAG=HEAD

all:
	@echo "Make what?"

# unless you've just unpacked, `make clean' first
tar: ${RELEASE}.tar.gz

CVSROOT = ${shell cat CVS/Root}

${RELEASE}.tar.gz:
	-rm -rf /tmp/mktar
	-mkdir -p /tmp/mktar
	cd /tmp/mktar; cvs -d ${CVSROOT} export -r${TAG} hugs98
	cd /tmp/mktar; cvs -d ${CVSROOT} export -r${HSLIBSTAG} fptools/hslibs
	# using `make parser.c' would be best, but by default yacc
	# will be used, and yacc is, for some reason, incompatible
	cp /tmp/mktar/hugs98/src/version.h /tmp/mktar
	cd /tmp/mktar/hugs98/src; sed -e s/YYMMDD/`date +"%d%m%y"`/ < /tmp/mktar/version.h > /tmp/mktar/hugs98/src/version.h
	cd /tmp/mktar/hugs98/src; bison -y parser.y; mv y.tab.c parser.c
	cd /tmp/mktar/hugs98/src/unix; ./convert_hslibs /tmp/mktar/fptools
	# Siggy deren't like these in distros
	cd /tmp/mktar/hugs98; rm -rf tests
	cd /tmp/mktar/hugs98/src/unix; autoconf; autoheader
	mv /tmp/mktar/hugs98 /tmp/mktar/${RELEASE}
	cd /tmp/mktar; tar cf /tmp/hugs98.tar ${RELEASE}
	gzip -9 /tmp/hugs98.tar
	mv /tmp/hugs98.tar.gz ${RELEASE}.tar.gz

rpm-dirs:
	-mkdir /tmp/rpm
	-mkdir /tmp/rpm/BUILD
	-mkdir /tmp/rpm/RPMS
	-mkdir /tmp/rpm/RPMS/i386
	-mkdir /tmp/rpm/SOURCES
	-mkdir /tmp/rpm/SPECS
	-mkdir /tmp/rpm/SRPMS

rpm: tar rpm-dirs
	cp ${RELEASE}.tar.gz /tmp/rpm/SOURCES
	rpm --define '_topdir /tmp/rpm' -ba hugs98.spec
	mv /tmp/rpm/RPMS/i386/${RELEASE}-${PATCHLEVEL}.i386.rpm .
	mv /tmp/rpm/SRPMS/${RELEASE}-${PATCHLEVEL}.src.rpm .

clean:
	-cd src; if test -f Makefile; then make veryclean; fi
	-cd docs; if test -f Makefile; then make veryclean; fi
	-rm -f ${RELEASE}.tar.gz
	-rm -f ${RELEASE}-*.rpm
