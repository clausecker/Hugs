# this Makefile is for building RPM distributions, and is, not surprisingly,
# somewhat redhat specific (and rpm version 3 specific - i.e. RedHat 6.0
# or greater).

NAME=$(shell egrep ^Name: hugs98.spec  | colrm 1 14)
VERSION=$(shell egrep ^Version: hugs98.spec  | colrm 1 14)
RELEASE=${NAME}-${VERSION}
PATCHLEVEL=$(shell egrep ^Release: hugs98.spec  | colrm 1 14)

all:
	@echo "Make what?"

# unless you've just unpacked, `make clean' first
tar: ${RELEASE}.tar.gz

${RELEASE}.tar.gz:
	# using `make parser.c' would be best, but by default yacc
	# will be used, and yacc is, for some reason, incompatible
	# cd src; make parser.c
	cd src; bison -y parser.y; mv y.tab.c parser.c
	cd src/unix; autoconf; autoheader
	rm -rf /tmp/${RELEASE}
	mkdir /tmp/${RELEASE}
	tar cf - `find . ! -type d -print | egrep -v CVS` | (cd /tmp/${RELEASE}; tar xfBp -)
	cd /tmp; tar cf /tmp/hugs98.tar ${RELEASE}
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
