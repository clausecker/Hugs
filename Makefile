# this Makefile is for building RPM distributions, and is, not surprisingly,
# somewhat redhat specific.

RELEASE=hugs98-Nov1999

all:
	@echo "Make what?"

# unless you've just unpacked, `make clean' first
tar: ${RELEASE}.tar.gz

${RELEASE}.tar.gz:
	# using `make parser.c' would be best, but by default yacc
	# will be used, and yacc is for some reason incompatible
	# cd src; make parser.c
	cd src; bison -y parser.y; mv y.tab.c parser.c
	cd src/unix; autoconf; autoheader
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
	mv /tmp/rpm/RPMS/i386/${RELEASE}-1.i386.rpm .
	mv /tmp/rpm/SRPMS/${RELEASE}-1.src.rpm .

clean:
	-cd src; make veryclean
	-rm ${RELEASE}.tar.gz
	-rm ${RELEASE}-*.rpm
