ROOT := ${subst /src/unix,,${shell pwd}}
OPTS =  --enable-ffi \
	--with-fptools=../../fptools
#	--disable-large-banner
GNULIBS = 

config: configure
	$(RM) -r config.cache autom4te.cache
	LIBS=${GNULIBS} hugsdir=${ROOT} ./configure ${OPTS} --datadir=${ROOT}

configure: configure.in
	-autoconf
	-autoheader
