ROOT := ${subst /src/unix,,${shell pwd}}
OPTS =  \
	--with-readline \
	--disable-large-banner
GNULIBS = 

config: configure
	-rm config.cache
	LIBS=${GNULIBS} hugsdir=${ROOT} ./configure ${OPTS} --datadir=${ROOT}

configure: configure.in
	-autoconf
	-autoheader
