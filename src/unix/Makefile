OPTS =
#	--disable-large-banner
GNULIBS = 

config: configure ../config.h.in
	$(RM) -r config.cache autom4te.cache
	LIBS=${GNULIBS} ./configure ${OPTS}

configure: configure.in
	-autoconf

../config.h.in: configure.in
	-autoheader
