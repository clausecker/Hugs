#
#
#

SRC_CONFIGURE_OPTS =
#	--disable-large-banner

CONFIGURE_OPTS = $(EXTRA_CONFIGURE_OPTS)  $(SRC_CONFIGURE_OPTS)
GNULIBS = 

config: configure ../config.h.in
	$(RM) -r config.cache autom4te.cache
	LIBS=${GNULIBS} ./configure ${CONFIGURE_OPTS}

configure: configure.in
	-autoconf

../config.h.in: configure.in
	-autoheader
