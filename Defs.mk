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

PACKAGE=${NAME}-${VERSION}

# TAG=Dec2001
# HSLIBSTAG=hugs-Dec2001
TAG=HEAD
HSLIBSTAG=HEAD

HSLIBSDIRS=hslibs libraries/base libraries/haskell98 libraries/haskell-src

CVSROOT = ${shell cat CVS/Root}
