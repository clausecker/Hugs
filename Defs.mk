# General settings
# (this file should work with any make)

NAME=hugs98

#
# Set to 0 if a snapshot release.
#
MAJOR_RELEASE=0

# Release number of RPM.
RELEASE=1

# TAG=Dec2001
# HSLIBSTAG=hugs-Dec2001
TAG=HEAD
HSLIBSTAG=HEAD
LIBRARIESTAG=HEAD

HSLIBSDIRS = concurrent data hssource lang net text util posix
LIBRARIESDIRS = base haskell98 haskell-src network parsec QuickCheck unix \
	GLUT OpenGL fgl
