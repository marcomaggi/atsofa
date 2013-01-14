## Process this file with automake to produce Makefile.in

AM_FFLAGS	= -Wall

if WANT_FLIB
check_PROGRAMS	= \
	cal2jd
endif

cal2jd_SOURCES	= cal2jd.f

LDADD		= ../f77/libsofaf.la
TESTS		= $(check_PROGRAMS)
EXTRA_DIST	= $(check_PROGRAMS)

### end of file
