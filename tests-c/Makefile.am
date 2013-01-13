## Process this file with automake to produce Makefile.in

AM_CPPFLAGS	= -I$(top_srcdir)/c

if WANT_CLIB
check_PROGRAMS	= \
	cal2jd
endif

LDADD		= ../c/libsofac.la
TESTS		= $(check_PROGRAMS)
EXTRA_DIST	= $(check_PROGRAMS)

### end of file
