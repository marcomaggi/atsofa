## Process this file with automake to produce Makefile.in

AM_CPPFLAGS	= -I$(top_srcdir)/c
AM_CFLAGS	= -Wall

if WANT_CLIB
check_PROGRAMS	= iaucal2jd t_sofa_c
endif

LDADD		= ../c/libsofac.la
TESTS		= $(check_PROGRAMS)
EXTRA_DIST	= $(check_PROGRAMS)

### end of file
