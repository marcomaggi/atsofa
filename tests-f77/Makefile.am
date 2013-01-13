## Process this file with automake to produce Makefile.in

#AM_CPPFLAGS	= -I$(top_srcdir)/f77

if WANT_FLIB
check_PROGRAMS	= \
	cal2jd
endif

LDADD		= ../f77/libsofaf.la
TESTS		= $(check_PROGRAMS)
EXTRA_DIST	= $(check_PROGRAMS)

# cal2jd$(EXEEXE): cal2jd.$(OBJEXE)
# 	$(F77) -o $@ $(AM_FFLAGS) $(FFLAGS) $^

# cal2jd$(OBJEXE): cal2jd.f
# 	$(F77) -o $@ -c $(AM_FFLAGS) $(FFLAGS) $^

### end of file
