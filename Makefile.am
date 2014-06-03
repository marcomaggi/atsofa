## Process this file with automake to produce Makefile.in

ACLOCAL_MFLAGS		= -I meta/autotools
SUBDIRS			= c f77
EXTRA_DIST		= INSTALL
dist_doc_DATA		= README COPYING

AM_CPPFLAGS		= -I$(srcdir)/c
AM_CFLAGS		= -Wall
AM_FFLAGS		= -Wall

## --------------------------------------------------------------------

pkgconfigdir		= $(libdir)/pkgconfig
nodist_pkgconfig_DATA	= meta/sofaf.pc meta/sofac.pc

## --------------------------------------------------------------------

AM_MAKEINFOFLAGS	= --no-split

info_TEXINFOS		= doc/iausofa.texi
doc_iausofa_TEXINFOS	= doc/macros.texi

EXTRA_DIST		+=	\
	doc/board.lis		doc/board.pdf			\
	doc/changes.lis		doc/changes.pdf			\
	doc/consts.lis		doc/consts.pdf			\
	doc/contents.lis	doc/contents.pdf		\
	doc/copyr.lis		doc/copyr.pdf			\
	doc/intro.lis		doc/intro.pdf			\
	doc/manual.lis		doc/manual.pdf			\
	doc/sofa_ast_c.pdf	doc/sofa_ast_summary.pdf	\
	doc/sofa_lib.lis	doc/sofa_lib.pdf		\
	doc/sofa_pn.pdf		doc/sofa_ts_c.pdf		\
	doc/sofa_vml.lis	doc/sofa_vml.pdf		\
	doc/title.lis		doc/title.pdf

## --------------------------------------------------------------------

check_PROGRAMS		=
if WANT_CLIB
check_PROGRAMS		+= \
	tests-c/iaucal2jd		\
	tests-c/t_sofa_c
endif
if WANT_FLIB
check_PROGRAMS		+= \
	tests-f77/cal2jd		\
	tests-f77/t_sofa_f
endif

TESTS				= $(check_PROGRAMS)

tests_c_iaucal2jd_LDADD		= c/libsofac.la
tests_c_t_sofa_c_LDADD		= c/libsofac.la

tests_f77_cal2jd_SOURCES	= tests-f77/cal2jd.f
tests_f77_cal2jd_LDADD		= f77/libsofaf.la

tests_f77_t_sofa_f_SOURCES	= tests-f77/t_sofa_f.for
tests_f77_t_sofa_f_LDADD	= f77/libsofaf.la

## --------------------------------------------------------------------

installcheck-local: $(TESTS)
	@for f in $(TESTS); do $(builddir)/$$f; done

### end of file
