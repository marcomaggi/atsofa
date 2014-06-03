dnl @configure_input@
dnl

AC_PREREQ([2.69])
AC_INIT([SOFA under GNU Autotools],[0.1d7],
  [marco.maggi-ipsu@poste.it],
  [atsofa],[http://github.com/marcomaggi/atsofa/])
AC_CONFIG_SRCDIR([c/])
AC_CONFIG_MACRO_DIR([meta/autotools])
AC_CONFIG_AUX_DIR([meta/autotools])
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET
AM_INIT_AUTOMAKE([foreign dist-bzip2 no-dist-gzip])
AM_MAINTAINER_MODE

AM_PROG_AR
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_MKDIR_P

LT_PREREQ([2.4])
LT_INIT

dnl page
#### libraries interface version

dnl This is the version stored in the pkg-config data file.
AC_SUBST([MY_PKG_CONFIG_VERSION],[0.1])

MM_LIBTOOL_LIBRARY_VERSIONS([sofac],1,0,0)
MM_LIBTOOL_LIBRARY_VERSIONS([sofaf],1,0,0)

dnl page
dnl Options

AC_MSG_CHECKING([whether to enable building the C library])
AC_ARG_ENABLE([clib],
  [AS_HELP_STRING([--enable-clib],
    [enable building the C library (default: yes)])],
  [AS_IF([test x$enableval = xyes],
         [AS_VAR_SET([atsofa_clib],[yes])],
         [test x$enableval = xno],
         [AS_VAR_SET([atsofa_clib],[no])])],
  [AS_VAR_SET([atsofa_clib],[yes])])
AC_MSG_RESULT([$atsofa_clib])
AM_CONDITIONAL([WANT_CLIB],[test x$atsofa_clib = xyes])

AC_MSG_CHECKING([whether to enable building the Fortran 77 library])
AC_ARG_ENABLE([flib],
  [AS_HELP_STRING([--enable-flib],
    [enable building the Fortran 77 library (default: yes)])],
  [AS_IF([test x$enableval = xyes],
         [AS_VAR_SET([atsofa_flib],[yes])],
         [test x$enableval = xno],
         [AS_VAR_SET([atsofa_flib],[no])])],
  [AS_VAR_SET([atsofa_flib],[yes])])
AC_MSG_RESULT([$atsofa_flib])
AM_CONDITIONAL([WANT_FLIB],[test x$atsofa_flib = xyes])

dnl page
dnl Checks for programs.

AC_LANG([Fortran 77])
AC_PROG_F77
AC_PROG_F77_C_O
AC_F77_LIBRARY_LDFLAGS
AC_F77_WRAPPERS

AC_LANG([C])
AC_PROG_CC
AM_PROG_CC_C_O
AC_PROG_CC_STDC

dnl Checks for header files.
AC_HEADER_STDC

dnl Checks for libraries.
AC_CHECK_LIB([m],[sin])

AC_CONFIG_FILES([Makefile]
  [c/Makefile]
  [f77/Makefile]
  [tests-c/Makefile]
  [tests-f77/Makefile]
  [doc/Makefile]
  [meta/sofac.pc]
  [meta/sofaf.pc])
AC_OUTPUT

dnl end of file
# Local Variables:
# mode: autoconf
# page-delimiter: "^dnl page"
# End:
