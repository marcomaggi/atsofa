dnl @configure_input@
dnl

AC_PREREQ([2.69])
AC_INIT([SOFA under GNU Autotools],[0.1d0],
  [marco.maggi-ipsu@poste.it],
  [atsofa],[http://github.com/marcomaggi/atsofa/])
AC_CONFIG_SRCDIR([c/])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_AUX_DIR([m4])
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET
AM_INIT_AUTOMAKE([foreign dist-bzip2 no-dist-gzip])
LT_PREREQ([2.2])
LT_INIT
AM_MAINTAINER_MODE

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

AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET

dnl Checks for header files.
AC_HEADER_STDC

dnl Checks for libraries.
AC_CHECK_LIB([m],[sin])

AC_CONFIG_FILES([Makefile c/Makefile f77/Makefile tests/Makefile])
AC_OUTPUT

dnl end of file
