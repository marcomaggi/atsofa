dnl @configure_input@
dnl

AC_PREREQ([2.69])
AC_INIT([SOFA under GNU Autotools],[0.1d2],
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

dnl Options
AC_MSG_CHECKING([whether to enable building the C library])
AC_ARG_ENABLE([clib],
  [AS_HELP_STRING([--enable-clib],[enable building the C library (default: yes)])],
  [if test x$withval = xyes
   then atsofa_clib=yes
   else atsofa_clib=no
   fi],
  [atsofa_clib=yes])
AC_MSG_RESULT([$atsofa_clib])
AM_CONDITIONAL([WANT_CLIB],[test x$atsofa_clib = xyes])

AC_MSG_CHECKING([whether to enable building the Fortran 77 library])
AC_ARG_ENABLE([flib],
  [AS_HELP_STRING([--enable-flib],[enable building the Fortran 77 library (default: yes)])],
  [if test x$withval = xyes
   then atsofa_flib=yes
   else atsofa_flib=no
   fi],
  [atsofa_flib=yes])
AC_MSG_RESULT([$atsofa_flib])
AM_CONDITIONAL([WANT_FLIB],[test x$atsofa_flib = xyes])

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

AC_CONFIG_FILES([Makefile c/Makefile f77/Makefile tests-c/Makefile tests-f77/Makefile])
AC_OUTPUT

dnl end of file
