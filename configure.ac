dnl Process this file with autoconf to produce a configure script.

AC_INIT(configure.ac)
AM_INIT_AUTOMAKE(gcalctool, 6.4.1.1)
AM_CONFIG_HEADER(config.h)
AM_MAINTAINER_MODE
GNOME_MAINTAINER_MODE_DEFINES
m4_ifdef([AM_SILENT_RULES],[AM_SILENT_RULES([yes])])

AC_ISC_POSIX
AC_PROG_CC
AM_PROG_CC_STDC
AC_HEADER_STDC

GLIB_GSETTINGS
GNOME_COMPILE_WARNINGS(maximum)

dnl ###########################################################################
dnl Dependencies
dnl ###########################################################################

GLIB_REQUIRED=2.31
GIO_REQUIRED=2.25.10
GTK_REQUIRED=3.0

PKG_CHECK_MODULES(GCALCTOOL, [
    gtk+-3.0 >= $GTK_REQUIRED
    glib-2.0 >= $GLIB_REQUIRED
    gio-2.0 >= $GIO_REQUIRED
    libxml-2.0
    gmodule-export-2.0
])

PKG_CHECK_MODULES(GCALCCMD, [
    glib-2.0 >= $GLIB_REQUIRED
    gio-2.0 >= $GIO_REQUIRED
    libxml-2.0
])

GLIB_MKENUMS=`$PKG_CONFIG --variable=glib_mkenums glib-2.0`
AC_SUBST(GLIB_MKENUMS)

AC_CHECK_LIB(m, log)

dnl ###########################################################################
dnl Determine if a usable lex is available on this system
dnl ###########################################################################

AM_PROG_LEX
if [[ "$LEX" != "flex" ]]; then
	AC_MSG_ERROR(flex is required to create the gcalctool scanners)
fi

dnl ###########################################################################
dnl Determine if a usable yacc is available on this system
dnl ###########################################################################

AC_PROG_YACC
AC_CHECK_PROG(HAVE_YACC, $YACC, yes, no)
if [[ "$HAVE_YACC" = "no" ]]; then
	AC_MSG_ERROR($YACC is not usable as yacc - consider using bison)
fi

dnl ###########################################################################
dnl Internationalization
dnl ###########################################################################

IT_PROG_INTLTOOL([0.35.0])
AC_SUBST(GETTEXT_PACKAGE, gcalctool)

dnl ###########################################################################
dnl Documentation
dnl ###########################################################################

YELP_HELP_INIT

dnl ###########################################################################
dnl Files to generate
dnl ###########################################################################

AC_OUTPUT([
Makefile
src/Makefile
po/Makefile.in
data/Makefile
help/Makefile
])
