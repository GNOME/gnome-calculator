dnl Process this file with autoconf to produce a configure script.

AC_INIT(gnome-calculator, 3.16.0)
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([1.11 no-dist-gzip dist-xz foreign])
AM_SILENT_RULES([yes])
AM_MAINTAINER_MODE
GNOME_MAINTAINER_MODE_DEFINES

AM_PROG_VALAC([0.24])
AM_PROG_CC_C_O

LT_PREREQ([2.2])
LT_INIT

AC_PATH_PROG(GLIB_COMPILE_RESOURCES, glib-compile-resources)

GLIB_GSETTINGS

dnl ###########################################################################
dnl Dependencies
dnl ###########################################################################

GLIB_REQUIRED=2.40.0
GTK_REQUIRED=3.11.6
GTKSOURCEVIEW_REQUIRED=3.15.1

# FIXME We link to it too, so this check needs to be better....
AC_CHECK_HEADER([mpfr.h], [], [AC_MSG_ERROR([The mpfr header is missing. Please, install mpfr])])

PKG_CHECK_MODULES(LIBCALCULATOR, [
    glib-2.0 >= $GLIB_REQUIRED
    gio-2.0 >= $GLIB_REQUIRED
    gtksourceview-3.0 >= $GTKSOURCEVIEW_REQUIRED
    libxml-2.0
])

PKG_CHECK_MODULES(GNOME_CALCULATOR, [
    gtk+-3.0 >= $GTK_REQUIRED
    gmodule-export-2.0
])

PKG_CHECK_MODULES(SEARCH_PROVIDER, [
    glib-2.0 >= $GLIB_REQUIRED
    gio-2.0 >= $GLIB_REQUIRED
])

dnl ###########################################################################
dnl Internationalization
dnl ###########################################################################

IT_PROG_INTLTOOL([0.50])
AC_SUBST(GETTEXT_PACKAGE, gnome-calculator)

dnl ###########################################################################
dnl Documentation
dnl ###########################################################################

YELP_HELP_INIT

dnl ###########################################################################
dnl Installed tests
dnl ###########################################################################
BEHAVE_INSTALLED_TESTS

dnl ###########################################################################
dnl Files to generate
dnl ###########################################################################

AC_OUTPUT([
Makefile
search-provider/Makefile
lib/Makefile
src/Makefile
po/Makefile.in
data/Makefile
help/Makefile
tests/Makefile
])
