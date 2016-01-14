pkglib_LTLIBRARIES = libcalculator.la

AM_CPPFLAGS = \
	-w \
	-DGETTEXT_PACKAGE=\"$(GETTEXT_PACKAGE)\" \
	$(LIBCALCULATOR_CFLAGS)

libcalculator_la_SOURCES = \
	mpfr.vapi \
	mpfr-glue.vala \
	mpc.vapi \
	currency.vala \
	equation.vala \
	equation-lexer.vala \
	equation-parser.vala \
	financial.vala \
	function-manager.vala \
	math-equation.vala \
	math-function.vala \
	math-variables.vala \
	number.vala \
	serializer.vala \
	unit.vala

libcalculator_la_VALAFLAGS = \
	--target-glib $(GLIB_REQUIRED) \
	--pkg posix \
	--pkg gtksourceview-3.0 \
	--pkg libxml-2.0 \
	--pkg libsoup-2.4 \
	--header=libcalculator.h \
	--vapi=libcalculator.vapi

libcalculator_la_LDFLAGS = -avoid-version -no-undefined

libcalculator_la_LIBADD = \
	$(LIBCALCULATOR_LIBS) \
	-lgmp \
	-lm \
	-lmpfr \
	-lmpc

EXTRA_DIST = \
	libcalculator.h \
	libcalculator.vapi

MAINTAINERCLEANFILES = \
	libcalculator.h \
	libcalculator.vapi

-include $(top_srcdir)/git.mk
