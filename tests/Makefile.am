noinst_PROGRAMS = test-number test-equation test-serializer
TESTS = test-number test-equation test-serializer

AM_CPPFLAGS = \
	-w \
	-I$(top_srcdir)/lib \
	$(LIBCALCULATOR_CFLAGS)

test_number_SOURCES = \
	test-number.vala

test_number_VALAFLAGS = \
	--target-glib $(GLIB_REQUIRED) \
	--pkg posix \
	--pkg gio-2.0 \
	--pkg gtksourceview-3.0 \
	--pkg libxml-2.0 \
	$(top_builddir)/lib/libcalculator.vapi \
	$(top_builddir)/lib/mpfr.vapi

test_number_LDADD = \
	$(LIBCALCULATOR_LIBS) \
	-lm \
	$(top_builddir)/lib/libcalculator.la

test_equation_SOURCES = \
	test-equation.vala

test_equation_VALAFLAGS = \
	--target-glib $(GLIB_REQUIRED) \
	--pkg posix \
	--pkg gio-2.0 \
	--pkg gtksourceview-3.0 \
	--pkg libxml-2.0 \
	$(top_builddir)/lib/libcalculator.vapi \
	$(top_builddir)/lib/mpfr.vapi

test_equation_LDADD = \
	$(LIBCALCULATOR_LIBS) \
	-lm \
	$(top_builddir)/lib/libcalculator.la

test_serializer_SOURCES = \
	test-serializer.vala

test_serializer_VALAFLAGS = \
	--target-glib $(GLIB_REQUIRED) \
	--pkg posix \
	--pkg gio-2.0 \
	--pkg gtksourceview-3.0 \
	--pkg libxml-2.0 \
	$(top_builddir)/lib/libcalculator.vapi \
	$(top_builddir)/lib/mpfr.vapi

test_serializer_LDADD = \
	$(LIBCALCULATOR_LIBS) \
	-lm \
	$(top_builddir)/lib/libcalculator.la

EXTRA_DIST = \
	steps \
	common_steps.py \
	environment.py \
	advanced_mode.feature \
	basic_mode.feature \
	financial_mode.feature \
	programming_mode.feature

-include $(top_srcdir)/git.mk
