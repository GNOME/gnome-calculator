bin_PROGRAMS = gnome-calculator gcalccmd
noinst_PROGRAMS = test-number test-equation test-serializer

TESTS = test-number test-equation test-serializer

AM_CPPFLAGS = \
	-DUI_DIR=\""$(datadir)/gnome-calculator"\" \
	-DVERSION=\""$(VERSION)"\" \
	-DLOCALE_DIR=\""$(localedir)"\" \
	-DGETTEXT_PACKAGE=\"$(GETTEXT_PACKAGE)\" \
	-w \
	$(GNOME_CALCULATOR_CFLAGS)

BUILT_SOURCES = resources.c

resources.c: $(top_srcdir)/data/gnome-calculator.gresource.xml $(shell $(GLIB_COMPILE_RESOURCES) --generate-dependencies --sourcedir=$(top_srcdir)/data/ $(top_srcdir)/data/gnome-calculator.gresource.xml)
	$(AM_V_GEN) $(GLIB_COMPILE_RESOURCES) --target=$@ --sourcedir=$(top_srcdir)/data/ --generate-source $<

gnome_calculator_SOURCES = \
	config.vapi \
	gnome-calculator.vala \
	currency.vala \
	equation.vala \
	equation-lexer.vala \
	equation-parser.vala \
	financial.vala \
	math-buttons.vala \
	math-converter.vala \
	math-display.vala \
	math-equation.vala \
	math-preferences.vala \
	math-variables.vala \
	math-variable-popup.vala \
	math-window.vala \
	number.vala \
	serializer.vala \
	unit.vala \
	math-function.vala \
	function-manager.vala \
	math-function-popup.vala \
	$(BUILT_SOURCES)

gnome_calculator_VALAFLAGS = \
	--target-glib 2.32 \
	--pkg posix \
	--pkg gtk+-3.0 \
	--pkg gtksourceview-3.0 \
	--pkg libxml-2.0

gnome_calculator_LDADD = \
	$(GNOME_CALCULATOR_LIBS) \
	-lm

gcalccmd_SOURCES = \
	config.vapi \
	gcalccmd.vala \
	currency.vala \
	equation.vala \
	equation-lexer.vala \
	equation-parser.vala \
	number.vala \
	serializer.vala \
	unit.vala \
	math-function.vala \
	function-manager.vala

gcalccmd_VALAFLAGS = \
	--target-glib 2.32 \
	--pkg posix \
	--pkg gio-2.0 \
	--pkg libxml-2.0

gcalccmd_LDADD = \
	$(GCALCCMD_LIBS) \
	-lm

test_number_SOURCES = \
	config.vapi \
	test-number.vala \
	number.vala \
	serializer.vala

test_number_VALAFLAGS = \
	--target-glib 2.32 \
	--pkg posix \
	--pkg gio-2.0 \
	--pkg libxml-2.0

test_number_LDADD = \
	$(GCALCCMD_LIBS) \
	-lm

test_equation_SOURCES = \
	config.vapi \
	test-equation.vala \
	currency.vala \
	equation.vala \
	equation-lexer.vala \
	equation-parser.vala \
	number.vala \
	serializer.vala \
	unit.vala \
	math-function.vala \
	function-manager.vala

test_equation_VALAFLAGS = \
	--target-glib 2.32 \
	--pkg posix \
	--pkg gio-2.0 \
	--pkg libxml-2.0

test_equation_LDADD = \
	$(GCALCCMD_LIBS) \
	-lm

test_serializer_SOURCES = \
	config.vapi \
	test-serializer.vala \
	number.vala \
	serializer.vala

test_serializer_VALAFLAGS = \
	--target-glib 2.32 \
	--pkg posix \
	--pkg gio-2.0 \
	--pkg libxml-2.0

test_serializer_LDADD = \
	$(GCALCCMD_LIBS) \
	-lm

-include $(top_srcdir)/git.mk
