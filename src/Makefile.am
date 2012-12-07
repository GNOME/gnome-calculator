bin_PROGRAMS = gnome-calculator gcalccmd
noinst_PROGRAMS = test-number test-equation test-serializer

TESTS = test-number test-equation

INCLUDES = \
	-DUI_DIR=\""$(datadir)/gnome-calculator"\" \
	-DVERSION=\""$(VERSION)"\" \
	-DLOCALE_DIR=\""$(localedir)"\" \
	-DGETTEXT_PACKAGE=\"$(GETTEXT_PACKAGE)\" \
	$(WARN_CFLAGS) \
	$(GNOME_CALCULATOR_CFLAGS)

gnome_calculator_SOURCES = \
	config.vapi \
	fixes.vapi \
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
  	unit.vala

gnome_calculator_VALAFLAGS = \
	--target-glib 2.32 \
	--pkg posix \
	--pkg gtk+-3.0 \
	--pkg libxml-2.0

gnome_calculator_LDADD = \
	$(GNOME_CALCULATOR_LIBS) \
	-lm

gcalccmd_SOURCES = \
	config.vapi \
	fixes.vapi \
	gcalccmd.vala \
	currency.vala \
	equation.vala \
	equation-lexer.vala \
	equation-parser.vala \
	number.vala \
	serializer.vala \
	unit.vala

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
	fixes.vapi \
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
	fixes.vapi \
	test-equation.vala \
	currency.vala \
	equation.vala \
	equation-lexer.vala \
	equation-parser.vala \
	number.vala \
	serializer.vala \
	unit.vala

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
	fixes.vapi \
	test-serializer.vala \
	number.vala \
	serializer.vala

test_serializer_VAFLAGS = \
	--target-glib 2.32 \
	--pkg posix \
	--pkg gio-2.0 \
	--pkg libxml-2.0

test_serializer_LDADD = \
	$(GCALCCMD_LIBS) \
	-lm

DISTCLEANFILES = \
	Makefile.in
