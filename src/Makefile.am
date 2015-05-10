bin_PROGRAMS = gnome-calculator gcalccmd

AM_CPPFLAGS = \
	-DUI_DIR=\""$(datadir)/gnome-calculator"\" \
	-DVERSION=\""$(VERSION)"\" \
	-DLOCALE_DIR=\""$(localedir)"\" \
	-DGETTEXT_PACKAGE=\"$(GETTEXT_PACKAGE)\" \
	-w \
	-I$(top_srcdir)/lib \
	$(LIBCALCULATOR_CFLAGS)

BUILT_SOURCES = resources.c

resources.c: $(top_srcdir)/data/gnome-calculator.gresource.xml $(shell $(GLIB_COMPILE_RESOURCES) --generate-dependencies --sourcedir=$(top_srcdir)/data/ $(top_srcdir)/data/gnome-calculator.gresource.xml)
	$(AM_V_GEN) $(GLIB_COMPILE_RESOURCES) --target=$@ --sourcedir=$(top_srcdir)/data/ --generate-source $<

gnome_calculator_SOURCES = \
	config.vapi \
	gnome-calculator.vala \
	math-buttons.vala \
	math-converter.vala \
	math-display.vala \
	math-preferences.vala \
	math-variable-popup.vala \
	math-window.vala \
	math-function-popup.vala \
	math-history.vala \
	$(BUILT_SOURCES)

gnome_calculator_VALAFLAGS = \
	--target-glib 2.32 \
	--pkg posix \
	--pkg gtk+-3.0 \
	--pkg gtksourceview-3.0 \
	--pkg libxml-2.0 \
	$(top_builddir)/lib/libcalculator.vapi \
	$(top_builddir)/lib/mpfr.vapi

gnome_calculator_CPPFLAGS = \
	$(AM_CPPFLAGS) \
	$(GNOME_CALCULATOR_CFLAGS)

gnome_calculator_LDADD = \
	$(GNOME_CALCULATOR_LIBS) \
	$(top_builddir)/lib/libcalculator.la

gcalccmd_SOURCES = \
	config.vapi \
	gcalccmd.vala

gcalccmd_VALAFLAGS = \
	--target-glib 2.32 \
	--pkg posix \
	--pkg gio-2.0 \
	--pkg gtksourceview-3.0 \
	--pkg libxml-2.0 \
	$(top_builddir)/lib/libcalculator.vapi \
	$(top_builddir)/lib/mpfr.vapi

gcalccmd_LDADD = \
	$(top_builddir)/lib/libcalculator.la

-include $(top_srcdir)/git.mk
