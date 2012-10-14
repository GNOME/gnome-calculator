bin_PROGRAMS = gcalctool gcalccmd
noinst_PROGRAMS = test-number test-equation

TESTS = test-number test-equation

INCLUDES = \
	-DUI_DIR=\""$(datadir)/gcalctool"\" \
	-DVERSION=\""$(VERSION)"\" \
	-DLOCALE_DIR=\""$(localedir)"\" \
	-DGETTEXT_PACKAGE=\"$(GETTEXT_PACKAGE)\" \
	$(WARN_CFLAGS) \
	$(GCALCTOOL_CFLAGS)

gcalctool_SOURCES = \
	config.vapi \
	gcalctool.vala \
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

gcalctool_VALAFLAGS = \
	--target-glib 2.32 \
	--pkg posix \
	--pkg gtk+-3.0 \
	--pkg libxml-2.0

gcalctool_LDADD = \
	$(GCALCTOOL_LIBS)        

gcalccmd_SOURCES = \
	config.vapi \
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
	unit.vala

test_equation_VALAFLAGS = \
	--target-glib 2.32 \
	--pkg posix \
	--pkg gio-2.0 \
	--pkg libxml-2.0

test_equation_LDADD = \
	$(GCALCCMD_LIBS) \
	-lm

# Install a symlink between gcalctool and gnome-calculator
install-exec-hook:
	test -e "$(DESTDIR)$(bindir)/gnome-calculator" \
	|| (cd "$(DESTDIR)$(bindir)" && ln -s gcalctool gnome-calculator)

# Remove the symlink between gcalctool and gnome-calculator
uninstall-local:
	test -h "$(DESTDIR)$(bindir)/gnome-calculator" \
	&& rm -f "$(DESTDIR)$(bindir)/gnome-calculator"

DISTCLEANFILES = \
	Makefile.in

test: gcalctool
	./gcalctool -u
