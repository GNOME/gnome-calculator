bin_PROGRAMS = gcalctool gcalccmd
noinst_PROGRAMS = test-mp test-mp-equation

TESTS = test-mp test-mp-equation

INCLUDES = \
	-DUI_DIR=\""$(datadir)/gcalctool"\" \
	-DVERSION=\""$(VERSION)"\" \
	-DLOCALE_DIR=\""$(localedir)"\" \
	-DGETTEXT_PACKAGE=\"$(GETTEXT_PACKAGE)\" \
	$(WARN_CFLAGS) \
	$(GCALCTOOL_CFLAGS)

gcalctool_SOURCES = \
	gcalctool.c \
	currency.c \
	currency.h \
	currency-manager.c \
	currency-manager.h \
	math-buttons.c \
	math-buttons.h \
	math-converter.c \
	math-converter.h \
	math-display.c \
	math-display.h \
	math-equation.c \
	math-equation.h \
	math-preferences.c \
	math-preferences.h \
	math-variables.c \
	math-variables.h \
	math-variable-popup.c \
	math-variable-popup.h \
	math-window.c \
	math-window.h \
	mp.c \
	mp.h \
	mp-binary.c \
	mp-convert.c \
	mp-enums.c \
	mp-enums.h \
	mp-equation.c \
	mp-equation.h \
	mp-equation-private.h \
	mp-equation-lexer.c \
	mp-equation-lexer.h \
	mp-equation-parser.c \
	mp-equation-parser.h \
	mp-private.h \
	mp-serializer.c \
	mp-serializer.h \
	mp-trigonometric.c \
	financial.c \
	financial.h \
  	unit.c \
	unit.h \
	unit-category.c \
	unit-category.h \
	unit-manager.c \
	unit-manager.h

gcalctool_LDADD = \
	$(GCALCTOOL_LIBS)        

gcalccmd_SOURCES = \
	gcalccmd.c \
	currency.c \
	currency.h \
	currency-manager.c \
	currency-manager.h \
	mp.c \
	mp-binary.c \
	mp-convert.c \
	mp-enums.c \
	mp-enums.h \
	mp-equation.c \
	mp-equation-parser.c \
	mp-equation-lexer.c \
	mp-serializer.c \
	mp-serializer.h\
	mp-trigonometric.c \
	unit.c \
	unit.h \
	unit-category.c \
	unit-category.h \
	unit-manager.c \
	unit-manager.h

gcalccmd_LDADD = \
	$(GCALCCMD_LIBS) \
	-lm

test_mp_SOURCES = \
	test-mp.c \
	mp.c \
	mp-binary.c \
	mp-convert.c \
	mp-enums.c \
	mp-enums.h \
	mp-serializer.c \
	mp-serializer.h \
	mp-trigonometric.c

test_mp_LDADD = \
	$(GCALCCMD_LIBS) \
	-lm

test_mp_equation_SOURCES = \
	test-mp-equation.c \
	currency.c \
	currency.h \
	currency-manager.c \
	currency-manager.h \
	mp.c \
	mp-convert.c \
	mp-binary.c \
	mp-enums.c \
	mp-enums.h \
	mp-equation.c \
	mp-equation-parser.c \
	mp-equation-lexer.c \
	mp-serializer.c \
	mp-serializer.h \
	mp-trigonometric.c \
	unit.c \
	unit.h \
	unit-category.c \
	unit-category.h \
	unit-manager.c \
	unit-manager.h

test_mp_equation_LDADD = \
	$(GCALCCMD_LIBS) \
	-lm

CLEANFILES = \
	mp-enums.c \
	mp-enums.h \
	mp-equation-parser.h \
	mp-equation-parser.c \
	mp-equation-lexer.c \
	mp-equation-lexer.h

# Generate parser files
mp-equation-parser.c mp-equation-parser.h: mp-equation-parser.y mp-equation-lexer.h
	$(AM_V_GEN)$(YACC) -d -o mp-equation-parser.c $(srcdir)/mp-equation-parser.y

# Generate lexer files
mp-equation-lexer.c mp-equation-lexer.h: mp-equation-lexer.l
	$(AM_V_GEN)$(LEX) $(srcdir)/mp-equation-lexer.l

# Rebuild parser when source files change
mp-equation-parser.o: mp-equation-lexer.h
mp-equation-lexer.o: mp-equation-parser.h
mp-equation.c: mp-equation-lexer.h mp-equation-parser.h

# Generate enum types
mp-enums.h: mp-enums.h.template mp-serializer.h
	$(AM_V_GEN)$(GLIB_MKENUMS) --template $(srcdir)/mp-enums.h.template $(srcdir)/mp-serializer.h > mp-enums.h

mp-enums.c: mp-enums.c.template mp-enums.h mp-serializer.h
	$(AM_V_GEN)$(GLIB_MKENUMS) --template $(srcdir)/mp-enums.c.template $(srcdir)/mp-serializer.h > mp-enums.c

# Fix dependencies
math-serializer.c: mp-enums.h
math-equation.c: mp-enums.h

# Install a symlink between gcalctool and gnome-calculator
install-exec-hook:
	test -e "$(DESTDIR)$(bindir)/gnome-calculator" \
	|| (cd "$(DESTDIR)$(bindir)" && ln -s gcalctool gnome-calculator)

# Remove the symlink between gcalctool and gnome-calculator
uninstall-local:
	test -h "$(DESTDIR)$(bindir)/gnome-calculator" \
	&& rm -f "$(DESTDIR)$(bindir)/gnome-calculator"

EXTRA_DIST = \
	mp-equation-parser.y \
	mp-equation-lexer.l \
	mp-enums.c.template \
	mp-enums.h.template

DISTCLEANFILES = \
	Makefile.in

test: gcalctool
	./gcalctool -u
