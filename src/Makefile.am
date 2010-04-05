bin_PROGRAMS = gcalctool gcalccmd

INCLUDES = \
	-DUI_DIR=\""$(datadir)/gcalctool"\" \
	-DVERSION=\""$(VERSION)"\" \
	-DLOCALE_DIR=\""$(localedir)"\" \
    -DGETTEXT_PACKAGE=\"$(GETTEXT_PACKAGE)\" \
	$(GCALCTOOL_CFLAGS)

gcalctool_SOURCES = \
	calctool.c \
	currency.c \
	currency.h \
	get.c \
	get.h \
	math-buttons.c \
	math-buttons.h \
	math-display.c \
	math-display.h \
	math-equation.c \
	math-equation.h \
	mp.c \
	mp.h \
	mp-binary.c \
	mp-convert.c \
	mp-internal.h \
	mp-trigonometric.c \
	mp-equation.c \
	mp-equation.h \
	mp-equation-private.h \
	mp-equation-lexer.c \
	mp-equation-lexer.h \
	mp-equation-parser.c \
	mp-equation-parser.h \
	financial.c \
	financial.h \
	register.c \
	register.h \
	ui.c \
	ui.h \
	ui-preferences.c \
	ui-preferences.h \
	unittest.c \
	unittest.h

gcalctool_LDADD = \
	$(GCALCTOOL_LIBS)

gcalccmd_SOURCES = \
	gcalccmd.c \
	mp.c \
	mp-convert.c \
	mp-binary.c \
	mp-trigonometric.c \
	mp-equation.c \
	mp-equation-parser.c \
	mp-equation-lexer.c

gcalccmd_LDADD = \
	$(GCALCCMD_LIBS) \
	-lm

CLEANFILES = \
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
	mp-equation-lexer.l

DISTCLEANFILES = \
	Makefile.in

test: gcalctool
	./gcalctool -u
