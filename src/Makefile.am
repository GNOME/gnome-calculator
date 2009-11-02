## Process this file with automake to produce Makefile.in
# FIXME: Don't distribute generated lex and parse files

INCLUDES = \
	-I$(includedir) \
	$(DISABLE_DEPRECATED) \
	-DDATA_DIR=\""$(datadir)"\" \
	-DUI_DIR=\""$(datadir)/gcalctool"\" \
	-DLOCALE_DIR=\""$(prefix)/$(DATADIRNAME)/locale"\" \
	-DPIXMAP_DIR=\""$(prefix)/$(DATADIRNAME)/pixmaps"\" \
	$(GCALCTOOL_CFLAGS) \
	$(GNOME_UTILS_CFLAGS) \
	$(GCONF_CFLAGS) \
	$(LIBXML_CFLAGS) \
	$(LIBSOUP_CFLAGS)

bin_PROGRAMS = gcalctool gcalccmd

gcalctool_SOURCES = \
	calctool.c \
	calctool.h \
	currency.c \
	currency.h \
	display.c \
	display.h \
	get.c \
	get.h \
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
	gtk.c \
	ui.h \
	unittest.c \
	unittest.h

gcalccmd_SOURCES = \
	gcalccmd.c \
	mp.c \
	mp-convert.c \
	mp-binary.c \
	mp-trigonometric.c \
	mp-equation.c \
	mp-equation-parser.c \
	mp-equation-lexer.c

CLEANFILES = \
	mp-equation-parser.h \
	mp-equation-parser.c \
	mp-equation-lexer.c \
	mp-equation-lexer.h \
	libparser.a

gcalctool_LDADD = \
	$(GCALCTOOL_LIBS) \
	$(GNOME_UTILS_LIBS) \
	$(GCONF_LIBS) \
	$(LIBXML_LIBS) \
	$(LIBSOUP_LIBS) \
	libparser.a

gcalccmd_LDADD = \
	$(GCALCCMD_LIBS) \
	-lm

libparser.a: \
	mp-equation-lexer.o\
	mp-equation-parser.o\
	mp-equation.o
	$(AR) r libparser.a $^
	$(RANLIB) libparser.a

mp-equation-parser.c mp-equation-parser.h: mp-equation-parser.y mp-equation-lexer.h
	$(YACC) -d -o mp-equation-parser.c $(srcdir)/mp-equation-parser.y

mp-equation-lexer.c mp-equation-lexer.h: mp-equation-lexer.l
	$(LEX) $(srcdir)/mp-equation-lexer.l

mp-equation-parser.o: mp-equation-lexer.h
mp-equation-lexer.o: mp-equation-parser.h
mp-equation.c: mp-equation-lexer.h mp-equation-parser.h

code-format: 
	ls *[ch] | xargs indent -nbad -bap -bbo -nbc -br\
	-brs -c33 -cd33 -ncdb -ce -ci4 -cli0 -cp33 \
	-cs -d0 -di1 -nfc1 -nfca -hnl -i4 -ip0 -l75 -lp -npcs\
	-nprs -npsl -nut -saf -sai -saw -nsc -nsob -nss

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

test: gcalctool
	./gcalctool -u
