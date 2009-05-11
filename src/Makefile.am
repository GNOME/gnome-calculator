## Process this file with automake to produce Makefile.in

INCLUDES = \
	-I$(includedir) \
	$(DISABLE_DEPRECATED) \
	-DPACKAGE_DATA_DIR=\""$(datadir)"\" \
	-DPACKAGE_UI_DIR=\""$(datadir)/gcalctool"\" \
	-DPACKAGE_LOCALE_DIR=\""$(prefix)/$(DATADIRNAME)/locale"\" \
	-DPACKAGE_PIXMAP_DIR=\""$(prefix)/$(DATADIRNAME)/pixmaps"\" \
	$(PACKAGE_CFLAGS) \
	$(GNOME_UTILS_CFLAGS) \
	$(GCONF_CFLAGS)

bin_PROGRAMS = gcalctool

gcalctool_SOURCES = \
	calctool.c \
	calctool.h \
	display.c \
	display.h \
	dsfuns.c \
	dsdefs.h \
	get.c \
	get.h \
	functions.c \
	functions.h \
	mp.c \
	mp.h \
	mp-binary.c \
	mp-convert.c \
	mp-internal.h \
	mp-trigonometric.c \
	mp-equation.c \
	mp-equation.h \
	mp-equation.tab.h \	
	financial.c \
	financial.h \
	register.c \
	register.h \
	gtk.c \
	ui.h \
	unittest.c \
	unittest.h
	
CLEANFILES = \
	mp-equation-parser.tab.h \
	mp-equation-parser.tab.c \
	mp-equation-lexer.c \
	libparser.a

gcalctool_LDADD = \
	$(PACKAGE_LIBS) \
	$(GNOME_UTILS_LIBS) \
	$(GCONF_LIBS) \
	libparser.a

libparser.a: \
	mp-equation-lexer.o\
	mp-equation-parser.tab.o\
	mp-equation.o
	$(AR) r libparser.a $^
	$(RANLIB) libparser.a

mp-equation.o: mp-equation.c mp-equation.h calctool.h
	$(COMPILE) -c $(INCLUDES) -o $@ $(srcdir)/mp-equation.c

mp-equation-lexer.o: mp-equation-lexer.c calctool.h
	$(COMPILE) -c $(INCLUDES) -o $@ mp-equation-lexer.c

mp-equation-parser.tab.o: mp-equation-parser.tab.c
	$(COMPILE) -c $(INCLUDES) -o $@ mp-equation-parser.tab.c

mp-equation-parser.tab.c: mp-equation-parser.y calctool.h
	bison -d -p ce -d $(srcdir)/mp-equation-parser.y

mp-equation-lexer.c: mp-equation-parser.tab.c mp-equation-tokeniser.l
	$(LEX) -Pce -o $@ $(srcdir)/mp-equation-tokeniser.l

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
	mp-equation-tokeniser.l

test: gcalctool
	./gcalctool -u
