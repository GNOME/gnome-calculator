## Process this file with automake to produce Makefile.in

INCLUDES = \
	-I$(includedir) \
	$(DISABLE_DEPRECATED) \
	-DPACKAGE_DATA_DIR=\""$(datadir)"\" \
	-DPACKAGE_GLADE_DIR=\""$(datadir)/gcalctool"\" \
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
	financial.c \
	financial.h \
	parser.c \
	parser.h \
	parser_mac.h \
	register.c \
	register.h \
	gtk.c \
	ui.h \
	unittest.c \
	unittest.h \
	ce_parser.c \
	ce_parser.h \
	ce_parser.tab.h
	
CLEANFILES = \
	ce_parser.tab.h \
	ce_parser.tab.c \
	lex.ce.c \
	libparser.a

gcalctool_LDADD = \
	$(PACKAGE_LIBS) \
	$(GNOME_UTILS_LIBS) \
	$(GCONF_LIBS) \
	libparser.a

libparser.a: \
	lex.ce.o\
	ce_parser.tab.o\
	ce_parser.o
	$(AR) r libparser.a $^
	$(RANLIB) libparser.a

ce_parser.o: ce_parser.c ce_parser.h calctool.h
	$(COMPILE) -c $(INCLUDES) -o $@ $(srcdir)/ce_parser.c

lex.ce.o: lex.ce.c calctool.h
	$(COMPILE) -c $(INCLUDES) -o $@ lex.ce.c

ce_parser.tab.o: ce_parser.tab.c
	$(COMPILE) -c $(INCLUDES) -o $@ ce_parser.tab.c

ce_parser.tab.c: ce_parser.y calctool.h
	bison -d -p ce -d $(srcdir)/ce_parser.y

lex.ce.c: ce_parser.tab.c ce_tokeniser.l
	$(LEX) -Pce $(srcdir)/ce_tokeniser.l

code-format: 
	ls *[ch] | xargs indent -nbad -bap -bbo -nbc -br\
	-brs -c33 -cd33 -ncdb -ce -ci4 -cli0 -cp33 \
	-cs -d0 -di1 -nfc1 -nfca -hnl -i4 -ip0 -l75 -lp -npcs\
	-nprs -npsl -nut -saf -sai -saw -nsc -nsob -nss

man1_MANS = gcalctool.1

# Install a symlink between gcalctool and gnome-calculator
install-exec-hook:
	test -e "$(DESTDIR)$(bindir)/gnome-calculator" \
	|| (cd "$(DESTDIR)$(bindir)" && ln -s gcalctool gnome-calculator)

# Remove the symlink between gcalctool and gnome-calculator
uninstall-local:
	test -h "$(DESTDIR)$(bindir)/gnome-calculator" \
	&& rm -f "$(DESTDIR)$(bindir)/gnome-calculator"

EXTRA_DIST = \
	$(man1_MANS) \
	ce_parser.y \
	ce_tokeniser.l

test: gcalctool
	./gcalctool -u
