## Process this file with automake to produce Makefile.in

INCLUDES = \
	-I$(includedir) \
	-DG_DISABLE_DEPRECATED -DGDK_PIXBUF_DISABLE_DEPRECATED -DGDK_DISABLE_DEPRECATED -DGTK_DISABLE_DEPRECATED \
	-DCALCTOOL_DATA_DIR=\""$(datadir)/gcalctool"\" \
	-DPACKAGE_DATA_DIR=\""$(datadir)"\" \
	-DPACKAGE_LOCALE_DIR=\""$(prefix)/$(DATADIRNAME)/locale"\" \
	@PACKAGE_CFLAGS@ \
	@GNOME_UTILS_CFLAGS@ \
	@GCONF_CFLAGS@

bin_PROGRAMS = gcalctool

gcalctool_SOURCES = \
	calctool.c \
	calctool.h \
	display.c \
	dsfuns.c \
	dsdefs.h \
	get.c \
	functions.c \
	graphics.c \
	mp.c \
	gtk.c \
	extern.h

gcalctool_LDADD = \
	@PACKAGE_LIBS@ \
	@GNOME_UTILS_LIBS@ \
	@GCONF_LIBS@ \
	$(X_LIBS) \
	$(X_PRE_LIBS) \
	$(X_BASIC_LIBS) \
	$(X_EXTRA_LIBS)

pkgdata_DATA = gcalctool.png

man1_MANS = gcalctool.1

EXTRA_DIST = \
	$(pkgdata_DATA) \
	$(man1_MANS)
