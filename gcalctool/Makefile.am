## Process this file with automake to produce Makefile.in

INCLUDES = \
	-I$(includedir) \
	-DGNOME_DISABLE_DEPRECATED \
	-DG_DISABLE_DEPRECATED \
	-DGDK_PIXBUF_DISABLE_DEPRECATED \
	-DGDK_DISABLE_DEPRECATED \
	-DPACKAGE_DATA_DIR=\""$(datadir)"\" \
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
	dsfuns.c \
	dsdefs.h \
	get.c \
	functions.c \
	graphics.c \
	mp.c \
	gtk.c \
	extern.h

gcalctool_LDADD = \
	$(PACKAGE_LIBS) \
	$(GNOME_UTILS_LIBS) \
	$(GCONF_LIBS) \
	$(X_LIBS) \
	$(X_PRE_LIBS) \
	$(X_BASIC_LIBS) \
	$(X_EXTRA_LIBS)

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
	$(man1_MANS)
