## Process this file with automake to produce Makefile.in

#
# FIXME:
#  currently scrollkeeper-update does not seem to
#  remove its database when there is no useful
#  information contained there. Therefore, with
#  "make uninstall" you're left with loads of
#  stuff in /var/scrollkeeper and distcheck
#  fails.
#
#  http://mail.gnome.org/archives/gnome-doc-list/2003-May/msg00008.html
#
#  This overrides the uninstall check so distcheck
#  passes
#
# distuninstallcheck_listfiles = find . -type f -print | grep -v 'scrollkeeper' | grep -v 'omf' | grep -v 'figures'
distuninstallcheck:
        @:

distcleancheck_listfiles = find . -type f -print | grep -v 'omf\.out' | grep -v 'legal.xml'

DISTCHECK_CONFIGURE_FLAGS = --disable-scrollkeeper

DISTCLEANFILES =                \
	intltool-extract        \
	intltool-merge          \
	intltool-update		\
	gnome-doc-utils.make	\
	gcalctool.desktop

SUBDIRS = po gcalctool glade help m4

SCHEMAS_FILE = gcalctool.schemas

schemadir = $(GCONF_SCHEMA_FILE_DIR)
schema_DATA = $(SCHEMAS_FILE)

EXTRA_DIST = \
	autogen.sh \
	ChangeLog-1990 \
	ChangeLog-1991 \
	ChangeLog-1992 \
	ChangeLog-1993 \
	ChangeLog-2002 \
	MAINTAINERS \
	TODO \
	gcalctoolrc \
	gcalctool.desktop.in \
	gnome-doc-utils.make	\
	intltool-extract.in     \
	intltool-merge.in       \
	intltool-update.in      \
	$(schema_DATA) \
	omf.make \
	xmldocs.make

Utilitiesdir = $(datadir)/applications
Utilities_in_files = gcalctool.desktop.in
Utilities_DATA = $(Utilities_in_files:.desktop.in=.desktop)
@INTLTOOL_DESKTOP_RULE@

if GCONF_SCHEMAS_INSTALL
# Install the gcalctool schema file.
install-data-local:
	GCONF_CONFIG_SOURCE=$(GCONF_SCHEMA_CONFIG_SOURCE) $(GCONFTOOL) --makefile-install-rule $(srcdir)/$(schema_DATA)
endif
