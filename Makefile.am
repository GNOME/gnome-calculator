## Process this file with automake to produce Makefile.in

SUBDIRS = po gcalctool help

schemadir = @GCONF_SCHEMA_FILE_DIR@
schema_DATA = gcalctool.schemas

EXTRA_DIST = \
	autogen.sh \
	MAINTAINERS \
	TODO \
	gcalctoolrc \
	gcalctool.desktop.in \
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

# Install the gcalctool schema file.
install-data-local:
	GCONF_CONFIG_SOURCE=$(GCONF_SCHEMA_CONFIG_SOURCE) $(GCONFTOOL) --makefile-install-rule $(srcdir)/$(schema_DATA)
