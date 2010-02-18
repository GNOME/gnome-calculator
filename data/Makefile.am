uidir = $(datadir)/gcalctool
ui_DATA = \
	gcalctool.ui \
	financial.ui

schemadir   = @GCONF_SCHEMA_FILE_DIR@
schema_in_files = gcalctool.schemas.in
schema_DATA = $(schema_in_files:.schemas.in=.schemas)
@INTLTOOL_SCHEMAS_RULE@

Utilitiesdir = $(datadir)/applications
Utilities_in_files = gcalctool.desktop.in
Utilities_DATA = $(Utilities_in_files:.desktop.in=.desktop)
@INTLTOOL_DESKTOP_RULE@

man1_MANS = gcalctool.1

EXTRA_DIST = $(ui_DATA) $(schema_in_files) gcalctool.desktop.in $(man1_MANS) gcalctoolrc

DISTCLEANFILES = \
	Makefile.in \
	gcalctool.desktop \
	$(schema_DATA)
