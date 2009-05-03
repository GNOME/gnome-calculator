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

EXTRA_DIST = $(ui_DATA) $(schema_in_files) gcalctool.desktop.in

if GCONF_SCHEMAS_INSTALL
# Install the gcalctool schema file.
install-data-local:
	GCONF_CONFIG_SOURCE=$(GCONF_SCHEMA_CONFIG_SOURCE) $(GCONFTOOL) --makefile-install-rule $(srcdir)/$(schema_DATA)
endif

DISTCLEANFILES = gcalctool.desktop $(schema_DATA)
