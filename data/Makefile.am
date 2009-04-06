uidir = $(datadir)/gcalctool
ui_DATA = \
	gcalctool.ui \
	financial.ui
	
SCHEMAS_FILE = gcalctool.schemas
schemadir = $(GCONF_SCHEMA_FILE_DIR)
schema_DATA = $(SCHEMAS_FILE)

Utilitiesdir = $(datadir)/applications
Utilities_in_files = gcalctool.desktop.in
Utilities_DATA = $(Utilities_in_files:.desktop.in=.desktop)
@INTLTOOL_DESKTOP_RULE@

EXTRA_DIST = $(ui_DATA) $(schema_DATA) gcalctool.desktop.in

if GCONF_SCHEMAS_INSTALL
# Install the gcalctool schema file.
install-data-local:
	GCONF_CONFIG_SOURCE=$(GCONF_SCHEMA_CONFIG_SOURCE) $(GCONFTOOL) --makefile-install-rule $(srcdir)/$(schema_DATA)
endif

DISTCLEANFILES = gcalctool.desktop
