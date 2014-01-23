gsettings_SCHEMAS = org.gnome.calculator.gschema.xml
@GSETTINGS_RULES@

Utilitiesdir = $(datadir)/applications
Utilities_in_files = gcalctool.desktop.in
Utilities_DATA = $(Utilities_in_files:.desktop.in=.desktop)
@INTLTOOL_DESKTOP_RULE@

@INTLTOOL_XML_RULE@
appdatadir = $(datadir)/appdata
appdata_DATA = $(appdata_in_files:.xml.in=.xml)
appdata_in_files = gcalctool.appdata.xml.in

man1_MANS = gnome-calculator.1 gcalccmd.1

EXTRA_DIST = \
	gcalctool.appdata.xml.in \
	buttons-basic.ui \
	buttons-advanced.ui \
	buttons-financial.ui \
	buttons-programming.ui \
	menu.ui \
	gnome-calculator.gresource.xml \
	org.gnome.calculator.gschema.xml \
	gcalctool.desktop.in \
	$(man1_MANS)

CLEANFILES = \
	gcalctool.appdata.xml \
	gcalctool.desktop

DISTCLEANFILES = \
	Makefile.in
