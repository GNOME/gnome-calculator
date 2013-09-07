gsettings_SCHEMAS = org.gnome.calculator.gschema.xml
@INTLTOOL_XML_NOMERGE_RULE@
@GSETTINGS_RULES@

Utilitiesdir = $(datadir)/applications
Utilities_in_files = gcalctool.desktop.in
Utilities_DATA = $(Utilities_in_files:.desktop.in=.desktop)
@INTLTOOL_DESKTOP_RULE@

appdatadir = $(datadir)/appdata
dist_appdata_DATA = gnome-calculator.appdata.xml

man1_MANS = gnome-calculator.1 gcalccmd.1

EXTRA_DIST = \
	buttons-basic.ui \
	buttons-advanced.ui \
	buttons-financial.ui \
	buttons-programming.ui \
	menu.ui \
	gnome-calculator.gresource.xml \
	org.gnome.calculator.gschema.xml.in \
	gcalctool.desktop.in \
	$(man1_MANS)

DISTCLEANFILES = \
	Makefile.in \
	gcalctool.desktop \
	$(gsettings_SCHEMAS)
