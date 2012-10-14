uidir = $(pkgdatadir)
ui_DATA = \
	buttons-basic.ui \
	buttons-advanced.ui \
	buttons-financial.ui \
	buttons-programming.ui

gsettings_SCHEMAS = org.gnome.calculator.gschema.xml
@INTLTOOL_XML_NOMERGE_RULE@
@GSETTINGS_RULES@

Utilitiesdir = $(datadir)/applications
Utilities_in_files = gnome-calculator.desktop.in
Utilities_DATA = $(Utilities_in_files:.desktop.in=.desktop)
@INTLTOOL_DESKTOP_RULE@

man1_MANS = gnome-calculator.1 gcalccmd.1

EXTRA_DIST = $(ui_DATA) org.gnome.calculator.gschema.xml.in gnome-calculator.desktop.in $(man1_MANS)

DISTCLEANFILES = \
	Makefile.in \
	gnome-calculator.desktop \
	$(gsettings_SCHEMAS)
