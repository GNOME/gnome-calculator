gsettings_SCHEMAS = org.gnome.calculator.gschema.xml
@GSETTINGS_RULES@

desktopdir = $(datadir)/applications
desktop_in_files = org.gnome.Calculator.desktop.in
desktop_DATA = $(desktop_in_files:.desktop.in=.desktop)
@INTLTOOL_DESKTOP_RULE@

@INTLTOOL_XML_RULE@
appdatadir = $(datadir)/appdata
appdata_DATA = $(appdata_in_files:.xml.in=.xml)
appdata_in_files = org.gnome.Calculator.appdata.xml.in

man1_MANS = gnome-calculator.1 gcalccmd.1

EXTRA_DIST = \
	$(appdata_in_files) \
	$(desktop_in_files) \
	$(gsettings_SCHEMAS) \
	$(man1_MANS)

CLEANFILES = \
	$(appdata_DATA) \
	$(desktop_DATA)

-include $(top_srcdir)/git.mk
