gsettings_SCHEMAS = org.gnome.calculator.gschema.xml
@GSETTINGS_RULES@

desktopdir = $(datadir)/applications
desktop_in_files = org.gnome.Calculator.desktop.in
desktop_DATA = $(desktop_in_files:.desktop.in=.desktop)

$(desktop_DATA): $(desktop_in_files)
	$(AM_V_GEN) $(MSGFMT) --desktop --template $< -d $(top_srcdir)/po -o $@

appdatadir = $(datadir)/appdata
appdata_XML = $(appdata_in_files:.xml.in=.xml)
appdata_in_files = org.gnome.Calculator.appdata.xml.in
@APPSTREAM_XML_RULES@

$(appstream_XML): $(appstream_in_files)
	$(AM_V_GEN) $(MSGFMT) --xml --template $< -d $(top_srcdir)/po -o $@

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
