libexec_PROGRAMS = gnome-calculator-search-provider

gnome_calculator_search_provider_SOURCES = \
	search-provider.vala

gnome_calculator_search_provider_VALAFLAGS = \
	--pkg posix \
	--pkg gio-2.0

gnome_calculator_search_provider_CPPFLAGS = \
	-w \
	$(SEARCH_PROVIDER_CFLAGS)

gnome_calculator_search_provider_LDADD = \
	$(SEARCH_PROVIDER_LIBS)

searchproviderdir = $(datadir)/gnome-shell/search-providers
searchprovider_DATA = org.gnome.Calculator-search-provider.ini

servicedir = $(datadir)/dbus-1/services
service_DATA = $(service_in_files:.service.in=.service)
service_in_files = org.gnome.Calculator.SearchProvider.service.in

org.gnome.Calculator.SearchProvider.service: org.gnome.Calculator.SearchProvider.service.in Makefile
	$(AM_V_GEN) sed -e "s|\@libexecdir\@|$(libexecdir)|" $< > $@.tmp && mv $@.tmp $@

EXTRA_DIST = \
	$(searchprovider_DATA) \
	$(service_in_files)

CLEANFILES = \
	$(service_DATA)

-include $(top_srcdir)/git.mk
