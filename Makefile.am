## Process this file with automake to produce Makefile.in

SUBDIRS = po gcalctool

schemadir = $(sysconfdir)/gconf/schemas
schema_DATA = gcalctool.schemas

EXTRA_DIST = \
	autogen.sh \
	depcomp \
	MAINTAINERS \
	TODO \
	gcalctoolrc \
	$(schema_DATA)

# Install the gcalctool schema file.
install-data-local:
	GCONF_CONFIG_SOURCE=$(GCONF_SCHEMA_CONFIG_SOURCE) $(GCONFTOOL) --makefile-install-rule $(srcdir)/$(schema_DATA)
