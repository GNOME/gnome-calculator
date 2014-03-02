SUBDIRS = po src data help

# Temporary fix for JHBuild, see https://bugzilla.gnome.org/show_bug.cgi?id=641652
ACLOCAL_AMFLAGS = ${ACLOCAL_FLAGS}

DISTCLEANFILES = test-driver

MAINTAINERCLEANFILES = \
	$(GITIGNORE_MAINTAINERCLEANFILES_TOPLEVEL) \
	$(GITIGNORE_MAINTAINERCLEANFILES_MAKEFILE_IN)

EXTRA_DIST = \
	autogen.sh \
	gnome-calculator.doap

-include $(top_srcdir)/git.mk
