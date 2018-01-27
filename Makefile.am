SUBDIRS = data lib src search-provider tests help po

AM_DISTCHECK_CONFIGURE_FLAGS = --enable-installed-tests

EXTRA_DIST = \
	meson.build \
	data/meson.build \
	help/meson.build \
	lib/meson.build \
	po/meson.build \
	search-provider/meson.build \
	src/meson.build \
	tests/meson.build \
	meson_post_install.py \
	$(NULL)

MAINTAINERCLEANFILES = \
	$(GITIGNORE_MAINTAINERCLEANFILES_TOPLEVEL) \
	$(GITIGNORE_MAINTAINERCLEANFILES_MAKEFILE_IN) \
	$(GITIGNORE_MAINTAINERCLEANFILES_M4_LIBTOOL)

GITIGNOREFILES = m4

@BEHAVE_INSTALLED_TESTS_RULE@
INSTALLED_TESTS=basic_mode advanced_mode financial_mode programming_mode
INSTALLED_TESTS_TYPE=session-exclusive

-include $(top_srcdir)/git.mk
