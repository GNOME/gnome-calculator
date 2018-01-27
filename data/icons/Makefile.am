NULL =

public_icons_themes = \
	hicolor \
	$(NULL)

public_icons = \
	hicolor_apps_16x16_gnome-calculator.png \
	hicolor_apps_22x22_gnome-calculator.png \
	hicolor_apps_24x24_gnome-calculator.png \
	hicolor_apps_32x32_gnome-calculator.png \
	hicolor_apps_48x48_gnome-calculator.png \
	hicolor_apps_256x256_gnome-calculator.png \
	hicolor_apps_scalable_gnome-calculator-symbolic.svg \
	$(NULL)

icon16dir=$(datadir)/icons/hicolor/16x16/apps
icon22dir=$(datadir)/icons/hicolor/22x22/apps
icon24dir=$(datadir)/icons/hicolor/24x24/apps
icon32dir=$(datadir)/icons/hicolor/32x32/apps
icon48dir=$(datadir)/icons/hicolor/48x48/apps
icon256dir=$(datadir)/icons/hicolor/256x256/apps
iconscalabledir=$(datadir)/icons/hicolor/scalable/apps

icon16_DATA = hicolor/16x16/apps/gnome-calculator.png
icon22_DATA = hicolor/22x22/apps/gnome-calculator.png
icon24_DATA = hicolor/24x24/apps/gnome-calculator.png
icon32_DATA = hicolor/32x32/apps/gnome-calculator.png
icon48_DATA = hicolor/48x48/apps/gnome-calculator.png
icon256_DATA = hicolor/256x256/apps/gnome-calculator.png
iconscalable_DATA = hicolor/scalable/apps/gnome-calculator-symbolic.svg

noinst_DATA = \
	gnome-calculator-src.svg \
	$(NULL)

EXTRA_DIST = \
	$(icon16_DATA)	\
	$(icon22_DATA)	\
	$(icon24_DATA)	\
	$(icon32_DATA)	\
	$(icon48_DATA)	\
	$(icon256_DATA)	\
	$(iconscalable_DATA)	\
	$(noinst_DATA)		\
	$(NULL)

###############################################################################

gtk_update_icon_cache = gtk-update-icon-cache -f -t

update-icon-cache:
	@-if test -z "$(DESTDIR)"; then \
		echo "Updating Gtk icon cache."; \
		for theme in $(public_icons_themes); do \
			$(gtk_update_icon_cache) $(datadir)/icons/$$theme; \
		done; \
	else \
		echo "*** Icon cache not updated.  After (un)install, run this:"; \
		for theme in $(public_icons_themes); do \
			echo "***   $(gtk_update_icon_cache) $(datadir)/icons/$$theme"; \
		done; \
	fi

install-data-local: update-icon-cache

uninstall-local: update-icon-cache

-include $(top_srcdir)/git.mk
