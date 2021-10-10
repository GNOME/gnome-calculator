project('gnome-calculator', ['c', 'vala'],
  version: '41.0',
  meson_version: '>= 0.52.0',
  license: 'GPLv3+',
)

inc_rooth = include_directories ('.')
inc_rooth_dep = declare_dependency (include_directories : inc_rooth)

gnome = import('gnome')
i18n = import('i18n')

# Some variables
libexec_dir = join_paths(get_option('prefix'), get_option('libexecdir'))
locale_dir = join_paths(get_option('prefix'), get_option('localedir'))
po_dir = join_paths(meson.source_root(), 'po')
gcalc_vapi_dir = join_paths(meson.current_source_dir(), 'vapi')

# Dependencies
glib_min_version = '2.40.0'

gio = dependency('gio-2.0', version: '>= ' + glib_min_version)
glib = dependency('glib-2.0', version: '>= ' + glib_min_version)
gmodule_export = dependency('gmodule-export-2.0')
gobject = dependency('gobject-2.0', version: '>= ' + glib_min_version)
libxml = dependency('libxml-2.0')
libsoup = dependency('libsoup-2.4', version: '>= 2.42')
gee = dependency('gee-0.8', version: '>= 0.20.0')

# Libraries
cc = meson.get_compiler('c')
valac = meson.get_compiler('vala')

libmath = cc.find_library('m')
searchpath = []
if get_option('libpath') != ''
searchpath += [get_option('libpath')]
endif
mpc = declare_dependency(
  dependencies: [
    cc.find_library('mpc', dirs: searchpath),
    valac.find_library('mpc', dirs: gcalc_vapi_dir)
  ]
)
mpfr = declare_dependency(
  dependencies: [
    cc.find_library('mpfr', dirs: searchpath),
    valac.find_library('mpfr', dirs: gcalc_vapi_dir)
  ]
)
posix = valac.find_library('posix')

# Configuration
conf = configuration_data()

conf.set_quoted('GETTEXT_PACKAGE', meson.project_name())
conf.set_quoted('LOCALE_DIR', locale_dir)
conf.set_quoted('VERSION', meson.project_version())

configure_file(output: 'config.h', configuration: conf)
config_h_dir = include_directories('.')

# Extra scripts
meson.add_install_script('meson_post_install.py')

# Subdirs
subdir('vapi')
subdir('gcalc')
if not get_option ('disable-ui')
gtk = dependency('gtk4', version: '>= 4.5.0')
libadwaita = dependency('libadwaita-1', version: '>= 1.0.0')
subdir('gci')
gtksourceview = dependency('gtksourceview-5', version: '>= 5.2.0')
subdir('data')
subdir('lib')
subdir('src')
subdir('search-provider')
subdir('help')
subdir('doc')
subdir('po')
endif
subdir('tests')
