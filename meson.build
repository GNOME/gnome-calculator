project('gnome-calculator', ['c', 'vala'],
  version: '49.1',
  meson_version: '>= 0.57.0',
  license: 'GPLv3+',
)

inc_rooth = include_directories ('.')
inc_rooth_dep = declare_dependency (include_directories : inc_rooth)

gnome = import('gnome')
i18n = import('i18n')

if get_option('development')
  app_id = 'org.gnome.Calculator.Devel'
  name_prefix = '(Nightly) '
  vcs_tag = run_command('git', 'rev-parse', '--short', 'HEAD').stdout().strip()
  version_suffix = '-@0@'.format (vcs_tag)
else
  app_id = 'org.gnome.Calculator'
  name_prefix = ''
  version_suffix = ''
endif

# Some variables
libexec_dir = join_paths(get_option('prefix'), get_option('libexecdir'))
locale_dir = join_paths(get_option('prefix'), get_option('localedir'))
po_dir = join_paths(meson.project_source_root(), 'po')
gcalc_vapi_dir = join_paths(meson.current_source_dir(), 'vapi')

# Dependencies
glib_min_version = '2.40.0'
libsoup_min_version = '3.4.0'

gio = dependency('gio-2.0', version: '>= ' + glib_min_version)
glib = dependency('glib-2.0', version: '>= ' + glib_min_version)
gmodule_export = dependency('gmodule-export-2.0')
gobject = dependency('gobject-2.0', version: '>= ' + glib_min_version)
libxml = dependency('libxml-2.0')
libsoup = dependency('libsoup-3.0', version: '>= ' + libsoup_min_version)
gee = dependency('gee-0.8', version: '>= 0.20.0')
gobject_introspection = dependency('gobject-introspection-1.0', required: false)

introspection = not get_option('disable-introspection')
if not gobject_introspection.found()
introspection = false
endif

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

conf.set_quoted('APP_ID', app_id)
conf.set_quoted('NAME_PREFIX', name_prefix)
conf.set_quoted('GETTEXT_PACKAGE', meson.project_name())
conf.set_quoted('LOCALE_DIR', locale_dir)
conf.set_quoted('VERSION', meson.project_version() + version_suffix)
conf.set10('DEVELOPMENT_BUILD', get_option('development'))

configure_file(output: 'config.h', configuration: conf)
config_h_dir = include_directories('.')


# Subdirs
subdir('po')
subdir('vapi')
subdir('libmpfr')
if get_option ('gcalc')
subdir('gcalc')
endif
if not get_option ('disable-ui')
gtk = dependency('gtk4', version: '>= 4.17.0')

if get_option('gci') and get_option('gcalc')
subdir('gci')
endif

if get_option('app')
# Extra scripts
gnome.post_install(
  glib_compile_schemas: true,
  gtk_update_icon_cache: true,
)

libadwaita = dependency('libadwaita-1', version: '>= 1.8.alpha')
gtksourceview = dependency('gtksourceview-5', version: '>= 5.3.0')
subdir('data')
subdir('lib')
subdir('src')
subdir('search-provider')
subdir('help')
endif
endif

if get_option('doc')
subdir('doc')
endif

subdir('tests')
