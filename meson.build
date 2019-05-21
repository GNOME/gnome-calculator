project('gnome-calculator', ['c', 'vala'],
  version: '3.33.2',
  meson_version: '>= 0.42',
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
vapi_dir = join_paths(meson.source_root(), 'vapi')

# Add our custom VAPI dir
add_project_arguments(['--vapidir', vapi_dir],
  language: 'vala'
)

# Dependencies
glib_min_version = '2.40.0'

gio = dependency('gio-2.0', version: '>= ' + glib_min_version)
glib = dependency('glib-2.0', version: '>= ' + glib_min_version)
gmodule_export = dependency('gmodule-export-2.0')
gobject = dependency('gobject-2.0', version: '>= ' + glib_min_version)
libxml = dependency('libxml-2.0')
libsoup = dependency('libsoup-2.4', version: '>= 2.42')
gee = dependency('gee-0.8', version: '>= 0.20.0')

# Vala compiler as expression parser
vala_version = get_option ('vala-version')
if vala_version == ''
	vc = find_program('valac')
	r = run_command(vc, '--api-version')
	vala_version = r.stdout().strip()
endif
vala_dep_str = 'libvala-@0@'.format (vala_version)
vala_dep = dependency(vala_dep_str)

# Libraries
cc = meson.get_compiler('c')
valac = meson.get_compiler('vala')

libmath = cc.find_library('m')
mpc = declare_dependency(
  dependencies: [
    cc.find_library('mpc'),
    valac.find_library('mpc', dirs: vapi_dir)
  ]
)
mpfr = declare_dependency(
  dependencies: [
    cc.find_library('mpfr'),
    valac.find_library('mpfr', dirs: vapi_dir)
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
subdir('gcalc')
if not get_option ('disable-ui')
gtk = dependency('gtk+-3.0', version: '>= 3.19.3')
gtksourceview = dependency('gtksourceview-4', version: '>= 4.0.2')
subdir('data')
subdir('lib')
subdir('src')
subdir('search-provider')
subdir('help')
subdir('po')
endif
subdir('tests')
