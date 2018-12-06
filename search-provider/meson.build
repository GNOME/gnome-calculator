search_provider_sources = [
  'search-provider.vala',
]

search_provider_deps = [
  gio,
  glib,
  gobject,
  gtk,
  gtksourceview,
  mpc,
  mpfr,
  posix,
  inc_rooth_dep
]

search_provider_vala_flags = [
  '--vapidir=' + meson.current_source_dir(),
  '--pkg', 'config',
]

search_provider_c_flags = [
  '-include', 'config.h',
]

search_provider_includes = [
  config_h_dir,
  include_directories('../lib'),
]

# The executable
executable('gnome-calculator-search-provider', search_provider_sources,
  dependencies: search_provider_deps,
  link_with: [lib, lib_mpfrg, libcalculator],
  install_dir: get_option('libexecdir'),
  include_directories: search_provider_includes,
  vala_args: search_provider_vala_flags,
  c_args: search_provider_c_flags,
  install: true,
)

# The search-provider file for GNOME shell
install_data('org.gnome.Calculator-search-provider.ini',
  install_dir: join_paths(get_option('datadir'), 'gnome-shell', 'search-providers'),
)

# The DBUS service file
dbusconf = configuration_data()
dbusconf.set('libexecdir', libexec_dir)
configure_file(
  input: 'org.gnome.Calculator.SearchProvider.service.in',
  output: 'org.gnome.Calculator.SearchProvider.service',
  configuration: dbusconf,
  install: true,
  install_dir: join_paths(get_option('datadir'), 'dbus-1', 'services'),
)
