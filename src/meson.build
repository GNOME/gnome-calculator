# gnome-calculator executable
gnome_calculator_vala_sources = [
  'buttons-advanced.vala',
  'buttons-basic.vala',
  'buttons-conversion.vala',
  'buttons-financial.vala',
  'buttons-programming.vala',
  'gnome-calculator.vala',
  'math-buttons.vala',
  'math-converter.vala',
  'math-display.vala',
  'math-preferences.vala',
  'math-popover.vala',
  'math-function-popover.vala',
  'math-variable-popover.vala',
  'math-window.vala',
  'math-history.vala',
]

blueprints = custom_target('blueprints',
  input: files(
    'ui/buttons-advanced.blp',
    'ui/buttons-basic.blp',
    'ui/buttons-conversion.blp',
    'ui/buttons-financial.blp',
    'ui/buttons-programming.blp',
    'ui/history-entry.blp',
    'ui/history-view.blp',
    'ui/math-converter.blp',
    'ui/math-display.blp',
    'ui/math-function-popover.blp',
    'ui/math-preferences.blp',
    'ui/math-shortcuts.blp',
    'ui/math-variable-popover.blp',
    'ui/math-window.blp',
  ),
  output: '.',
  command: [find_program('blueprint-compiler'), 'batch-compile', '@OUTPUT@', '@CURRENT_SOURCE_DIR@/ui', '@INPUT@'],
)

gnome_calculator_resource_files = files('ui/gnome-calculator.gresource.xml')
gnome_calculator_resources = gnome.compile_resources('org.gnome.Calculator', gnome_calculator_resource_files, dependencies: blueprints, source_dir: ['ui'])

gnome_calculator_sources = [
  gnome_calculator_vala_sources,
  gnome_calculator_resources,
]

gnome_calculator_deps = [
  glib,
  gobject,
  gtk,
  gtksourceview,
  libadwaita,
  mpc,
  mpfr,
  posix
]

gnome_calculator_vala_flags = [
  '--target-glib', glib_min_version,
  '--vapidir=' + meson.current_source_dir(),
  '--pkg', 'config',
  '--gresources', gnome_calculator_resource_files,
  '--vapidir=' + gcalc_vapi_dir
]

gnome_calculator_c_flags = [
  '-include', 'config.h',
]

gnome_calculator_includes = [
  config_h_dir,
  include_directories('../lib'),
]

executable('gnome-calculator', gnome_calculator_sources,
  dependencies: gnome_calculator_deps,
  include_directories: gnome_calculator_includes,
  vala_args: gnome_calculator_vala_flags,
  c_args: gnome_calculator_c_flags,
  link_with: [libcalculator, lib_mpfrg],
  install: true,
)


# gcalccmd executable
gcalccmd_sources = [
  'gcalccmd.vala',
]

gcalccmd_deps = [
  gio,
  glib,
  gobject,
  gtk,
  gtksourceview,
  mpc,
  mpfr,
  posix,
  libmpfr_inc_libh_dep
]

gcalccmd_vala_flags = [
  '--vapidir=' + meson.current_source_dir(),
  '--pkg', 'config',
  '--vapidir=' + gcalc_vapi_dir
]

gcalccmd_includes = [
  config_h_dir,
  include_directories('../lib'),
]

executable('gcalccmd', gcalccmd_sources,
  dependencies: gcalccmd_deps,
  link_with: [libcalculator, lib_mpfrg],
  include_directories: gcalccmd_includes,
  vala_args: gcalccmd_vala_flags,
  install: true,
)
