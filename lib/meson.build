libcalculator_sources = [
  'currency.vala',
  'currency-provider.vala',
  'equation.vala',
  'equation-lexer.vala',
  'equation-parser.vala',
  'financial.vala',
  'function-manager.vala',
  'math-equation.vala',
  'math-function.vala',
  'math-variables.vala',
  'number.vala',
  'serializer.vala',
  'unit.vala',
  'providers/imf-provider.vala',
  'providers/ecb-provider.vala',
  'providers/unt-provider.vala',
  'providers/boc-provider.vala',
]

libcalculator_vala_flags = [
  '--target-glib', glib_min_version,
]

libcalculator_c_flags = [
  '-include', 'config.h',
]

libcalculator_deps = [
  gio,
  glib,
  gobject,
  gtk,
  gtksourceview,
  libadwaita,
  libsoup,
  libxml,
  mpc,
  mpfr,
  posix,
  libmpfr_inc_libh_dep
]

libcalculator = static_library('calculator', libcalculator_sources,
  dependencies: libcalculator_deps,
  c_args: libcalculator_c_flags,
  vala_args: libcalculator_vala_flags,
  include_directories: config_h_dir,
  link_with: [ lib_mpfrg ],
  install: false,
)
