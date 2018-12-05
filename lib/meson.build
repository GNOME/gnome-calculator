libcalculator_sources = files ([
  'mpfr-glue.vala',
  'currency.vala',
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
])

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
  libsoup,
  libxml,
  mpc,
  mpfr,
  posix,
]

libcalculator = static_library('calculator', libcalculator_sources,
  dependencies: libcalculator_deps,
  c_args: libcalculator_c_flags,
  vala_args: libcalculator_vala_flags,
  include_directories: config_h_dir,
  install: false,
)
