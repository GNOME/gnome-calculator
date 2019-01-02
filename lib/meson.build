libcalculator_sources = files ([
  'financial.vala',
  'function-manager.vala',
  'math-equation.vala',
  'math-function.vala',
  'number.vala',
  'serializer.vala',
  'currency.vala',
  'equation.vala',
  'equation-lexer.vala',
  'equation-parser.vala',
  'math-variables.vala',
  'unit-manager.vala'
])

libcalculator_vala_flags = [
  '--target-glib', glib_min_version,
]

libcalculator_c_flags = [
  '-include', 'config.h',
]

libcalculator_deps = [
  gtk,
  gtksourceview,
  mpc,
  mpfr,
  posix,
  libxml,
  libsoup,
  inc_libh_dep,
  inc_rooth_dep,
]

libcalculator = static_library('calculator', libcalculator_sources,
  dependencies: libcalculator_deps,
  c_args: libcalculator_c_flags,
  vala_args: libcalculator_vala_flags,
  include_directories: config_h_dir,
  install: false,
  link_with: [lib, lib_mpfrg],
)
