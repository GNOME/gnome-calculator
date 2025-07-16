if not get_option ('disable-ui')

if get_option('app')

# Common
gnome_calculator_tests_deps = [
  gtk,
  gtksourceview,
  gio,
  glib,
  gobject,
  libmath,
  mpc,
  mpfr,
  posix,
  inc_rooth_dep
]

gnome_calculator_tests_includes = [
  include_directories('../lib'),
]

# Tests

test_equation_sources = [
  'test-equation.vala',
]
test_equation = executable('test-equation', test_equation_sources,
  dependencies: gnome_calculator_tests_deps,
  link_with: [libcalculator, lib_mpfrg],
  include_directories: gnome_calculator_tests_includes,
)
test('Equation test', test_equation)

test_currency_sources = [
  'test-currency-providers.vala',
]

test_currency = executable('test-currency-providers', test_currency_sources,
  dependencies: gnome_calculator_tests_deps,
  link_with: [libcalculator, lib_mpfrg],
  include_directories: gnome_calculator_tests_includes,
)
tests_root_env = environment({'TESTS_ROOT': meson.current_source_dir()})
test('Currency test', test_currency, env: tests_root_env)


test_number_sources = [
  'test-number.vala',
]
test_number = executable('test-number', test_number_sources,
  dependencies: gnome_calculator_tests_deps,
  link_with: [libcalculator, lib_mpfrg],
  include_directories: gnome_calculator_tests_includes,
)
test('Number test', test_number)

test_serializer_sources = [
  'test-serializer.vala',
]
test_serializer = executable('test-serializer', test_serializer_sources,
  dependencies: gnome_calculator_tests_deps,
  link_with: [libcalculator, lib_mpfrg],
  include_directories: gnome_calculator_tests_includes,
)
test('Serializer test', test_serializer)

endif

if get_option('ui-tests') and get_option('gci') and get_option('gcalc')
gci_test_deps = [
	gee,
	gtk,
	inc_rooth_dep,
	gcalc_inc_libh_dep,
	gci_inc_libh_dep
]

blueprints = custom_target('blueprints',
  input: files(
    'gci-test-entry-controller.blp',
  ),
  output: '.',
  command: [find_program('blueprint-compiler'), 'batch-compile', '@OUTPUT@', '@CURRENT_SOURCE_DIR@', '@INPUT@'],
)

test_entry_controler_resource_files = files('test-entry-controller.gresource.xml')
test_entry_controler_resources = gnome.compile_resources('test-entry-controller-resources', test_entry_controler_resource_files, dependencies: blueprints)

test_entry_controler_sources = [
  'test-gci-entry-controller.vala',
]
test_entry_controler_sources += test_entry_controler_resources

test_entry_controler = executable('test-entry-controller', test_entry_controler_sources,
  dependencies: gci_test_deps,
  link_with: [libgci, libgcalc],
)
test('gci-entry-controller', test_entry_controler)
endif

endif

if get_option('gcalc')
tests_deps = [
  gio,
  glib,
  gobject,
  libmath,
  mpc,
  mpfr,
  posix,
  gee,
  inc_rooth_dep
]

test_parsing_sources = [
  'gcalc-parsing.vala',
]

test_parsing = executable('gcalc-parsing', test_parsing_sources,
  dependencies:tests_deps,
  link_with: [libgcalc, lib_mpfrg],
)
test('gcalc-parsing', test_parsing)


test_solve_basic_sources = [
  'gcalc-solving-basic.vala',
]

test_solve_basic = executable('gcalc-solve-basic', test_solve_basic_sources,
  dependencies:tests_deps,
  link_with: [libgcalc, lib_mpfrg],
)
test('gcalc-solve-basic', test_solve_basic)

endif
