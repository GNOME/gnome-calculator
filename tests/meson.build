# Common
gnome_calculator_tests_deps = [
  gio,
  glib,
  gobject,
  gtk,
  gtksourceview,
  libmath,
  mpc,
  mpfr,
  posix,
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
  link_with: libcalculator,
  include_directories: gnome_calculator_tests_includes,
)
test('Equation test', test_equation)

test_number_sources = [
  'test-number.vala',
]
test_number = executable('test-number', test_number_sources,
  dependencies: gnome_calculator_tests_deps,
  link_with: libcalculator,
  include_directories: gnome_calculator_tests_includes,
)
test('Number test', test_number)

test_serializer_sources = [
  'test-serializer.vala',
]
test_serializer = executable('test-serializer', test_serializer_sources,
  dependencies: gnome_calculator_tests_deps,
  link_with: libcalculator,
  include_directories: gnome_calculator_tests_includes,
)
test('Serializer test', test_serializer)
