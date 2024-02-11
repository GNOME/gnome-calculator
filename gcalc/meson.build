PROJECT_NAME='gcalc'
API_VERSION='2'
VERSIONED_PROJECT_NAME=PROJECT_NAME+'-'+API_VERSION
CAMEL_CASE_NAME='GCalc'
VERSIONED_CAMEL_CASE_NAME=CAMEL_CASE_NAME+'-'+API_VERSION
vapidir = join_paths (get_option('datadir'),'vala','vapi')
GIR_NAME= VERSIONED_CAMEL_CASE_NAME+'.gir'
TYPELIB_NAME= VERSIONED_CAMEL_CASE_NAME+'.typelib'
VAPI_NAME = VERSIONED_PROJECT_NAME+'.vapi'

conf = configuration_data()
conf.set('prefix', get_option('prefix'))
conf.set('libdir', '${exec_prefix}/'+get_option ('libdir'))
conf.set('PROJECT_NAME', PROJECT_NAME)
conf.set('PROJECT_VERSION', meson.project_version ())
conf.set('API_VERSION', API_VERSION)

configure_file(input : 'gcalc.pc.in',
	output : 'gcalc-@0@.pc'.format(API_VERSION),
	configuration : conf,
	install : true,
	install_dir : join_paths(get_option('libdir'), 'pkgconfig'))

configure_file(input : 'gcalc.deps.in',
	output : 'gcalc-@0@.deps'.format(API_VERSION),
	configuration : conf,
	install : true,
	install_dir : vapidir)

gcalc_nsinfo = configure_file(input : 'namespace-info.vala.in',
	output : 'namespace-info.vala',
	configuration : conf)
gcalc_namespaceinfo_dep = declare_dependency (sources : gcalc_nsinfo)

confh = configuration_data ()
confh.set_quoted('PACKAGE_LOCALE_DIR', join_paths(get_option('prefix'), get_option('datadir'), 'locale'))
confh.set_quoted('GETTEXT_PACKAGE', 'GCalc')
configure_file(output : 'config.h',
	configuration : confh)

gcalc_sources = files([
	'gcalc-assign.vala',
	'gcalc-calculator.vala',
	'gcalc-constant.vala',
	'gcalc-division.vala',
	'gcalc-equation.vala',
	'gcalc-equation-manager.vala',
	'gcalc-error-result.vala',
	'gcalc-expression.vala',
	'gcalc-expression-container.vala',
	'gcalc-expression-hash-map.vala',
	'gcalc-function.vala',
	'gcalc-function-acos.vala',
	'gcalc-function-acosh.vala',
	'gcalc-function-asin.vala',
	'gcalc-function-asinh.vala',
	'gcalc-function-atan.vala',
	'gcalc-function-atanh.vala',
	'gcalc-function-cos.vala',
	'gcalc-function-cosh.vala',
	'gcalc-function-exp.vala',
	'gcalc-function-log.vala',
	'gcalc-function-sin.vala',
	'gcalc-function-sinh.vala',
	'gcalc-function-sqrt.vala',
	'gcalc-function-tan.vala',
	'gcalc-function-tanh.vala',
	'gcalc-group.vala',
	'gcalc-hashable.vala',
	'gcalc-math-assign.vala',
	'gcalc-math-binary-operator.vala',
	'gcalc-math-constant.vala',
	'gcalc-math-constant-complex.vala',
	'gcalc-math-constant-number.vala',
	'gcalc-math-constant-vector.vala',
	'gcalc-math-division.vala',
	'gcalc-math-equation.vala',
	'gcalc-math-equation-manager.vala',
	'gcalc-math-error-result.vala',
	'gcalc-math-expression.vala',
	'gcalc-math-function.vala',
	'gcalc-math-group.vala',
	'gcalc-math-minus.vala',
	'gcalc-math-multiply.vala',
	'gcalc-math-operator.vala',
	'gcalc-math-parameter.vala',
	'gcalc-math-plus.vala',
	'gcalc-math-polynomial.vala',
	'gcalc-math-pow.vala',
	'gcalc-math-result.vala',
	'gcalc-math-term.vala',
	'gcalc-math-variable.vala',
	'gcalc-minus.vala',
	'gcalc-multiply.vala',
	'gcalc-parameter.vala',
	'gcalc-parser.vala',
	'gcalc-plus.vala',
	'gcalc-polynomial.vala',
	'gcalc-pow.vala',
	'gcalc-result.vala',
	'gcalc-solver.vala',
	'gcalc-term.vala',
	'gcalc-units.vala',
	'gcalc-variable.vala',
])


gcalc_inc_libh = include_directories ('.')
gcalc_inc_libh_dep = declare_dependency (include_directories : gcalc_inc_libh)
gcalc_build_dir = meson.current_build_dir ()
gcalc_sources_dir = meson.current_source_dir ()

gcalc_exported_deps = [
	gio,
	posix,
	libxml,
	libsoup,
	libmath,
	gee,
	inc_rooth_dep,
	gcalc_inc_libh_dep,
	libmpfr_inc_libh_dep,
]

gcalc_deps = [
	gcalc_namespaceinfo_dep,
]
gcalc_deps += gcalc_exported_deps

# LT_VERSION for ABI related changes
# From: https://autotools.io/libtool/version.html
# This rules applies to Meson 0.43
# Increase the current value whenever an interface has been added, removed or changed.
# Always increase revision value whenever an interface has been added, removed or changed.
# Increase the age value only if the changes made to the ABI are backward compatible.
# Set version to the value of subtract age from current
# Reset current and version to 1 and, age and version to 0 if library's name is changed
LT_CURRENT='1'
LT_REVISION='1'
LT_AGE='0'
LT_VERSION='1'
libgcalc = library(VERSIONED_PROJECT_NAME,
	gcalc_sources,
	version : LT_VERSION,
	soversion : LT_VERSION+'.'+LT_AGE+'.'+LT_REVISION,
	vala_header : PROJECT_NAME+'.h',
	vala_vapi : VAPI_NAME,
	vala_gir : GIR_NAME,
	dependencies : gcalc_deps,
	vala_args: [
		'--vapidir='+gcalc_vapi_dir,
		'--pkg=mpc',
		'--pkg=mpfr'
	],
	c_args : [
		'-include',
		meson.current_build_dir() + '/config.h',
	],
	link_with: [ lib_mpfrg ],
	install : true,
	install_dir : [
		true,
		join_paths (get_option('includedir'), 'gcalc-@0@'.format (API_VERSION), 'gcalc'),
		vapidir,
		true
	])

if introspection
g_ir_compiler = find_program('g-ir-compiler', required: false)
if g_ir_compiler.found()
custom_target('gcalc-typelib',
	command: [
		g_ir_compiler,
		'--shared-library', 'lib'+PROJECT_NAME+'-@0@.so'.format (API_VERSION),
		'--output', '@OUTPUT@',
		join_paths(meson.current_build_dir(), GIR_NAME)
	],
	output: TYPELIB_NAME,
	depends: libgcalc,
	install: true,
	install_dir: join_paths(get_option('libdir'), 'girepository-1.0'))
endif
endif

libgcalc_dep = declare_dependency(include_directories : gcalc_inc_libh,
  link_with : libgcalc,
  dependencies: gcalc_exported_deps,
  )
