GCI_PROJECT_NAME='gci'
GCI_API_VERSION='1'
GCI_VERSIONED_PROJECT_NAME=GCI_PROJECT_NAME+'-'+GCI_API_VERSION
GCI_CAMEL_CASE_NAME='GCi'
GCI_VERSIONED_CAMEL_CASE_NAME=GCI_CAMEL_CASE_NAME+'-'+GCI_API_VERSION
GCI_GIR_NAME= GCI_VERSIONED_CAMEL_CASE_NAME+'.gir'
GCI_TYPELIB_NAME= GCI_VERSIONED_CAMEL_CASE_NAME+'.typelib'
GCI_VAPI_NAME = GCI_VERSIONED_PROJECT_NAME+'.vapi'

conf = configuration_data()
conf.set('prefix', get_option('prefix'))
conf.set('libdir', '${exec_prefix}/'+get_option ('libdir'))
conf.set('PROJECT_NAME', GCI_PROJECT_NAME)
conf.set('PROJECT_VERSION', meson.project_version ())
conf.set('API_VERSION', GCI_API_VERSION)

configure_file(input : 'gci.pc.in',
	output : 'gci-@0@.pc'.format(GCI_API_VERSION),
	configuration : conf,
	install : true,
	install_dir : join_paths(get_option('libdir'), 'pkgconfig'))

configure_file(input : 'gci.deps.in',
	output : 'gci-@0@.deps'.format(GCI_API_VERSION),
	configuration : conf,
	install : true,
	install_dir : vapidir)

gci_nsinfo = configure_file(input : 'namespace-info.vala.in',
	output : 'namespace-info.vala',
	configuration : conf)
gci_namespaceinfo_dep = declare_dependency (sources : gci_nsinfo)

gci_confh = configuration_data ()
gci_confh.set_quoted('PACKAGE_LOCALE_DIR', join_paths(get_option('prefix'), get_option('datadir'), 'locale'))
gci_confh.set_quoted('GETTEXT_PACKAGE', 'GCalc')
configure_file(output : 'config.h',
	configuration : gci_confh)


gci_sources = files([
	'gci-entry-controller.vala'
])


gci_inc_libh = include_directories ('.')
gci_inc_libh_dep = declare_dependency (include_directories : gci_inc_libh)
gci_build_dir = meson.current_build_dir ()
gci_sources_dir = meson.current_source_dir ()

gci_exported_deps = [
	gio,
	gcalc_inc_libh_dep,
	inc_rooth_dep,
	gci_inc_libh_dep,
	gee,
	gtk
]
gci_deps = [
	gci_namespaceinfo_dep,
]

gci_deps += gci_exported_deps

# LT_VERSION for ABI related changes
# From: https://autotools.io/libtool/version.html
# This rules applies to Meson 0.43
# Increase the current value whenever an interface has been added, removed or changed.
# Always increase revision value whenever an interface has been added, removed or changed.
# Increase the age value only if the changes made to the ABI are backward compatible.
# Set version to the value of subtract age from current
# Reset current and version to 1 and, age and version to 0 if library's name is changed
GCI_LT_CURRENT='0'
GCI_LT_REVISION='0'
GCI_LT_AGE='0'
GCI_LT_VERSION='0'
libgci = library(GCI_VERSIONED_PROJECT_NAME,
	gci_sources,
	version : GCI_LT_VERSION,
	soversion : GCI_LT_VERSION+'.'+GCI_LT_AGE+'.'+GCI_LT_REVISION,
	vala_header : GCI_PROJECT_NAME+'.h',
	vala_vapi : GCI_VAPI_NAME,
	vala_gir : GCI_GIR_NAME,
	dependencies : gci_deps,
	vala_args: [
	],
	c_args : [
		'-include',
		meson.current_build_dir() + '/config.h',
	],
	link_with: [ libgcalc ],
	install : true,
	install_dir : [
		true,
		join_paths (get_option('includedir'), 'gci-@0@'.format (API_VERSION), 'gci'),
		vapidir,
		true
	])

if introspection
if g_ir_compiler.found()
custom_target('gci-typelib',
	command: [
		g_ir_compiler,
		'--shared-library', 'lib'+GCI_PROJECT_NAME+'-@0@.so'.format (GCI_API_VERSION),
		'--output', '@OUTPUT@',
		join_paths(meson.current_build_dir(), GCI_GIR_NAME)
	],
	output: GCI_TYPELIB_NAME,
	depends: libgci,
	install: true,
	install_dir: join_paths(get_option('libdir'), 'girepository-1.0'))
endif
endif

libgci_dep = declare_dependency(include_directories : gci_inc_libh,
  link_with : libgci,
  dependencies: gci_exported_deps,
  )
