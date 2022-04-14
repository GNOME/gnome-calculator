
lib_mpfrg_sources = files([
  'mpfr-glue.vala'
])

libmpfr_inc_libh = include_directories ('.')
libmpfr_inc_libh_dep = declare_dependency (include_directories : libmpfr_inc_libh)

libmpfr_vapi_dir = meson.current_build_dir()

lib_mpfrg = static_library ('mpfrg',
	lib_mpfrg_sources,
	vala_header: 'mpfrg.h',
	vala_vapi: 'mpfrg.vapi',
	dependencies: [
		gio,
		mpc,
		mpfr,
	],
	install: false,
)
