valadoc = find_program ('valadoc', required: false)
if valadoc.found()

if get_option('gcalc')
	gtkdoc_outdir = CAMEL_CASE_NAME+'-'+API_VERSION
	docsdir = join_paths (get_option ('datadir'), 'devhelp','books')
	custom_target ('libgcalc_valadocs',
			input : gcalc_sources,
			output : CAMEL_CASE_NAME+'-'+API_VERSION,
			command : [valadoc,
				'--doclet=devhelp',
				'--force',
				'--package-name='+CAMEL_CASE_NAME+'-'+API_VERSION,
				'--package-version='+meson.project_version(),
				'--vapidir='+gcalc_sources_dir,
				'--vapidir='+gcalc_build_dir,
				'--vapidir='+libmpfr_vapi_dir,
				'--pkg=glib-2.0',
				'--pkg=gio-2.0',
				'--pkg=gee-0.8',
				'--vapidir='+vapis_dir,
				'--pkg=mpc',
				'--pkg=mpfr',
				'--pkg=mpfrg',
				'--directory=@OUTDIR@',
				'@INPUT@'
				],
			install : true,
			install_dir : docsdir,
			depends: libgcalc
			)
endif

if not get_option('disable-ui') and get_option('gci')  and get_option('gcalc')
	gtkdoc_outdir = GCI_CAMEL_CASE_NAME+'-'+GCI_API_VERSION
	docsdir = join_paths (get_option ('datadir'), 'devhelp','books')

	custom_target ('libgci_valadocs',
			input : gci_sources,
			output : GCI_CAMEL_CASE_NAME+'-'+GCI_API_VERSION,
			command : [valadoc,
				'--doclet=devhelp',
				'--force',
				'--package-name='+GCI_CAMEL_CASE_NAME+'-'+GCI_API_VERSION,
				'--package-version='+meson.project_version(),
				'--vapidir='+gcalc_build_dir,
				'--vapidir='+gci_build_dir,
				'--pkg=glib-2.0',
				'--pkg=gio-2.0',
				'--pkg=gee-0.8',
				'--vapidir='+vapis_dir,
				'--pkg=gcalc-'+API_VERSION,
				'--pkg=gtk4',
				'--directory=@OUTDIR@',
				'@INPUT@'
				],
			install : true,
			install_dir : docsdir,
			depends: libgcalc
			)
endif

endif
