# Icons
scalable_iconsdir = join_paths('icons', 'hicolor', 'scalable', 'apps')
install_data(
  join_paths(scalable_iconsdir, '@0@.svg'.format(app_id)),
  install_dir: join_paths(get_option('datadir'), scalable_iconsdir)
)

symbolic_iconsdir = join_paths('icons', 'hicolor', 'symbolic', 'apps')
install_data(
  join_paths(symbolic_iconsdir, 'org.gnome.Calculator-symbolic.svg'),
  install_dir: join_paths(get_option('datadir'), symbolic_iconsdir),
  rename: '@0@-symbolic.svg'.format(app_id)
)

# Desktop file
desktop_file_in_config = configuration_data()
desktop_file_in_config.set('app_id', app_id)
desktop_file_in = configure_file(
          input: 'org.gnome.Calculator.desktop.in',
         output: '@0@.desktop.in'.format(app_id),
  configuration: desktop_file_in_config,
)

desktop_file = i18n.merge_file(
        input: desktop_file_in,
       output: '@0@.desktop'.format(app_id),
         type: 'desktop',
       po_dir: po_dir,
      install: true,
  install_dir: join_paths(get_option('datadir'), 'applications')
)

# Appdata
appconf = configuration_data()
appconf.set('app_id', app_id)
metainfo_file_in = configure_file(
          input: 'org.gnome.Calculator.metainfo.xml.in',
         output: '@0@.metainfo.xml.in'.format(app_id),
  configuration: appconf,
)

metainfo_file = i18n.merge_file(
          input: metainfo_file_in,
         output: '@0@.metainfo.xml'.format(app_id),
         po_dir: po_dir,
        install: true,
    install_dir: join_paths(get_option('datadir'), 'metainfo')
)

# Validate metainfo
appstreamcli = find_program('appstreamcli', required: false)
if (appstreamcli.found())
  test('Validating appdata',
    appstreamcli,
    args: ['validate', '--no-net', '--explain', metainfo_file],
    workdir: meson.current_build_dir()
  )
endif

# Gschema
install_data('org.gnome.calculator.gschema.xml',
  install_dir: join_paths(get_option('datadir'), 'glib-2.0/schemas'),
)

# Manpage
install_man(
  'gnome-calculator.1',
  'gcalccmd.1'
)
