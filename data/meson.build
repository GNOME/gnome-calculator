# Icons
install_subdir('icons',
  install_dir: join_paths(get_option('datadir')),
  exclude_files: [ 'Makefile', 'Makefile.in', 'Makefile.am', 'gnome-calculator-src.svg' ],
)

# Desktop file
i18n.merge_file(
  input: 'org.gnome.Calculator.desktop.in',
  output: 'org.gnome.Calculator.desktop',
  type: 'desktop',
  po_dir: po_dir,
  install: true,
  install_dir: join_paths(get_option('datadir'), 'applications'),
)

# Appdata
i18n.merge_file(
  input: 'org.gnome.Calculator.appdata.xml.in',
  output: 'org.gnome.Calculator.appdata.xml',
  type: 'xml',
  po_dir: po_dir,
  install: true,
  install_dir: join_paths(get_option('datadir'), 'metainfo')
)

# Gschema
install_data('org.gnome.calculator.gschema.xml',
  install_dir: join_paths(get_option('datadir'), 'appdata'),
)

# Manpage
install_man(
  'gnome-calculator.1',
  'gcalccmd.1'
)
