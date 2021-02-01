project('fdict', 'fortran',
  version : '0.7.2',
  license : 'MPLv2',
  default_options: ['default_library=static']
)
install = not meson.is_subproject()
root_dir = meson.current_source_dir()

gensettings_files = [
  root_dir / 'settings.bash',
] + files([
    'default_settings.bash'
])

subdir('src')

fdict_lib = library('fdict', fdict_src, install: install)
fdict_dep = declare_dependency(link_with: fdict_lib)
