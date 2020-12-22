project('fdict', 'fortran',
  version : '0.7.2',
  license : 'MPLv2',
  default_options: ['default_library=static']
)
install = not meson.is_subproject()

gensettings_files = files([
    'settings.bash',
    'default_settings.bash'
])
root_dir = meson.current_source_dir()

subdir('src')

fdict_lib = library('fdict', fdict_src, install: install)
fdict_dep = declare_dependency(link_with: fdict_lib)
