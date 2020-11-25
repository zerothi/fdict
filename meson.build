project('fdict', 'fortran',
  version : '0.7.2',
  license : 'MPLv2',
)
gensettings_files = files([
    'settings.bash',
    'default_settings.bash'
])
subdir('src')
fdict_lib = library('fdict', fdict_src, install: true)
fdict_dep = declare_dependency(link_with: fdict_lib, include_directories: fdict_inc)
