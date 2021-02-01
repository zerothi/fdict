# For variablexxx and dictionaryxxx, the process (from make) is to run variable/dictionary.sh,
# which produces some .inc files. Then the preprocessor must be run *manually* and then 
# the sed filter applied to produce the final .F90 file.
# The reason for the need for sed is something to do with preprocesor nesting being disallowed.
# Presumably for the same reason this is not simple to achieve in meson either.
# The step in the makefile is essentially:
# variable.f90: variable_pp.F90 ...
# $(FPP) $(FPPFLAGS) $(INCLUDES) -I$(src_DIR) -I. $< | sed -f $(src_DIR)/filter.sed > $@
# expanding (for me) to gfortran -E -P -x c  -I./src -I./src -I. variable_pp.F90 [...]
src_dir = meson.current_source_dir()

preprocessor_args = (
    meson.get_compiler('fortran').cmd_array()
    + ['-E', '-P', '-x', 'c', '-I.', '-I@0@'.format(root_dir), '-I./src', '-I@0@'.format(src_dir), '@INPUT0@', '-o', '@OUTPUT@']
)
sedfiltersrc = files(['filter.sed'])[0]
sed_f = [find_program('sed'), '-f', sedfiltersrc]
shell_w_vpath = [find_program('env'), 'VPATH=@0@'.format(root_dir)]

# Meson does not properly support multiple outputs from configure_file, but we list them here anyway.
# TODO: Use depfile for gensettings_files + var_inc_deps (to automatically reconfigure upon change).
# We cannot use custom_target because meson needs to scan the fortran files for proper dependency resolution
# at configure time, and custom_target is a build time target. So we must use configure_file to generate
# fortran sources.
var_inc_files = [
    'variable_declarations_.inc',
    'variable_declarations2_.inc',
    'variable_interface_.inc',
    'variable_delete_.inc',
    'variable_variable_alloc_.inc',
    'variable_variable_set_.inc',
    'variable_variable_assoc_.inc',
    'variable_funcs_.inc'
]
var_inc_deps = files(['settings.inc', 'variable_funcs_inc.inc'])
var_incfile_src = configure_file(
    command: shell_w_vpath + [find_program('variable.sh')],
    output: var_inc_files[-1]
)
var_sed_src = configure_file(
    command: preprocessor_args,
    input: files(['variable_pp.F90']) + [var_incfile_src],
    output: 'variable_sed.F90'
)
var_src = configure_file(
    command: sed_f + ['@INPUT@'],
    capture: true,
    input: var_sed_src,
    output: 'variable.F90'
)

# Same comments as the above for `variable.F90` applies for `dictionary.F90`
dict_inc_files = [
    'dictionary_interface_.inc',
    'dictionary_funcs_.inc'
]
dict_inc_deps = files(['settings.inc', 'dictionary_funcs_inc.inc'])
dict_incfile_src = configure_file(
    command: shell_w_vpath + [find_program('dictionary.sh')],
    output: dict_inc_files[-1]
)
dict_sed_src = configure_file(
    command: preprocessor_args,
    input: files(['dictionary_pp.F90']) + [dict_incfile_src],
    output: 'dictionary_sed.F90'
)
dict_src = configure_file(
    command: sed_f + ['@INPUT0@'],
    capture: true,
    input: [dict_sed_src],
    output: 'dictionary.F90'
)

fdict_src = [var_src, dict_src]
