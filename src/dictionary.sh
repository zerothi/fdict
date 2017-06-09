#!/bin/bash

_vpath=..
if [ ! -z "$VPATH" ]; then
    _vpath=$VPATH
fi

source $_vpath/default_settings.bash
if [ $? -ne 0 ]; then
    echo "ERROR Could not find: $_vpath/default_settings.bash"
    exit 1
fi
[ -e $_vpath/settings.bash ] && source $_vpath/settings.bash

# The different settings used in this
vars=(a s d c z b h i l)

{
_psnl "interface operator(.KV.)"
# Add character
modproc dict_kv a 0_0
# Dictionaries are not allowed to be passed by value
# Add variable
modproc dict_kv var ""
for v in ${vars[@]} ; do
    md=0
    [ $v == 'a' ] && md=1
    for d in `seq $md $(var_N $v)` ; do
	modproc dict_kv $v $d
    done
done
_psnl "end interface"
_psnl "interface operator(.KVP.)"
# Add variable
modproc dict_kvp var ""
# Add dictionary
modproc dict_kvp dict ""
for v in ${vars[@]} ; do
    md=0
    [ $v == 'a' ] && md=1
    for d in `seq $md $(var_N $v)` ; do
	modproc dict_kvp $v $d
    done
done
_psnl "end interface"

_psnl "interface assign"
_psnl "module procedure dict_get_val"
_psnl "module procedure dict_get_val_a_"
# ! dict_key2dict is not allowed as
# ! the user might assume that all variables
# ! are copied. They are not, hence the user
# ! (for now) *MUST* do their own copying.
# !module procedure dict_key2dict
for v in ${vars[@]} ; do
    md=0
    [ $v == 'a' ] && md=1
    for d in `seq $md $(var_N $v)` ; do
	modproc dict_get_val $v $d
	modproc dict_get_val_first $v $d
    done
done
_psnl "end interface"

_psnl "interface associate"
_psnl "module procedure dict_get_p_val"
_psnl "module procedure dict_get_p_dict"
for v in ${vars[@]} ; do
    md=0
    [ $v == 'a' ] && md=1
    for d in `seq $md $(var_N $v)` ; do
	modproc dict_get_p $v $d
	modproc dict_get_p_first $v $d
    done
done
_psnl "end interface"
} > dict_interface.inc


{
_psnl '#include "settings.inc"'
_psnl "#undef VAR_PREC"
for v in ${vars[@]} ; do
    _psnl "#define VAR_TYPE $(var_name $v)"
    md=0
    [ $v == 'a' ] && md=1
    for d in `seq $md $(var_N $v)` ; do
	if [ $d -eq 0 ]; then
	    _psnl "#define DIMS"
	else
	    _psnl "#define DIMS , dimension$(dim_to_size $d)"
	fi
	_psnl "#define VAR $v$d"
	_psnl "#define DIM $d"
	_psnl '#include "dict_funcs_inc.inc"'
	_psnl "#undef VAR"
	_psnl "#undef DIM"
	_psnl "#undef DIMS"
    done
    _psnl "#undef VAR_TYPE"
done
} > dict_funcs.inc
