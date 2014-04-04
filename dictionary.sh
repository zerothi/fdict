#!/bin/bash

source settings.sh

# The different settings used in this
vars=(s d c z b i l)


{
_psnl "interface operator(.KV.)"
for v in a ${vars[@]} ; do
    for d in `seq 0 ${N[$v]}` ; do
	modproc dict_kv $v $d
    done
done
_psnl "end interface"
_psnl "interface operator(.KVP.)"
for v in ${vars[@]} ; do
    for d in `seq 0 ${N[$v]}` ; do
	modproc dict_kvp $v $d
    done
done
_psnl "end interface"
} > dict_interface.inc


{
_psnl '#include "settings.inc"'
_psnl "#undef VAR_PREC"
for v in ${vars[@]} ; do
    _psnl "#define VAR_TYPE ${name[$v]}"
    for d in `seq 0 ${N[$v]}` ; do
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