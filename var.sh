#!/bin/bash

_vpath=.
if [ ! -z "$VPATH" ]; then
    _vpath=$VPATH
fi

source $_vpath/settings.sh
[ -e $_vpath/current_settings.sh ] && source $_vpath/current_settings.sh

# The different settings used in this
vars=(V s d c z b h i l)

ptr_declarations ${vars[@]} > var_declarations.inc
ptr_declarations -count 2 ${vars[@]} > var_declarations2.inc

# Print out to the mod file
{
for sub in assign associate associatd ; do
args="get set"
[ "$sub" == "associatd" ] && args="l r"
_psnl "interface $sub"
# Add the character(len=*)
[ "$sub" == "assign" ] && modproc $sub char 0 $args
# Add the variable
[ "$sub" != "associatd" ] && modproc $sub var ""
for v in ${vars[@]} ; do
    for d in `seq 0 $(var_N $v)` ; do
	modproc $sub $v $d $args
    done
done
_psnl "end interface $sub"
_psnl "public :: $sub"
done
if [ 1 -eq 0 ]; then
for sub in eq ne lt gt ge le ; do
_psnl "interface operator(.$sub.)"
for v in ${vars[@]} ; do
    for d in `seq 0 $(var_N $v)` ; do
	modproc $sub $v $d l r
    done
done
modproc $sub v 0
_psnl "end interface operator(.$sub.)"
_psnl "public :: operator(.$sub.)"
done
fi
} > var_interface.inc


{
for v in ${vars[@]} ; do
    for d in `seq 0 $(var_N $v)` ; do
	_psnl "if (this%t == '$v$d') then"
	_psnl "  p$v$d = transfer(this%enc,p$v$d)"
	_psnl "  deallocate(p$v$d%p)"
	_psnl "end if"
    done
done
# We define the type 'ut' as a "user-type"
_psnl "if (this%t == 'ut') then"
_psnl "print '(a)','var: Cannot deallocate UT, proceed:'"
_psnl "print '(a)','     1) retrieve type, 2) deallocate, 3) call nullify(var)'"
_psnl "end if"
} > var_delete.inc


{
for v in ${vars[@]} ; do
    for d in `seq 0 $(var_N $v)` ; do
	_psnl "if ( this%t == '$v$d' ) then"
	_psnl "#define DIM $d"
	_psnl '#include "settings.inc"'
	_psnl "p$v${d}_2 = transfer(rhs%enc,p$v${d}_2)"
	_psnl "ALLOC(p$v${d}_1%p,p$v${d}_2%p)"
	_psnl "#undef DIM"
	[ $d -lt $(var_N $v) ] && _ps "else"
    done
    _psnl "endif"
done
} > var_var_alloc.inc


{
for v in ${vars[@]} ; do
    for d in `seq 0 $(var_N $v)` ; do
	_psnl "if ( this%t == '$v$d' ) then"
	_psnl "p$v${d}_1%p = p$v${d}_2%p"
	_psnl "allocate(this%enc(size(transfer(p$v${d}_1, local_enc_type))))"
	_psnl "this%enc = transfer(p$v${d}_1, local_enc_type)"
	[ $d -lt $(var_N $v) ] && _ps "else"
    done
    _psnl "endif"
done
# In case the variable is a user-type, then we
# copy the encoding, probably this is a bit wearing, but
_psnl "if ( this%t == 'ut' ) then"
_psnl "print '(a)','var: Cannot assign a UT, USE call associate(..)'"
_psnl "end if"
} > var_var_set.inc


{
for v in ${vars[@]} ; do
    for d in `seq 0 $(var_N $v)` ; do
	_psnl "if ( this%t == '$v$d' ) then"
	_psnl "p$v${d}_1 = transfer(this%enc,p$v${d}_1)"
	_psnl "p$v${d}_2 = transfer(rhs%enc,p$v${d}_2)"
	_psnl "ret = associated(p$v${d}_1%p,p$v${d}_2%p)"
	[ $d -lt $(var_N $v) ] && _ps "else"
    done
    _psnl "endif"
done
# We check that the encoding is the same address
_psnl "if ( this%t == 'ut' ) then"
_psnl "ret = all(this%enc == rhs%enc)"
_psnl "end if"
} > var_var_assoc.inc


{
_psnl "#undef VAR_PREC"
for v in ${vars[@]} ; do
    _psnl "#define VAR_TYPE $(var_name $v)"
    for d in `seq 0 $(var_N $v)` ; do
	if [ $d -eq 0 ]; then
	    _psnl "#define DIMS"
	else
	    _psnl "#define DIMS , dimension$(dim_to_size $d)"
	fi
	_psnl "#define VAR $v$d"
	_psnl "#define DIM $d"
	_psnl '#include "var_funcs_inc.inc"'
	_psnl "#undef VAR"
	_psnl "#undef DIM"
	_psnl "#undef DIMS"
    done
    _psnl "#undef VAR_TYPE"
done
} > var_funcs.inc