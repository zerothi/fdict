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
vars=(a s d c z b h i l cp fp)

ptr_declarations ${vars[@]} > variable_declarations_.inc
{
    _psnl "type :: pta_"
    _psnl " type(pta__), pointer :: p(:) => null()"
    _psnl "end type pta_"
    _psnl "type :: pta__"
    _psnl " character(len=1), pointer :: p => null()"
    _psnl "end type pta__"
    _psnl "type(pta_) :: pa_"
} >> variable_declarations_.inc
ptr_declarations -count 2 ${vars[@]} > variable_declarations2_.inc
{
    _psnl "type :: pta_"
    _psnl " type(pta__), pointer :: p(:) => null()"
    _psnl "end type pta_"
    _psnl "type :: pta__"
    _psnl " character(len=1), pointer :: p => null()"
    _psnl "end type pta__"
    _psnl "type(pta_) :: pa__1, pa__2"
} >> variable_declarations2_.inc

# Print out to the mod file
{
for sub in assign associate associatd ; do
args="get set"
[ "$sub" == "associatd" ] && args="l r"
_psnl "interface $sub"
# Add the character(len=*)
case "$sub" in
    assign)
	# Add the character(len=*)
	modproc $sub a "0_0" get set
	# Add the variable
	modproc $sub var ""
	;;
    associate)
	# Add the variable
	modproc $sub var ""
	;;
esac
for v in ${vars[@]} ; do
    md=0
    [ $v == 'a' ] && md=1
    for d in `seq $md $(var_N $v)` ; do
	modproc $sub $v $d $args
    done
done
_psnl "end interface"
_psnl "public :: $sub"
done
if [ 1 -eq 0 ]; then
for sub in eq ne lt gt ge le ; do
_psnl "interface operator(.$sub.)"
for v in ${vars[@]} ; do
    md=0
    [ $v == 'a' ] && md=1
    for d in `seq $md $(var_N $v)` ; do
	modproc $sub $v $d l r
    done
done
modproc $sub v 0
_psnl "end interface"
_psnl "public :: operator(.$sub.)"
done
fi
} > variable_interface_.inc


{
for v in ${vars[@]} ; do
    md=0
    [ $v == 'a' ] && md=1
    for d in `seq $md $(var_N $v)` ; do
	_psnl "if (this%t == '$v$d') then"
	_psnl "  p$v$d = transfer(this%enc,p$v$d)"
	if [[ $v == "V" ]]; then
	    _psnl "  p$v$d%p = ''"
	fi
	_psnl "  deallocate(p$v$d%p)"
	_psnl "end if"
    done
done
} > variable_delete_.inc


{
for v in ${vars[@]} ; do
    md=0
    [ $v == 'a' ] && md=1
    for d in `seq $md $(var_N $v)` ; do
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
} > variable_variable_alloc_.inc


{
for v in ${vars[@]} ; do
    md=0
    [ $v == 'a' ] && md=1
    for d in `seq $md $(var_N $v)` ; do
	_psnl "if ( this%t == '$v$d' ) then"
	_psnl "p$v${d}_1%p = p$v${d}_2%p"
	_psnl "allocate(this%enc(size(transfer(p$v${d}_1, local_enc_type))))"
	_psnl "this%enc(:) = transfer(p$v${d}_1, local_enc_type)"
	[ $d -lt $(var_N $v) ] && _ps "else"
    done
    _psnl "endif"
done
# In case the variable is a user-type, then we
# copy the encoding, probably this is a bit wearing, but
_psnl "if ( this%t == 'USER' ) then"
_psnl "write(*,'(a)') 'var: Cannot assign a UT, USE call associate(..)'"
_psnl "end if"
} > variable_variable_set_.inc


{
for v in ${vars[@]} ; do
    md=0
    [ $v == 'a' ] && md=1
    for d in `seq $md $(var_N $v)` ; do
	_psnl "if ( this%t == '$v$d' ) then"
	_psnl "p$v${d}_1 = transfer(this%enc,p$v${d}_1)"
	_psnl "p$v${d}_2 = transfer(rhs%enc,p$v${d}_2)"
	_psnl "ret = associated(p$v${d}_1%p,p$v${d}_2%p)"
	[ $d -lt $(var_N $v) ] && _ps "else"
    done
    _psnl "endif"
done
# We check that the encoding is the same address
_psnl "if ( this%t == 'USER' ) then"
_psnl "ret = all(this%enc == rhs%enc)"
_psnl "end if"
} > variable_variable_assoc_.inc


{
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
	_psnl '#include "variable_funcs_inc.inc"'
	_psnl "#undef VAR"
	_psnl "#undef DIM"
	_psnl "#undef DIMS"
    done
    _psnl "#undef VAR_TYPE"
done
} > variable_funcs_.inc
