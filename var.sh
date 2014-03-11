#!/bin/bash

# Define here the number of dimensions for the 5 different
# Variables
declare -A N
N[a]=0 # character
N[s]=3 # single
N[d]=3 # double
N[c]=3 # single complex
N[z]=3 # double complex
N[b]=3 # logical
N[i]=3 # integer
N[l]=3 # long integer

# Names of the different short-hands
declare -A name
name[a]="character(250)"
name[s]="real(sp)"
name[d]="real(dp)"
name[c]="complex(sp)"
name[z]="complex(dp)"
name[b]="logical"
name[i]="integer(is)"
name[l]="integer(il)"


vars=(s d c z b i l)

function _ps { printf "%b" "$@" ; }
function _psnl { printf "%b\n" "$@" ; }
function interface {
    local sub=$1 ; shift
    local sn=$1  ; shift
    local dim=$1 ; shift
    while [ $# -gt 0 ]; do
	_psnl "module procedure ${sub}_${1}_$sn$dim"
	shift
    done
}

# Print out to the mod file
{
for sub in assign associate ; do
echo interface $sub 
for v in ${vars[@]} ; do
    for d in `seq 0 ${N[$v]}` ; do
	interface $sub $v $d get set
    done
done
echo end interface $sub
done
if [ 1 -eq 0 ]; then
for sub in eq ne lt gt ge le ; do
echo "interface operator(.$sub.)"
for v in ${vars[@]} ; do
    for d in `seq 0 ${N[$v]}` ; do
	interface $sub $v $d l r
    done
done
echo "end interface operator(.$sub.)"
done
fi
} > mods.inc

{
for v in ${vars[@]} ; do
    for d in `seq 0 ${N[$v]}` ; do
	_psnl "nullify(this%$v$d)"
    done
done
} > nullify.inc

{
for v in ${vars[@]} ; do
    for d in `seq 0 ${N[$v]}` ; do
	_psnl "if (associated(this%$v$d)) deallocate(this%$v$d)"
    done
done
_psnl "call nullify(this)"
} > delete.inc

function dim_to_size {
    case $1 in
	1)
	    _ps "(:)" ;;
	2)
	    _ps "(:,:)" ;;
	3)
	    _ps "(:,:,:)" ;;
	4)
	    _ps "(:,:,:,:)" ;;
	5)
	    _ps "(:,:,:,:,:)" ;;
	6)
	    _ps "(:,:,:,:,:,:)" ;;
    esac
}

{
for v in ${vars[@]} ; do
    _ps "${name[$v]}, pointer :: "
    for d in `seq 0 ${N[$v]}` ; do
	_ps "$v$d$(dim_to_size $d)=>null()"
	if [ $d -lt ${N[$v]} ]; then
	    _ps ", "
	else
	    _psnl ""
	fi
    done
done
} > types.inc

{
_psnl "#include 'var.inc'"
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
	_psnl "#include 'funcs_inc.inc'"
	_psnl "#undef VAR"
	_psnl "#undef DIM"
	_psnl "#undef DIMS"
    done
    _psnl "#undef VAR_TYPE"
done
} > funcs.inc