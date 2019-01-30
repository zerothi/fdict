#!/bin/bash

# Define here the number of dimensions for the different types
# Variables
function var_N {
    local var=$1 ; shift
    case $var in
	VAR) _ps 0 ;;
	a) _ps 1 ;;
	cp|fp) _ps 1 ;;
	*) _ps 3 ;;
    esac
}
	
#declare -A N
#N[VAR]=0 # variable
#N[a]=0 # character
#N[s]=3 # single
#N[d]=3 # double
#N[c]=3 # single complex
#N[z]=3 # double complex
#N[b]=3 # logical
#N[h]=3 # short
#N[i]=3 # integer
#N[l]=3 # long integer
#N[cp]=3 # type(c_ptr)
#N[fp]=3 # type(c_funptr)


# Names of the different short-hands
# DONT change these
function var_name {
    local var=$1 ; shift
    case $var in
	VAR) _ps "type(variable_t)" ;;
	a) _ps "character(len=1)" ;;
	s) _ps "real(sp)" ;;
	d) _ps "real(dp)" ;;
	c) _ps "complex(sp)" ;;
	z) _ps "complex(dp)" ;;
	b) _ps "logical" ;;
	h) _ps "integer(ih)" ;;
	i) _ps "integer(is)" ;;
	l) _ps "integer(il)" ;;
	cp) _ps "type(c_ptr)" ;;
	fp) _ps "type(c_funptr)" ;;
    esac
}
#declare -A name
#name[VAR]="type(variable_t)"
#name[a]="character(len=*)"
#name[s]="real(sp)"
#name[d]="real(dp)"
#name[c]="complex(sp)"
#name[z]="complex(dp)"
#name[b]="logical"
#name[h]="integer(ih)"
#name[i]="integer(is)"
#name[l]="integer(il)"
#name[cp]="type(c_ptr)"
#name[fp]="type(c_funptr)"

function _ps { printf "%b" "$@" ; }
function _psnl { printf "%b\n" "$@" ; }
function modproc {
    local sub=$1 ; shift
    local sn=$1  ; shift
    local dim=$1 ; shift
    if [ $# -eq 0 ]; then
	_psnl "module procedure ${sub}_$sn$dim"
    else
	while [ $# -gt 0 ]; do
	    _psnl "module procedure ${sub}_${1}_$sn$dim"
	    shift
	done
    fi
}

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
	7)
	    _ps "(:,:,:,:,:,:,:)" ;;
	0)
	    ;;
	*)
	    echo "You are requesting a too large array size."
	    return 1
	    ;;
    esac
}

function ptr_declarations {
    local v
    local md
    local count=1
    if [ $1 == "-count" ]; then
	shift
	count=$1
	shift
    fi
    while [ $# -gt 0 ]; do
	v=$1 ; shift
	md=0
	[ $v == 'a' ] && md=1
	for d in `seq $md $(var_N $v)` ; do
	    _psnl "type :: pt$v$d"
	    _psnl " $(var_name $v), pointer :: p$(dim_to_size $d) => null()"
	    _psnl "end type pt$v$d"
	    _ps "type(pt$v$d) :: "
	    if [ $count -eq 1 ]; then
		_psnl "p$v${d}"
	    else
		for i in `seq 1 $count` ; do
		    _ps "p$v${d}_$i"
		    if [ $i -lt $count ]; then
			_ps ", "
		    fi
		done
		_psnl ""
	    fi
	done
    done
}
