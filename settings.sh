#!/bin/bash

# Define here the number of dimensions for the different types
# Variables
declare -A N
N[v]=0 # variable
N[a]=0 # character
N[s]=3 # single
N[d]=3 # double
N[c]=3 # single complex
N[z]=3 # double complex
N[b]=3 # logical
N[i]=3 # integer
N[l]=3 # long integer

# Names of the different short-hands
# DONT change these
declare -A name
name[v]="type(var)"
name[a]="type(var_str)"
name[s]="real(sp)"
name[d]="real(dp)"
name[c]="complex(sp)"
name[z]="complex(dp)"
name[b]="logical"
name[i]="integer(is)"
name[l]="integer(il)"

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
	0)
	    ;;
	*)
	    echo "You are requesting a too large array size."
	    return 1
	    ;;
    esac
}
