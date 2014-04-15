#!/bin/bash

source settings.sh
rm -f current_settings.sh

function quick_setup {
    local n=$1 ; shift
    while [ $# -gt 0 ]; do
	echo "Updating number of dimensions for: ${name[$1]} to $n"
	echo "N[$1]=$n" >> current_settings.sh
	shift
    done
}
	 	
function _help {
    echo "Calling $0 consists of supplying arguments to control"
    echo "number of dimensions for each variable."
    echo "Option should be one of:"
    for v in -s -d -c -z -b -i -l -A -R -C -I ; do
	echo -n "$v <num> "
    done
    echo ""
}

[ $# -eq 0 ] && _help

while [ $# -gt 0 ]; do
    opt=$1 ; shift
    case $opt in
	--*)
	    opt=${opt:1} ;;
	-*)
	    ;;
	*)
	    echo "Error, could not recognize option"
	    _help ; exit
	    ;;
    esac
    case $opt in 
	-s|-d|-c|-z|-b|-i|-l)
	    n=${opt:1}
	    quick_setup $1 $n
	    shift ;;
	-A)
	    quick_setup $1 s d c z b i l
	    shift ;;
	-R)
	    quick_setup $1 s d
	    shift ;;
	-C)
	    quick_setup $1 c z
	    shift ;;
	-I)
	    quick_setup $1 i l
	    shift ;;
	-h|-help)
	    _help ; exit ;;
	*)
	    ;;
    esac
done

