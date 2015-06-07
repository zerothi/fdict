#!/bin/bash

# Create link from .arch.make to
# arch.make

_old_arch=
if [ -L arch.make ]; then
    # We assume the arch.make is 
    # a link to .arch.make
    # Simply delete it, we will re-instantiate it
    rm arch.make
elif [ -e arch.make ]; then
    _old_arch=.temporary_arch.make
    mv arch.make $_old_arch
fi

# Create link to arch.make
ln -s .arch.make arch.make

# call make...
make clean
make
make check

if [ -z "$_old_arch" ]; then
    # the link should sustain,
    # it still links to .arch.make
    echo "do nothing" > /dev/null
else
    rm arch.make
    mv $_old_arch arch.make
fi