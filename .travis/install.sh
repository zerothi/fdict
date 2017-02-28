#!/bin/bash

# Run installation
echo "CPP version"
cpp --version
echo "gfortran version"
gfortran --version
cd obj
# Show help from Makefile
make show
# Actually build fdict
if [ -z "$FDICT_OPT" ]; then
   OPT=0
else
   OPT="$FDICT_OPT"
fi
make OPT=$OPT
