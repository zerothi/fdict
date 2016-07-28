#!/bin/bash

# Run installation
echo "CPP version"
cpp --version
echo "gfortran version"
gfortran --version
# Show help from Makefile
make show
# Actually build fdict
make
