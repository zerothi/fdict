#!/bin/bash

# Do pre-install commands
echo "OS-name: $TRAVIS_OS_NAME"

function fdict_osx {
    brew update
    brew install gfortran
    {
      echo CPP = gcc -E -P -x c
    } > setup.make
}

function fdict_linux {
    sudo apt-get install -qq gfortran
}

case "$TRAVIS_OS_NAME" in
   osx)
    fdict_osx
    ;;
   *)
    fdict_linux
    ;;
esac

mkdir obj
cd obj
# Common flags for code-coverage
{
    echo FFLAGS = -fprofile-arcs -ftest-coverage
} >> setup.make

{
    echo "TOP_DIR=.."
    echo "include ../Makefile"
} > Makefile
