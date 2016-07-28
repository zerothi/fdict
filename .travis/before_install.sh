#!/bin/bash

# Do pre-install commands
if [ "x$TRAVIS_OS_NAME" == "xosx" ]; then
    brew update
else
    sudo apt-get install -qq gfortran
fi
