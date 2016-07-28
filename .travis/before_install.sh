#!/bin/bash

# Do pre-install commands
if [ $TRAVIS_OS_NAME == "osx" ]; then
    brew update
    brew install gcc
else
    sudo apt-get install -qq gfortran
fi
