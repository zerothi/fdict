#!/bin/bash

# Do pre-install commands
if [ $TRAVIS_OS_NAME == "osx" ]; then
    brew update
    if ! `which gfortran` ; then
        brew install gcc
    fi
else
    sudo apt-get install -qq gfortran
fi
