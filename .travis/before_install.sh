#!/bin/bash

# Do pre-install commands
case "$TRAVIS_OS_NAME" in
   osx)
    brew update
    ;;
   *)
    sudo apt-get install -qq gfortran
    ;;
esac
