#!/bin/bash

# Do pre-install commands
echo "OS-name: $TRAVIS_OS_NAME"
case "$TRAVIS_OS_NAME" in
   osx)
    brew update
    ;;
   *)
    sudo apt-get install -qq gfortran
    ;;
esac
