#!/bin/bash

# install script for local machines with access to PyPI.
# The pypi access is needed to install build dependencies (meson, meson-python, ninja)
# not installed system wide.
# Does not work on the super computers because there is no access to pypi.

python3 -m venv venv_test --system-site-packages
source ./venv_test/bin/activate
pip install meson meson-python ninja
python3 -m numpy.f2py -c crps.f90 -m crps --lower # --build-dir bla
deactivate
rm -rf venv_test

