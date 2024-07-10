#!/bin/sh
# install script to be used on super computers.
# may work on other machines without the first line if
# the python version is <3.12 or meson is installed on the system
module load python
python -m numpy.f2py -c ./snowtools/scores/crps.f90 -m crps --lower
mv crps.*.so ./snowtools/scores/
