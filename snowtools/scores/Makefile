

belenos:

# install script to be used on super computers.
# may work on other machines without the first line if
# the python version is <3.12 or meson is installed on the system
	exec `module load python`
	python3 -m numpy.f2py -c crps.f90 -m crps --lower

ubuntu:

# install script for local machines with access to PyPI.
# The pypi access is needed to install build dependencies (meson, meson-python, ninja)
# not installed system wide.
# Does not work on the super computers because there is no access to pypi.

	python3 -m venv venv_test --system-site-packages
	./venv_test/bin/pip install ../../
	cp ./venv_test/lib/python3.*/site-packages/snowtools/scores/crps.*.so .

	rm -rf venv_test