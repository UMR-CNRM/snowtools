# -*- coding: utf-8 -*-

import os
import sys
import shutil
import argparse


description = "Snowtools installation script for MF developpers"
parser = argparse.ArgumentParser(description=description)

parser.add_argument('-e', '--editable', action='store_true',
                    help="Install editable version for onging developments")

parser.add_argument('-o', '--optional', action='store', choices=['plot', 'sql', 'all'], default=None,
                    help="Install optional dependencies (this option is ignored on MF's HPC):\n" +
                         "* 'plot' install graphical tools\n" +
                         "* 'sql' install sql extraction tools\n" +
                         "* 'all' install all optionnal dependencies")

args = parser.parse_args()

snowtools_dir = os.path.dirname(os.path.dirname(__file__))
os.chdir(snowtools_dir)
shutil.rmtree('build', ignore_errors=True)

HOSTNAME = os.getenv('HOSTNAME', '')

if args.optional is None or 'hpc' in HOSTNAME:
    optional = ''
else:
    optional = f'[{args.optional}]'

if '-sidev' in HOSTNAME:
    pip_options = '--trusted-host pypi.org --trusted-host pypi.python.org --trusted-host files.pythonhosted.org '
else:
    pip_options = ''

if args.editable:

    if sys.version_info < (3, 10, 1):
        raise SystemError('Editable install is not possible with python versions lower than 3.10')

    # For an editable install, the build dependencies have to be available at runtime in
    # the virtual environment and not just temporarily during the install
    os.system(f'pip install {pip_options} numpy>=1.21.6 meson-python ninja')

    # 'no-build-isolation' is required for an editable install
    pip_options = pip_options + '--no-build-isolation -e '


# Install snowtools snowtools
os.system(f'pip install {pip_options} .{optional}')
