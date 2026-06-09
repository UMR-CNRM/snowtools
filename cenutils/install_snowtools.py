# -*- coding: utf-8 -*-

import os
import sys
import shutil
import glob
import argparse
import subprocess

# TODO : Use a proper API of the venv package
# see https://docs.python.org/3/library/venv.html

# TODO : Fix script avec python3.12.12 sur HPC


description = "Snowtools installation script for MF developpers"
parser = argparse.ArgumentParser(description=description)

parser.add_argument('-e', '--editable', action='store_true',
                    help="Install editable version for onging developments")

parser.add_argument('-v', '--venv', type=str, required=False, default=None,
                    help="Path (relative or absolute) to the virtual environment to be created."
                         "If this script is already called from a virtual environment,"
                         "this argument is ignored.")

parser.add_argument('-o', '--optional', choices=['plot', 'sql', 'all'], nargs='*', default=['all'],
                    help="Install optional dependencies (this option is ignored on MF's HPC):\n" +
                         "* 'plot' install graphical tools\n" +
                         "* 'sql' install sql extraction tools\n" +
                         "* 'all' install all optional dependencies")

parser.add_argument('--system-site-packages', help="Install system site packages (activate similar pip option)",
                    action='store_true')

args = parser.parse_args()


# Retrieve the snowtools root directory from the current script location
snowtools_dir = os.path.dirname(os.path.dirname(__file__))

# Retrieve server name to activate server-specific installation steps
HOSTNAME = os.getenv('HOSTNAME', '')

# Check for packages installed locally to issue a warning.
if glob.glob(os.path.join(os.environ['HOME'], '.local', 'lib', 'python*')):
    print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    print("WARNING: It looks like you have locally installed python packages.")
    print("You should re-install these packages in a dedicated virtual environment and remove them with:")
    print("rm -r $HOME/.local/lib/python*")
    print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")

# Virtual environment
# -------------------
outstr = '=====================================================================\n' \
         '                     INSTALLATION INFORMATION                        \n' \
         '=====================================================================\n'

if sys.base_prefix == sys.prefix:
    # The script was NOT called from within a virtual environment

    if args.venv:
        # If the venv argument is provided, the user wants a virtual environment to be created

        venv = os.path.abspath(args.venv)

        # Security: An editable install in a virtual environment within the snowtools repository leads meson to crash
        # See related comment above
        if args.editable and snowtools_dir in venv:
            raise Exception("For an editable install, the virtual environment must not be created "
                            "in the snowtools root directory.\n"
                            "Please choose a different path for the creation of your virtual environment")

        if not os.path.isfile(os.path.join(venv, 'bin', 'pip')):
            # Create the virtual environment if it does not exist already
            from venv import create
            if 'hpc' in HOSTNAME:
                # Do not create a virtual environment with system site packages on HPC
                create(venv, with_pip=True)
            else:
                create(venv, with_pip=True, system_site_packages=True)
            outstr = outstr + "Snowtools has been installed in a new virtual environment.\n" \
                "To activate it, run :\n" \
                f"source {venv}/bin/activate"
        else:
            # TODO : quel comportement dans le cas où l'environnement virtuel existe déjà ?
            # comportement actuel : on tente l'installation dans cet environnement
            outstr = outstr + "Snowtools has been installed in an existing virtual environment.\n" \
                "To activate it, run :\n" \
                f"source {venv}/bin/activate"

        # Activate the virtual environment
        os.environ['PATH'] = ':'.join([os.path.join(venv, 'bin'), os.environ['PATH']])
        pip = os.path.join(venv, 'bin', 'pip')
        sys.prefix = venv
        sys.exec_prefix = venv

    else:

        raise SystemError('It looks like you are not in a virtual environment.\n'
                'Please activate a virtual environment or create one with the -v/--venv argument.')
else:
    # The script was called from within a virtual environment
    outstr = outstr + "Snowtools has been installed in the current virtual environment."

    # Security : an editable install in a virtual environment within the snowtools repository leads meson to crash:
    # ```
    #      meson.build:38:9: ERROR: Tried to form an absolute path to a dir in the source tree.
    #      You should not do that but use relative paths instead, for
    #      directories that are part of your project.
    # ```
    if args.editable and snowtools_dir in sys.executable:
        raise Exception("It looks like the current virtual environment is at the snowtools root directory.\n"
                        "An editable install is not possible in this case.\n"
                        "Please create your virtual environement elsewhere or install snowtools as non-editable")

    venv = sys.prefix
    pip = 'pip'

if args.optional is None or 'hpc' in HOSTNAME:
    # Optional dependencies are unavailable on MF HPC (that is the reason they are optional)
    print("The '-o' argument will be ignored because optional dependencies are not available on MF HPC")
    optional = ''
else:
    optional = '[' + ','.join(args.optional) + ']'

if '-sidev' in HOSTNAME:
    # On SOPRANO servers, the following pip arguments are required to enable the connexion to PyPI
    pip_options = ['--trusted-host', 'pypi.org', '--trusted-host', 'pypi.python.org', '--trusted-host',
            'files.pythonhosted.org']
else:
    pip_options = list()

# Ensure to use the latest available pip version
print("Running command:")
print(f"{pip} install --upgrade pip")
subprocess.run([pip, 'install'] + pip_options + ['--upgrade', 'pip'])

# Get a proper version of setuptools (more than 66 -> editable, less than 71 to avoir bug)
print("Setuptools:")
subprocess.run([pip, 'install'] + ['setuptools>=66.0.0,<71.0.0'])

# Snowtools installation
# ----------------------

os.chdir(snowtools_dir)
# Security : an existing "build" directory from a former installation may cause trouble
# shutil.rmtree('build', ignore_errors=True)
# shutil.rmtree('.mesonpy*', ignore_errors=True)

if args.editable:

    if sys.version_info < (3, 10, 1):
        raise SystemError('Editable install is not possible with python versions lower than 3.10')

    # 'no-build-isolation' is required for an editable install
    pip_options.extend(['--no-build-isolation', '-e'])


# Install snowtools snowtools
# pip install [--no-build-isolation -e] .
print("Running command:")
print(f"{pip} install {' '.join(pip_options)} .{optional}")
subprocess.run([pip, 'install'] + pip_options + [f'.{optional}'])

# Write latest snowtools commit number into the virtual environment to keep a track of what has just been installed
if os.path.isdir('.git'):
    commit = subprocess.check_output('git show --pretty=format:"%H" --no-patch', shell=True, encoding='utf-8')
    with open(os.path.join(venv, '.snowtools_info'), 'w') as f:
        f.write(commit)
elif os.path.exists('.git_info'):
    shutil.copyfile('.git_info', os.path.join(venv, '.snowtools_info'))

# Temporary step for Belenos because packages on nexus are not available from HPC
if 'hpc' in HOSTNAME:
    HOME = os.getenv('HOME')
    subprocess.run([pip, 'install', f'{HOME}/Projects/mkjob/', f'{HOME}/Projects/vortex-gco',
        f'{HOME}/Projects/vortex-olive'])

# TODO : Crash if any subprces crashed

print(outstr)
print()
