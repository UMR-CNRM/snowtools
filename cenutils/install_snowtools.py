# -*- coding: utf-8 -*-

import argparse
import glob
import os
import shutil
import subprocess
import sys

# TODO : Use a proper API of the venv package
# see https://docs.python.org/3/library/venv.html


description = "Snowtools installation script for MF developpers"
parser = argparse.ArgumentParser(description=description)

parser.add_argument("-e", "--editable", action="store_true", help="Install editable version for onging developments")

parser.add_argument(
    "-v",
    "--venv",
    type=str,
    required=False,
    default=None,
    help="Path (relative or absolute) to the virtual environment to be created."
    "If this script is already called from a virtual environment,"
    "this argument is ignored.",
)

parser.add_argument(
    "-o",
    "--optional",
    action="store",
    choices=["plot", "sql", "all"],
    nargs="+",
    default=None,
    help="Install optional dependencies (this option is ignored on MF's HPC):\n"
    + "* 'plot' install graphical tools\n"
    + "* 'sql' install sql extraction tools\n"
    + "* 'all' install all optionnal dependencies",
)

args = parser.parse_args()

# Retrieve the snowtools root directory from the current script location
snowtools_dir = os.path.dirname(os.path.dirname(__file__))

# Retrieve server name to activate server-specific installation steps
HOSTNAME = os.getenv("HOSTNAME", "")

# Check for packages installed locally to issue a warning.
if glob.glob(os.path.join(os.environ["HOME"], ".local", "lib", "python*")):
    print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    print("WARNING: It looks like you have locally installed python packages.")
    print("You should re-install these packages in a dedicated virtual environment and remove them with:")
    print("rm -r $HOME/.local/lib/python*")
    print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")

# Virtual environment
# -------------------
outstr_header = (
    "=====================================================================\n"
    "                     INSTALLATION INFORMATION                        \n"
    "=====================================================================\n"
)

if sys.base_prefix == sys.prefix:
    # The script was NOT called from within a virtual environment

    if args.venv:
        # If the venv argument is provided, the user wants a virtual environment to be created

        venv = os.path.abspath(args.venv)

        # Security: An editable install in a virtual environment within the snowtools repository leads meson to crash
        # See related comment above
        if args.editable and snowtools_dir in venv:
            raise Exception(
                "For an editable install, the virtual environment must not be created "
                "in the snowtools root directory.\n"
                "Please choose a different path for the creation of your virtual environment"
            )

        if not os.path.isfile(os.path.join(venv, "bin", "pip")):
            # Create the virtual environment if it does not exist already
            from venv import create

            create(venv, with_pip=True)
            # TODO : ajouter un message pour dire comment activer l'environnement virtuel créé ?
            outstr = (
                outstr_header + "Snowtools has been installed in a new virtual environment.\n"
                "To activate it, run :\n"
                f"source {venv}/bin/activate"
            )
        else:
            # TODO : quel comportement dans le cas où l'environnement virtuel existe déjà ?
            # comportement actuel : on tente l'installation dans cet environnement
            outstr = (
                outstr_header + "Snowtools has been installed in an existing virtual environment.\n"
                "To activate it, run :\n"
                f"source {venv}/bin/activate"
            )

        # Activate the virtual environment
        os.environ["PATH"] = ":".join([os.path.join(venv, "bin"), os.environ["PATH"]])
        pip = os.path.join(venv, "bin", "pip")
        sys.prefix = venv
        sys.exec_prefix = venv

    else:
        raise SystemError(
            "It looks like you are not in a virtual environment.\n"
            "Please activate a virtual environment or create one with the -v/--venv argument."
        )
else:
    # The script was called from within a virtual environment
    outstr = outstr_header + "Snowtools has been installed in the current virtual environment."

    # Security : an editable install in a virtual environment within the snowtools repository leads meson to crash:
    # ```
    #      meson.build:38:9: ERROR: Tried to form an absolute path to a dir in the source tree.
    #      You should not do that but use relative paths instead, for
    #      directories that are part of your project.
    # ```
    if args.editable and snowtools_dir in sys.executable:
        raise Exception(
            "It looks like the current virtual environment is at the snowtools root directory.\n"
            "An editable install is not possible in this case.\n"
            "Please create your virtual environement elsewhere or install snowtools as non-editable"
        )

    venv = sys.prefix
    pip = "pip"

if args.optional is None or "hpc" in HOSTNAME:
    # Optional dependencies are unavailable on MF HPC (that is the reason they are optional)
    print("The '-o' argument will be ignored because optional dependencies are not available on MF HPC")
    optional = ""
else:
    optional = "[" + ",".join(args.optional) + "]"

if "-sidev" in HOSTNAME:
    # On SOPRANO servers, the following pip arguments are required to enable the connexion to PyPI
    pip_options = [
        "--trusted-host",
        "pypi.org",
        "--trusted-host",
        "pypi.python.org",
        "--trusted-host",
        "files.pythonhosted.org",
    ]
else:
    pip_options = list()

# Ensure to use the latest available pip version
print("Running command:")
print(f"{pip} install --upgrade pip")
subprocess.run([pip, "install"] + pip_options + ["--upgrade", "pip"])

# Snowtools installation
# ----------------------

os.chdir(snowtools_dir)
# Security : an existing "build" directory from a former installation may cause trouble
shutil.rmtree("build", ignore_errors=True)
shutil.rmtree(".mesonpy*", ignore_errors=True)

if args.editable:
    if sys.version_info < (3, 10, 1):
        raise SystemError("Editable install is not possible with python versions lower than 3.10")

    # Snowtools contains a compiled extension module written in Fortran.
    # In order to render compiled extension modules editable similarly to ordinary python code,
    # they are compiled at import time in an editable install rather than during
    # installation in case of a classical install (:ref:`sec-install_users`).
    # This means that the build dependencies have to be available at runtime in
    # the virtual environment and not just temporarily during the install.
    # The advantage is, that edits in the Fortran code trigger the (partial) re-compilation of
    # the extension module at the next import in a new interpreter instance.
    # https://mesonbuild.com/meson-python/how-to-guides/editable-installs.html
    print("Running command:")
    print(f"{pip} install {' '.join(pip_options)} numpy>=1.21.6 meson-python ninja")
    subprocess.run([pip, "install"] + pip_options + ["numpy>=1.21.6", "meson-python", "ninja"])

    # 'no-build-isolation' is required for an editable install
    pip_options.extend(["--no-build-isolation", "-e"])


# Install snowtools snowtools
# pip install [--no-build-isolation -e] .
print("Running command:")
cmd_install = f"{pip} install {' '.join(pip_options)} .{optional}"
print(cmd_install)

out = subprocess.run([pip, "install"] + pip_options + [f".{optional}"])
if out.returncode == 1:
    outstr = outstr_header + f"An error has occurred during the installation with {cmd_install}"


# Write latest snowtools commit number into the virtual environment to keep a track of what has just been installed
if os.path.isdir(".git"):
    commit = subprocess.check_output('git show --pretty=format:"%H" --no-patch', shell=True, encoding="utf-8")
    with open(os.path.join(venv, ".snowtools_info"), "a") as f:
        f.write(commit)
elif os.path.exists(".git_info"):
    shutil.copyfile(".git_info", os.path.join(venv, ".snowtools_info"))

print(outstr)
print()
