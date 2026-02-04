.. _sec-install:

Install Snowtools
=================

.. note::

   If you are using snowtools on Meteo-France computers, go directly to the last section (:ref:`sec-install_dev`).

Dependencies
------------

The snowtools project is mainly designed for a Linux environment.

The whole project requires at least python 3.6 and classical scientific packages (``numpy``, ``netCDF4``...). Some specific parts of the code (especially tests, scores, documentation generation require an extended set of dependencies that are fully described in ``requirements.txt``. The vortex toolbox is also needed for run on Meteo-France super-computers.

.. note::
   On Meteo-France computers, you do not have take care of dependecies that are already on your professional computer !


Install dependencies with pip
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Most of the packages can easily be installed in a virtual environment with ``pip`` (for instance with ``pip install -r requirements.txt``). This is useless if you plan to then install snowtools with  ``pip``.

Only GDAL python binding need to be installed manually to be installed consistently with your installed ``libgdal-dev`` version. Please install before the system packages ``ligdal`` and ``libgdal-dev`` (or similar) and run: ``pip install GDAL==$(gdal-config --version) --global-option=build_ext --global-option="$(gdal-config --cflags)"``.

.. _sec-install_users:

Snowtools install for users
---------------------------

If you are only a user of snowtools, you can install the package easily with pip in a virtual environment:

1. Download the source code: ``git clone https://github.com/UMR-CNRM/snowtools.git``
2. Create a virtual environment : ``python3 -m venv <name_of_your_virtual_env>`` or ``python3 -m venv --system-site-packages <name_of_your_virtual_env>``
3. Enter in the virtual environment:  ``source <name_of_your_virtual_env>/bin/activate``
4. Ensure you are at the root of the snowtools repository.
   a. If you want the minimal install, run:

.. code-block::

    pip install .


   b. If you want to use graphic tools, run:

.. code-block::

    pip install .[plot]

   c. If yout want to use sql tools (MF-only), run:

.. code-block::

    pip install .[sql]

   d. If you want to install all extensions, run:

.. code-block::

    pip install .[all]


.. _sec-install_dev:

Snowtools install for developers
--------------------------------

Install dependencies
^^^^^^^^^^^^^^^^^^^^

Please read ``requirements.txt`` and install the necessary dependecies. On Meteo-France computers you only need to install ``bronx``, ``footprints`` and ``epygram`` packages with ``pip`` (``pip install bronx footprints epygram``).

Clone the git repository
^^^^^^^^^^^^^^^^^^^^^^^^

Make sure you have a github account, linked to snowtools repository (send a mail to crocus at meteo dot fr) and that you have a SSH key attached to your github account [#footnote1]_. You can then clone the git repository on your computer with :

.. code-block:: bash

   git clone git@github.com:UMR-CNRM/snowtools.git

Install
^^^^^^^

Make sure you have a github account, linked to snowtools repository (send a mail to crocus at meteo dot fr) and that you have a SSH key attached to your github account [#footnote1]_. You can then clone the git repository on your computer with :

.. code-block:: bash

   git clone git@github.com:UMR-CNRM/snowtools.git

Set an environment variable pointing to the snowtools repository in your ``~/.bashrc`` file:

.. code-block:: bash

   export SNOWTOOLS_CEN=/{path_to_snowtools_repository}/snowtools


It is also recommended to create useful aliases for the installation, the use of the s2m command and proreader graphical interface in the ``~/.bashrc`` file:

.. code-block:: bash

   alias s2m="python $SNOWTOOLS_CEN/snowtools/tasks/s2m_command.py"
   alias proplotter="python3 $SNOWTOOLS_CEN/snowtools/plots/stratiprofile/proplotter.py"
   alias procompare="python3 $SNOWTOOLS_CEN/snowtools/plots/stratiprofile/procompare.py"
   alias put="$SNOWTOOLS_CEN/cenutils/put"
   alias install_snowtools="python3 $SNOWTOOLS_CEN/cenutils/install_snowtools.py"


Method 1: install with ``pip`` (recommended)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. note::

   To ensure a reproductible installation, you must **not** have any package installed locally (under your $HOME/.local).
   If this is the case, re-install the locally installed packages in a proper virtual environment and remove your $HOME/.local directory.

   You also need to make sure **not** to have your snowtools directory in your PYTHONPATH.
   So do not mix with method 2.

   There are two different possibilities:
    - make an editable install for ongoing developments
    - make a standard install for stable applications (real-time applications, reanalyses, ...)

..   Depending on your use case, follow the corresponding instructions, as well as server-specific instructions.


1. Ensure that your"snowtools" directory points to the version / commit that you want to install
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

If necessary, clone the snowtools git repository (see method 1) and checkout to the target commit.

If yout want to install snowtools on a remote server, sync your snowtools repository on this server:

.. code-block:: bash

    $SNOWTOOLS_CEN/cenutils/put snowtools {server}

and follow the next step on the remote server.


2. Choose the python version to be used
"""""""""""""""""""""""""""""""""""""""

<b>On MF HPC:</b>

For an <b>editable install</b>, you must load python version 3.10.12 or 3.12.12.

For a <b>standard install</b>, you can choose between available python versions 3.7.6nomkl, 3.10.12 or 3.12.12:

You also need to load the gcc compiler:

.. code-block::bash

   module load python/{version} gcc

<b>On sxcen:</b>

You must specify which python version you will use. Available version are 3.8 and 3.12.
To use the latest available python version, simply choose "python3":

.. code-block::bash

    alias python=python3


2. Install snowtools in a virtual environment
"""""""""""""""""""""""""""""""""""""""""""""

Use the snowtools installation script snowtools/cenutils/install_snowtools.py.

You can either create and activate a virtual environment by following the instructions given in the section "creation of a virtual environment" before launching the installation script,
or use the -v/-venv option.

For a <b>standard install</b> (stable applications):

.. code-block:: bash

   python $SNOWTOOLS_CEN/cenutils/install_snowtools.py [-v <path_to_your_virtual_env>]

For an <b>editable install</b> (ongoing developments), use the option -e:

.. code-block:: bash

   python $SNOWTOOLS_CEN/cenutils/install_snowtools.py -e [-v <path_to_your_virtual_env>]

In both cases, you can install optional dependencies with the "-o" argument:

1. If you want to use graphic tools, run:

.. code-block::

    install_snowtools -o plot [-e] [-v <path_to_your_virtual_env>]

2. If yout want to use sql tools, run:

.. code-block::

    install_snowtools -o sql [-e] [-v <path_to_your_virtual_env>]

3. If you want to install all dependencies, run:

.. code-block::

    install_snowtools -o all [-e] [-v <path_to_your_virtual_env>]


Method 2: local install with PYTHONPATH (not recommended)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Alternatively, you can add the snowtools root directory to your ``PYTHONPATH``.
This can be done by adding these tho following lines to your ``.bashrc`` or ``.bash-profile``:


.. code-block:: bash

   export SNOWTOOLS_CEN=/{path_to_snowtools_repository}/snowtools
   export PYTHONPATH=$PYTHONPATH:$SNOWTOOLS_CEN


Vortex package
^^^^^^^^^^^^^^

Only Météo-France users who need to either extract operational S2M files either to run their own experiments on the HPC system need to install the vortex package by following this link :ref:`install-vortex`.


Spatial interpolator for SAFRAN
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

**On Meteo-France super-computers**, a precompiled binary is provided in the CEN uenv environment. Therefore, this step is not required unless you need to modify the interpolation software.
If you want to use your own version :

.. code-block:: bash

   cd $SNOWTOOLS_CEN/snowtools/interpolation/
   module purge
   module load intel
   module load intelmpi

   ln -sf Makefile_belenos Makefile
   make

Running the code does not require any module load command. It is much safer to purge all modules before running.
Do never add module load commands in your .bashrc or .bash_profile files to avoid very tricky bugs.
Do absolutely never load netcdf module before running the code as this would load conflictual library versions with the ones used for compilation

**On your PC**, if you need the interpolation software of SAFRAN meteorological fields on list of points or regular grids, you need to compile the corresponding Fortran application even if you do not modify the code:

.. code-block:: bash

   cd $SNOWTOOLS_CEN/snowtools/interpolation/
   ln -s Makefile_pc Makefile
   make

At CEN, Netcdf with parallel support must be installed in /opt/netcdf4-parallel
(ask Cyril if not available)

.. [#footnote1] To generate a new ssh key, go to your ``~/.ssh`` folder (create if it does not exist) and run ``ssh-keygen -t rsa -b 4096 -f github``. You will be asked for an optional password to protect your key. Once created, go to your `github account, section SSH keys <https://github.com/settings/keys>`_, click on "add a SSH key" and copy the content of the file ``~/.ssh/github.pub`` in the "key" field.
    You may need to run

.. code-block:: bash

    eval `ssh-agent -s`
    ssh-add ~/.ssh/github

on your computer in order to define the key location on your computer.

CRPS scores
^^^^^^^^^^^
If you need to use CRPS scoring tools, which parts are written in Fortran, you need to compile them.

For CRPS scores, go to the ``snowtools/scores``, and run ``./install_ubuntu.sh`` (or ``./install_belenos.sh`` if you are on a Meteo-France super computer).


Creation of a virtual environment
"""""""""""""""""""""""""""""""""

.. note::

   For an editable install, it is necessary to create your virtual environment OUTSIDE
   the snowtools root directory.

<b>On all servers:</b>

Create a virtual environment with a freely chosen name {nameofmyenv}:

.. code-block:: bash

   python -m venv {nameofmyenv}

.. note::

   In case you also want to include packages installed by default on the server, add "--system-site-packages" to the previous command:
   python -m venv {nameofmyenv} --system-site-packages


Or create a virtual environment within the PyCharm IDE:


   File -> Settings

   In settings go to
   Project -> Python Interpreter

   Next to the Interpreter line clic "add interpreter" -> "add local interpreter"

   choose
   environment: new environment
   type: "virtuelenv"
   python base:
   choose the location and a base interpreter
   (typically the system python install /usr/bin/python3.XX)
   location: choose the location and name of your environment

   Hint: tick the "inherit packages from base interpreter" check box
   for the --system-site-packages option.

   clic the "Ok" button.


Finaly, activate the virtual environment with:

.. code-block:: bash

   source ./{pathtovenv}/{nameofmyenv}/bin/activate

now the commandline prompt should start with ``(nameofmyenv)``
and thus look like ``(nameofmyenv) username@host:~$`` for example.
