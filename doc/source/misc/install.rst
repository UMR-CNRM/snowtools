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
2. Create a virtual environment : ``python3 -m venv --system-site-packages <name_of_your_virtual_env>``
3. Enter in the virtual environment:  ``source <name_of_your_virtual_env>/bin/activate``
4. Ensure you are at the root of the snowtools repository and install the package by running:

.. code-block::

    pip install .



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


You have to add the install folder to your ``PYTHONPATH``. This can be done by adding these tho following lines to your ``.bashrc`` or ``.bash-profile``:


.. code-block:: bash

   export SNOWTOOLS_CEN=/yourpath/snowtools
   export PYTHONPATH=$PYTHONPATH:$SNOWTOOLS_CEN

It is also recommended to create useful aliases for s2m command and proreader graphical user interface in the ``~/.bashrc`` file:

.. code-block:: bash


You have to add the install folder to your ``PYTHONPATH``. This can be done by adding these tho following lines to your ``.bashrc`` or ``.bash-profile``:


.. code-block:: bash

   export SNOWTOOLS_CEN=/yourpath/snowtools
   export PYTHONPATH=$PYTHONPATH:$SNOWTOOLS_CEN

It is also recommended to create useful aliases for s2m command and proreader graphical user interface in the ``~/.bashrc`` file:

.. code-block:: bash

   alias s2m="python $SNOWTOOLS_CEN/snowtools/tasks/s2m_command.py"
   alias proplotter="python3 $SNOWTOOLS_CEN/snowtools/plots/stratiprofile/proplotter.py"
   alias procompare="python3 $SNOWTOOLS_CEN/snowtools/plots/stratiprofile/procompare.py"
   alias put="$SNOWTOOLS_CEN/cenutils/put"


..
   Method 2
   ^^^^^^^^

   editable install with ``pip``.

   .. note::

       If using this method make sure **not** to have
       your snowtools directory in your PYTHONPATH.
       So do not mix with method 1.


   1. Clone the git repository on your computer.
   """""""""""""""""""""""""""""""""""""""""""""
   (see method 1)

   2. create or choose a virtual environment.
   """""""""""""""""""""""""""""""""""""""""""
   To create a virtual environment you can run:

   .. code-block:: bash

       python -m venv nameofmyenv --system-site-packages

   where ``nameofmyenv`` is a freely chosen name for the environment
   and --system-site-packages makes the packages already installed on
   the system available inside the virtual environment.

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

   3. source the virtual environment
   """""""""""""""""""""""""""""""""

   .. code-block:: bash

       source ./<pathtovenv>/nameofmyenv/bin/activate

   now the commandline prompt should start with ``(nameofmyenv)``
   and thus look like ``(nameofmyenv) username@host:~$`` for example.

   4. install build dependencies
   """"""""""""""""""""""""""""""
   ``numpy>=1.24.4``, ``meson-python`` and ``ninja`` inside the virtual environment.

   .. code-block:: bash

           pip install numpy>=1.24.4 meson-python ninja

   .. note::

       Snowtools contains a compiled extension module written in Fortran.
       In order to render compiled extension modules editable similarly to ordinary python code,
       they are compiled at import time in an editable install rather than during
       installation in case of a classical install (:ref:`sec-install_users`).
       This means that the build dependencies have to be available at runtime in
       the virtual environment and not just temporarily during the install.
       The advantage is, that edits in the Fortran code trigger the (partial) re-compilation of
       the extension module at the next import in a new interpreter instance.
       https://mesonbuild.com/meson-python/how-to-guides/editable-installs.html

   5. install snowtools:
   """""""""""""""""""""""
   inside the snowtools directory do:

   .. code-block:: bash

       pip install --no-build-isolation -e .

   .. note::

       ``--no-build-isolation`` disables build isolation.
       Disabling build isolation is necessary in order to be able to re-build extensions
       at import time in editable installs. For ordinary installs build isolation is a desired feature.

Optional installations
----------------------


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

