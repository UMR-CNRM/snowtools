.. _sec-install:

Install Snowtools
=================

.. note::

   If you are using snowtools on Meteo-France computers, go directly to the last section (:ref:`sec-install_dev`).

Dependencies
------------

The snowtools project is mainly designed for a Linux environment.

The whole project requires at least python 3.6 and the following packages:

* ``numpy``
* ``netCDF4``
* ``matplotlib``
* ``python-dateutil``
* ``pyproj``
* ``shapely``

Some specific parts of the code (including test) require to have also:

* ``Pillow`` or ``PIL`` (Pillow Imaging library)
* ``scipy``
* ``psycopg2``
* ``pandas``
* ``xarray``
* ``GDAL``
* ``rpy2``
* ``cartopy``
* The vortex toolbox for run on Meteo-France super-computers

The detailed list of dependencies could be find in the file ``requirements.txt``.

.. note::
   On Meteo-France computers, you do not have take care of dependecies that are already on your professional computer.

Install dependencies
^^^^^^^^^^^^^^^^^^^^
We encourage you to install dependencies through system packages, for instance with the following command line on Debian or Ubuntu 22.04 system:

.. code-block:: bash
   
   sudo apt install python3-numpy python3-netcdf4 \
                    python3-matplotlib python3-dateutil python3-pyproj \
                    python3-shapely python3-willow python3-scipy \
                    python3-psycopg2 python3-pandas python3-xarray \
                    libgdal-dev python3-gdal python3-rpy2

Install ``bronx`` package with ``pip install bronx`` (no distribution package for this python package).

Install with pip
^^^^^^^^^^^^^^^^
Most of the packages can easily be installed in a virtual environment with ``pip`` (for instance with ``pip install -r requirements.txt``). This is useless if you plan to then install snowtools with  ``pip``.

Only GDAL python binding need to be installed manually to be installed consistently with your installed ``libgdal-dev`` version. Please install before the system packages ``ligdal`` and ``libgdal-dev`` (or similar) and run: ``pip install GDAL==$(gdal-config --version) --global-option=build_ext --global-option="$(gdal-config --cflags)"``.

Snowtools install for users
---------------------------

If you are only a user of snowtools, download the data with git: ``git clone https://github.com/UMR-CNRM/snowtools.git``.
To install, you can simply go into the snowtools directory and then run  ``pip install -e .`` (possibly in a python virtual environment).


.. _sec-install_dev:

Snowtools install for developers
--------------------------------

Make sure you have a github account, linked to snowtools repository (send a mail to crocus at meteo dot fr) and that you have a SSH key attached to your github account [#footnote1]_. You can then clone the git repository on your computer with :

.. code-block:: bash

   git clone git@github.com:UMR-CNRM/snowtools.git

To install, you have to add the install folder to your ``PYTHONPATH``. This can be done by adding these tho following lines to your ``.profile`` (or ``.bash-profile`` if the first one does not exist):

.. code-block:: bash

   export SNOWTOOLS_CEN=/yourpath/snowtools
   export PYTHONPATH=$PYTHONPATH:$SNOWTOOLS_CEN

It is also recommended to create useful aliases for s2m command and proreader graphical user interface in the ``~/.bashrc`` file:

.. code-block:: bash

   alias s2m="python $SNOWTOOLS_CEN/snowtools/tasks/s2m_command.py"
   alias proplotter="python3 $SNOWTOOLS_CEN/snowtools/plots/stratiprofile/proplotter.py"
   alias procompare="python3 $SNOWTOOLS_CEN/snowtools/plots/stratiprofile/procompare.py"
   alias put="python3 $SNOWTOOLS_CEN/cenutils/put"

Optional installations
----------------------


Vortex package
^^^^^^^^^^^^^^

Only Météo-France users who need to either extract operational S2M files either to run their own experiments on the HPC system need to install the vortex package by following this link :ref:`install-vortex`


Spatial interploator for SAFRAN
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

At CEN, Netcdf with parallel support must be installed in /opt/netcdf4-parallel (ask Romain if not available)

.. [#footnote1] To generate a new ssh key, go to your ``~/.ssh`` folder (create if it does not exist) and run ``ssh-keygen -t rsa -b 4096 -f github``. You will be asked for an optional password to protect your key. Once created, go to your `github account, section SSH keys <https://github.com/settings/keys>`_, click on "add a SSH key" and copy the content of the file ``~/.ssh/github.pub`` in the "key" field.
