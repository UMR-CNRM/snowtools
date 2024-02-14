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

Install dependencies with system packages
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We encourage you to install these packages through system packages, for instance with the following command line on Debian or Ubuntu 22.04 system:

.. code-block:: bash
   
   sudo apt install python3-numpy python3-netcdf4 \
                    python3-matplotlib python3-dateutil python3-pyproj \
                    python3-shapely python3-willow python3-scipy \
                    python3-psycopg2 python3-pandas python3-xarray \
                    libgdal-dev python3-gdal python3-rpy2

Install ``bronx`` package with ``pip install bronx`` (no distribution package for this python package).

Install with pip
^^^^^^^^^^^^^^^^
Most of the packages can easily be installed in a virtual environment with ``pip``: use ``pip install -r requirements.txt``.

Only GDAL python binding need to be installed manually to be installed consistently with your installed ``libgdal-dev`` version. Please install before the system packages ``ligdal`` and ``libgdal-dev`` (or similar) and run: ``pip install GDAL==$(gdal-config --version) --global-option=build_ext --global-option="$(gdal-config --cflags)"``.

Snowtools install
^^^^^^^^^^^^^^^^^

If you are only a user of snowtools, you can simply go into the snowtools directory and then run  ``pip install .``.


.. _sec-install_dev:

Install for developers
----------------------

Clone the git repository on your computer. You have to add the install folder to your ``PYTHONPATH``. This can be done by adding these tho following lines to your ``.bashrc`` or ``.bash-profile``: 

.. code-block:: bash
   
   export SNOWTOOLS_CEN=/yourpath/snowtools
   export PYTHONPATH=$PYTHONPATH:$SNOWTOOLS_CEN

It is also recommended to create useful aliases for s2m command and proreader graphical user interface in the same file:

.. code-block:: bash

   alias s2m="python $SNOWTOOLS_CEN/snowtools/tasks/s2m_command.py"
   alias proplotter="python3 $SNOWTOOLS_CEN/snowtools/plots/stratiprofile/proplotter.py"
   alias procompare="python3 $SNOWTOOLS_CEN/snowtools/plots/stratiprofile/procompare.py"

Optional installations
----------------------

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
