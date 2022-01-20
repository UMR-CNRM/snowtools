.. _sec-install:

Install Snowtools
=================

Dependencies
------------

The snowtools project is mainly designed for a Linux environment.

The whole project requires at least python 3.6 and the following packages:

* ``six``
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

We encourage you to install these packages through system packages, for instance with the following command line on Debian or Ubuntu 20.04 system:

.. code-block:: bash
   
   sudo apt install python3-six python3-numpy python3-netcdf4 \
                    python3-matplotlib python3-dateutil python3-pyproj \
                    python3-shapely python3-willow python3-scipy \
                    python3-psycopg2 python3-pandas python3-xarray \
                    libgdal libgdal-dev python3-gdal python3-rpy2

Install with pip
^^^^^^^^^^^^^^^^
Most of the packages can easily be installed in a virtual environment with ``pip``: use ``pip install -r requirements.txt``.

Only GDAL need to be installed manually to be installed consistently with your installed ``libgdal-dev`` version. Please install before the system packages ``ligdal`` and ``libgdal-dev`` (or similar) and run: ``pip install GDAL==$(gdal-config --version) --global-option=build_ext --global-option="$(gdal-config --cflags)"``.

Specific case of cartopy
^^^^^^^^^^^^^^^^^^^^^^^^

Cartopy will need to be installed with ``pip install cartopy`` as you must specify the version corresponding to the matplotlib version installed (see :ref:`maps plots documentation <plots-maps>`).



Install for developers
----------------------

Snowtools is available on a git repository. See https://opensource.umr-cnrm.fr/projects/snowtools_git/wiki/Procedure_for_new_users for getting access.

Clone the git repository on your computer. You have to add the install folder to your ``PYTHONPATH``. This can be done by adding these tho following lines to your ``.bashrc`` or ``.bash-profile``: 

.. code-block:: bash
   
   export SNOWTOOLS_CEN=/yourpath/snowtools_git
   export PYTHONPATH=$PYTHONPATH:$SNOWTOOLS_CEN

It is also recommended to create useful aliases for s2m command and proreader graphical user interface in the same file:

.. code-block:: bash

   alias s2m="python $SNOWTOOLS_CEN/snowtools/tasks/s2m_command.py"
   alias proreader="python3 $SNOWTOOLS_CEN/snowtools/plots/GUI_Proreader.py"
