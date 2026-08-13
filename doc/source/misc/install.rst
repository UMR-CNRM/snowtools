.. _sec-install:

Install Snowtools
=================
The snowtools project is mainly designed for a Linux environment.

Snowtools install for users
---------------------------

If you are only a user of snowtools, you can install the package easily with pip in a virtual environment:

1. Download the source code: ``git clone https://github.com/UMR-CNRM/snowtools.git``
2. Create a virtual environment : ``python3 -m venv <name_of_your_virtual_env>``
3. Enter in the virtual environment:  ``source <name_of_your_virtual_env>/bin/activate``
4. Ensure you are at the root of the snowtools repository and install the package (with optional dependencies plot) by running:

.. code-block::

    pip install .[plot]

5. Once you have finished working with snowtools, you can leave the virtual environment by typing ``deactivate``. You can come back to the environment later by calling again ``source <name_of_your_virtual_env>/bin/activate``.

Snowtools install for developers and Meteo-France staff
-------------------------------------------------------

Get the code
^^^^^^^^^^^^

Make sure you have a github account and that you have a SSH key attached to your github account [#footnote1]_. Do not hesitate to ask an access to the snowtools code and tickets repository (send a mail to crocus at meteo dot fr). You can then clone the git repository on your computer with:

.. code-block:: bash

   git clone git@github.com:UMR-CNRM/snowtools.git


.. admonition:: Special case of insallation on Meteo-France sxcen and HPC machines

   For CEN staff, on sxcen and HPC, we recommend you not to clone the repository directly but to synchronize with the code already present on your PC by using the ``put`` tool. To do so, run this command **on your PC** : 

    .. code-block:: bash

        # For Belenos
        $SNOWTOOLS_CEN/cenutils/put snowtools belenos

        # For SXCEN
        $SNOWTOOLS_CEN/cenutils/put snowtools sxcen


.. warning::
   If you had a previous verison of snowtools installed as a developper, you first need to uninstall the previous version.

   .. toctree::
      :maxdepth: 1

      uninstall-snowtools2.rst

Prerequisites on Meteo-France computers
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

- On HPC you first have to set the correct python version and compiler to use by running : ``module load python/3.10.12 gcc/15.2.0``
- On all Meteo-Frace machines, if you intend to deal with data stored on Meteo-France archive (hendrix), you will have to configure connecton creedentials as explained on page :ref:`vortex-file-transfer`.

Installation
^^^^^^^^^^^^

1. Choose a location where to store your virtual environments (e.g. ``~/my_envs``, if you do not have a dedicated folder, create it with ``mkdir ~/my_envs``).
2. Create a virtual environment :  ``python3 -m venv ~/my_envs/snowtools_env``.
3. Enter in the virtual environment:  ``source ~/my_envs/snowtools_env/bin/activate``.
4. Use the script dedicated to installation of snowtools on Meteo-France machines : ``python3 cenutils/install_snowtools.py -o all -e``
   (This command will run ``pip install`` of snowtools, as an editable install and prepare necessary configuration for vortex).

That's all for snowtools.

Each time you will need snowtools, you will have to activate the python virtual environment with ``source ~/my_envs/snowtools_env/bin/activate``. When you have finissed your work, you can leave the virtual environment by using the ``deactivate`` command.

Additional optional dependencies and configuration
--------------------------------------------------

Optional dependecines of the snowtools package
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

By default, we recommend external users use the ``plot`` optional dependencies and developers have all optional dependencies by using the ``all`` optional dependency. However a finer granularity is available if needed :

- ``plot`` : tools for plotting, including plotting tools of simulation outputs
- ``scores`` : CRPS score computation
- ``vortex`` : Tools to work with simulations made on HPC (launching of simulations on Meteo-France HPC and retrieve data produced)
- ``all`` gather previous dependencies
- ``doc`` : Dependencies for documentation generation (in addition to ``all`` dependency), only available at Meteo-France for the moment.
- ``hpc``: only used on Meteo-France HPC, to run simullations on these machines, do not try to install elsewhere.

Additional dependencies that have to be installed manually
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

**GDAL** is a dependency for some geopsatial processings. You first need to install gdal binaries (e.g. on Ubuntu, run ``sudo apt install libgdal libgdal-dev``, already installed on Meteo-France machines). Then, you need to install the python binding manually to be consistently with your installed ``libgdal-dev`` version by running: ``pip install GDAL==$(gdal-config --version) --global-option=build_ext --global-option="$(gdal-config --cflags)"``.

Optional additional denpendencies on nexus
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

By default, Pip fetches package distributions from the global Python package registry, pypi.org. The MF's internal Nexus package registry provides additional packages that can be usefull (for example vortex-gco, which enables the use of "Uenv" tools). It is thus recommended to configure Pip so that it can access Nexus. To do so, add the following lines to ~/.config/pip/pip.conf containaing:

.. code-block:: ini

    [global]
    index = https://nexus-sidev.meteo.fr/repository/pypi-group/pypi
    index-url = https://nexus-sidev.meteo.fr/repository/pypi-group/simple
    extra-index-url = https://nexus.meteo.fr/pypi-vortex-releases/simple

Note that this is not required on HPC because the access to Nexus from HPC is currently blocked.


[Optional] Install UEnv tools
"""""""""""""""""""""""""""""
Install UEnv tools (already installed by default on Belenos):

.. code-block:: bash

    source ~/my_envs/snowtools_env/bin/activate
    pip install vortex-gco
    deactivate


Spatial interpolator for SAFRAN
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

- **On Meteo-France super-computers**, a precompiled binary is provided in the CEN uenv environment.
- **On your PC at CEN**, a pre-compiled binary is also provided on shared filesystems.
- **Otherwise** (or if you want to use a custom version, e.g. for development) you will need to compile the ``interpol`` binaty and set the environment variable ``SNOWTOOLS_INTERPOL`` to point to the location of the interpol compiled binary. To do so, the procedure is detailed below.

To compile the interpol binary:

1. go into the ``snowtools/interpolation`` folder
2. On Meteo-France HPC only, load the necessary modules :

.. code-block:: bash

   module purge
   moudle load intel
   module load intelmpi

3. On external PC only (outside of Meteo-France), you need to have a Fortran90 compiler, a MPI compiler, openmpi, netcdf-parallel and netcdff libraries with headers availables. On Ubuntu, it means installing the following packages : ``build-essential libopenmpi-dev libnetcdf-mpi-dev libnetcdff-dev``.

4. Remove Makefile if defined with ``rm Makefile`` and then link to the correct one depending on your situation:

   - On Meteo-France HPC : ``ln -s Makefile_belenos Makefile``
   - On Meteo-France PC : ``ln -s Makefile_pc_mf Makefile``
   - On external PC : ``ln -s Makefile_pc_nomf Makefile``

5. You just have to run ``make``. That's all. You now have an ``interpol`` binary in the current folder.


CRPS scores
^^^^^^^^^^^
CRPS score is now as an independent package available at https://github.com/UMR-CNRM/snowtools-crps

To install it along with snowtools, just install the optional dependency ``snowtools[scores]`` or ``pip install .[all]``.
Note that you may need to upgrade pip to version above 23.0 to install scores dependency due to a bug in previous pip versions.

.. [#footnote1] To generate a new ssh key, go to your ``~/.ssh`` folder (create if it does not exist) and run ``ssh-keygen -t rsa -b 4096 -f github``. You will be asked for an optional password to protect your key. Once created, go to your `github account, section SSH keys <https://github.com/settings/keys>`_, click on "add a SSH key" and copy the content of the file ``~/.ssh/github.pub`` in the "key" field.
    You may had to add to your ``.ssh/config`` the following lines:

    .. code-block::

        Host github.com
            IdentityFile ~/.ssh/github

    Alternatively, you can use the ssh-agent by running :

    .. code-block:: bash

       eval `ssh-agent -s`
       ssh-add ~/.ssh/github

