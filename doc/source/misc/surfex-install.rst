Install SURFEX
==============

SURFEX is now available as open-source.

Below is given an extract of the install procedure with the necessary information to get started with SURFEX in particular if snow on the ground is a target.

Users at Meteo-France
---------------------

Simple users at Meteo-France do not need to install SURFEX, but rather should point at an existing install:

* On supercomputers, no need to specify a SURFEX install path, snowtools will use an appropriate one automatically;
* On Meteo-France PC or server at CEN, please set ``EXESURFEX`` environment variable to ``/rd/cenfic3/cenmod/home/lafaysse/common/SURFEX/surfex-cen-NOMPI-O2-X0/exe``;
* On sxcen, point ``EXESURFEX`` to ``/home/lafaysse/common/SURFEX/surfex-cen-NOMPI-O2-X0/exe``.

Note: If you need to work with more recent branch (``cen_dev`` for instance) or other compile options (``MPI``) have a look to available binaries in the previously pointed ``SURFEX`` folder where other compiled binaries are available.


Get the code
------------

Download the source code: ``git clone https://github.com/UMR-CNRM/SURFEX_CEN.git``

   
You need some packages which are not in the git repository (has to be fixed). So please copy from your old repo

.. code-block:: bash

   cp /home/your_name/.../your_old_repo/src/LIB/SPARTACUS_SURFACE_0.6.1.tar.gz /home/your_name/.../SURFEX_CEN/src/LIB/.
   cp /home/your_name/.../your_old_repo/src/LIB/XIOS-GMGEC-2152.tar /home/your_name/.../SURFEX_CEN/src/LIB/.

Please read the infos for developper: https://umr-cnrm.github.io/snowtools-doc/misc/surfex-dev.html

Compile the code
----------------

Then you need to compile the code following these instructions.

Note that all the followings commands must be typed in the **same terminal**. Once finnished, you have to open a **new terminal** before running any simulation.

Preparation on a Linux PC
^^^^^^^^^^^^^^^^^^^^^^^^^

.. note::
   If you run on Meteo-France supercomputer, read next section.


If you do not want to compile SURFEX with a MPI-support for parallel runs (recommended for local simulations):

.. code-block:: bash

   export VER_MPI=NOMPI


Do not type this command in the following cases :

* you are working on a supercomputer
* you want to run simulations over more than 4 points and MPI fortran compiler is available on your computer (this is the case in Meteo-France computers)
* you need to use the interpolator of SAFRAN fields.

In the other cases, adding this line to your $HOME/.bash_profile or equivalent is convenient.

You can then follow the install procedure given below.

Preparation on Méteo France Supercomputers
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. note::
   If you do not run on Meteo-France supercomputer, please skip this section.

   To use the last stable release of SURFEX-Crocus, you **do not need** to compile SURFEX on Belenos ! ``s2m`` will automatically use the last available binary.

You first need to load the compilers :

.. code-block:: bash

   module load intel
   module load intelmpi
   module load curl

The next step of the configuration depends on your application:

* For MPI parallel applications (big domains, deterministic application) : let ``VER_MPI`` and ``VER_CDF`` options to default.
* For sequential ensemble applications associed with a ParaBlindRun AlgoComponent (one member per core through vortex), and only in this case, export the following variables:

.. code-block:: bash

    export VER_MPI=NOMPI
    export VER_CDF=CDF2020

You will also need to install :ref:`install-vortex`.

Common configuration
^^^^^^^^^^^^^^^^^^^^
At this step, it is also possible to modify the optimization level by :

.. code-block:: bash

   export OPTLEVEL=DEBUG
   # or
   export OPTLEVEL=O2


Optimization level “O2” is recommended in the general case (significant decrease of running time). In case of bugs or for the first local run after a new development, the optimization level may be changed to "DEBUG" to generate better error messages.

.. warning::

   Long runs must never be run in DEBUG mode.


Initialize environment variables needed for surfex: go into ``src`` directory and run

.. code-block:: bash

   ./configure

Then, execute the profile file for this master version of surfex:

.. code-block:: bash

   source ../conf/profile_surfex-LXgfortran-SFX-V8-1-1-NOMPI-OMP-O2-X0


(or equivalent name depending on compiler, SURFEX version and compilation options)

Compilation
^^^^^^^^^^^

Compile the master version of the code:
in the src directory and in the same terminal, run


.. code-block:: bash

   make
   make installmaster


Master executables are created in directory exe. If everything goes well until this step, then surfex has been successfully installed on you computer.


Then follow the paragraphs "configuration" and "Compilation" from normal computers.

Additional steps
----------------

Setting specifically the SURFEX binaries to be used by s2m is done by exporting the variable EXESURFEX pointing to the directory containing the PGD, PREP and OFFLINE binaries:

.. code-block:: bash

   export EXESURFEX=/home/...yoursurfexdirectory../exe


You can add this line to your $HOME/.bash_profile file if you are installing the SURFEX version you want to use by default.

After SURFEX compilation, binaries have their complete names (with compiler name, surfex version, mpi option and optimization level) so you need to create manually symbolic links with these short names as s2m work with the short names.
Please check carefully the full names of your binaries (depending on SURFEX version, compiler and compilation options) to build the symbolic links. Example :

.. code-block:: bash

   ln -s $EXESURFEX/OFFLINE-LXgfortran-SFX-V8-1-1-NOMPI-O2-X0 $EXESURFEX/OFFLINE
   ln -s $EXESURFEX/PREP-LXgfortran-SFX-V8-1-1-NOMPI-O2-X0 $EXESURFEX/PREP
   ln -s $EXESURFEX/PGD-LXgfortran-SFX-V8-1-1-NOMPI-O2-X0 $EXESURFEX/PGD
   ln -s $EXESURFEX/SODA-LXgfortran-SFX-V8-1-1-NOMPI-O2-X0 $EXESURFEX/SODA

Test your snowtools and SURFEX install
--------------------------------------
If you correctly installed the snowtools and SURFEX projects, you must be able to run successfully the following test case:

.. code-block:: bash

   s2m research -f $SNOWTOOLS_CEN/snowtools/DATA/FORCING_test_base.nc -b 20100801 -e 20110801 -o output -g -s ...yoursurfexdirectory.../exe
