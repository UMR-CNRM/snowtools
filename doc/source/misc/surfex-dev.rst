Information for SURFEX-Crocus developers
========================================

Git branches used for develoment
--------------------------------

Currently useful branches for SURFEX-Crocus developers:

- cen: last stable release of the Crocus code (recommended for simple users)
- cen_hotfix: branch dedicated to publish bug fixes
- cen_dev: branch for new developments (recommended for all developers)

New branches must be named ``cen_dev_<project>`` with ``<project>`` being the name of the project or clearly describe the goal of the branch and originating from ``cen_dev``.

Rules coding for SURFEX-Crocus
------------------------------

If you plan to code in SURFEX-Crocus and to share your work, you must respect 4 main ideas:

- CODE: Readability counts
- EFFICIENCY: Time is money
- TEST: Errors should never pass silently
- COMMENT: Now is better than never

CODE
^^^^
    
- Follow the general rules for SURFEX : https://www.umr-cnrm.fr/surfex/spip.php?rubrique93 and in particular the DOCTOR norm for prefixing variables (mandatory).
- Do not implement IF statements within loops when the condition does not depend on points or layers (i.e. for physical options). Tests are expensive.
- No spaces at end of lines
- No tabulation
- Two spaces for indentation in IF/DO/WHERE etc...
- Number of columns shouldn’t be more than 100
- Prefer one line for each dummy variable in declaration. Describe the variable in comments providing the unit.
- Separate commit for indentation/rule coding and commit for science (implementation of new physical scheme, implementation of diagnostic).

TEST
^^^^
Snowtools provides some developments test for surfex. Run all tests to check that your devs don't break something.

.. code-block:: bash

   cd $SNOWTOOLS_CEN/snowtools/tests
   python3 test_dev_surfex.py

Implement a new test in order to secure your new developments providing a namelist and if necessary a forcing file.
If sufficient to test your implementation, prefer using the reference FORCING file for the First Test.
Otherwise, FORCING files must not be added to the snowtools repository but provided in the ``snowtools.DATA.TESTBASE_DIR`` folder.

CHECK NUMERICAL EFFICIENCY
^^^^^^^^^^^^^^^^^^^^^^^^^^
New developments are not allowed to increase the reference computing time of each routine. This has to be checked by CEN team:

On belenos:

.. code-block:: bash

   s2m research -b 20230801 -e 20240801 -n $SNOWTOOLS_CEN/DATA/OPTIONS_V9_reanalysis_forprep.nam -s $HOME/SURFEX/cen/exe --drhook -f reanalysis2025.1@lafaysse -m s2m -o reanalysis2025.drhook -r alp_allslopes

Get drhook outputs on a laptop or server and run:

.. code-block:: bash

   $SNOWTOOLS_CEN/snowtools/scripts/drhook/script_drhook.sh
   
Results in SORT_TOTAL file should exhibit lower computation times than reference computation times for all routines (cf. GMD paper for version3.0).
   
COMMENT
^^^^^^^

Please comment your code in the Fortran files providing units of variables.


Debug SURFEX-Crocus
-------------------

If you are implementing new developments or if you meet unexpected troubles during a run execution, please check the following instructions before contacting the support team :

Always compile SURFEX with the DEBUG option. Once the development is finished, recompile with O2 optimization level before running your long experiments.

For problems occurring inside the snow scheme, it is helpful to use a special debugging mode for Crocus, please read the following documentation:
https://www.umr-cnrm.fr/surfex/IMG/pdf/doc_crodebug.pdf

On Meteo-France supercomputers
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
To understand unexpected crashes during belenos runs, it is recommended to use use the ddt debugger from ARM-FORGE.

Install debugger
""""""""""""""""
First, check the remote version of DDT on the supercomputer with ``module avail``

Select a version and find the remote installation path for later step: ``module show arm/forge/22.1.1``

Locally, download the same version of ddt client on your machine at https://www.linaroforge.com/downloadForge/ (Since 2023 the ARM forge debugger has a new name, Linaro forge).
Then untar and run the install on your machine

.. code-block:: bash

   tar xvf arm-forge-22.1.3-linux-x86_64.tar
   arm-forge-22.1.3-linux-x86_64/textinstall.sh

Follow the installation procedure and specify an appropriate path for installation (example: home/username/arm/forge/)
Then start the debugger locally with ``/home/username/arm/forge/bin/forge``.

Below "remote launch" click on "configure", "add", and fill the different fields :

.. code-block:: text

   Connection name: belenos
   Hostname: username@belenos
   Remote installation directory: /opt/softs/arm/22.1.1/bin

and click "ok"

Now, you are ready to use the debugger from your PC.

Run the debugger job on the server
""""""""""""""""""""""""""""""""""

Remember, that the binaries must have be compiled in DEBUG mode.
Then, prepare a simple debug job job_debug.sh following this example:

.. code-block:: bash

   #!/bin/bash
   #SBATCH --verbose
   #SBATCH --job-name=debug_offline
   #SBATCH --nodes=1
   #SBATCH --ntasks=80
   #SBATCH --ntasks-per-core=1
   #SBATCH --time=01:00:00
   
   module load arm/forge/22.1.1
   cd /scratch/mtool/username/abort/dump_XXXXXX_reas2m/root/surfex_vortex_task
   ulimit -s unlimited
   ddt --connect srun --ntasks 80 /home/cnrm_other/cen/mrns/lafaysse/SURFEX/cen_dev/exe/OFFLINE-LXifort-SFX-V8-1-1-MPIAUTO-OMP-DEBUG-X0

.. warning::
   Tricky segmentation faults are likely to happen if you forget "ulimit -s unlimited"

Run the job: ``sbatch job_debug.sh`` or in case of saturation: ``sbatch --partition=debug256 --qos=debug job_debug.sh``

Once the job is running, a pop-up opens in ddt : you must accept the remote connection in ddt on your PC.
You can start using the debugger normally.

.. warning::
   Don't forget to tick the OpenMP box and seclect 1 threads!!

.. warning::
   Memory debugging must be disactivated with this version

Add diagnostic in SURFEX-Crocus
-------------------------------

If you need to add a diagnostic relative to the snowpack simulation, some guidelines are provided below. 
For variables defined for each snow, it is possible to follow instructions applied to ACC_RAT diagnostics which is also defined for each layer of the snowpack.

Files to modify (a minima) :

- modd_diag_misc_isban.F90
- diag_misc_isba_initn.F90
- snow3L_isba.F90
- snowcro_diag.F90
- write_diag_misc_isban.F90

**modd_diag_misc_isban.F90**:

In this module, the "ISBA Diagnostic" variable is declared. The type of this variable is TYPE.

You can imagine the TYPE as an object and you are adding a new attribute to this object.

In this module, the name of ISBA_Diagnostic is DMI. The way to access to an attribute is via the % sign : DMI%XVAR_NEWDIAG.

If your diagnostic dimension is (number of points, number of layers), you can follow ACC_RAT for the declaration of your new diagnostic

**diag_isba_initn.F90**:

In this module, this is the initialisation of ISBA_Diagnostic, named DMA.

So we are working on DMA%XVAR_NEWDIAG

For a (number of points, number of layers) diagnostic, you can follow ACC_RAT also.

Beware, there are two dimensions possible : KLUA and KLUAP which depends on the behaviour of your diagnostic regarding to SURFEX Patches -> be sure that the existing diagnostic you are following has the same behaviour and dimensions.


**snow3L_isba.F90**:

In this module, you’re starting to work with ISBA_Diagnostic (named DMK here):

- initialise DMK%XVAR_NEWDIAG( :, : ) = XUNDEF
- define a local variable ZP_XVAR_NEWDIAG (packed for snow-covered points)
- call snowcro_diag.F90 with the local variable if it can be computed from snowcro outputs (Of course, for specific diagnostic, the computation could be done in another part of the code e.g. snowcro.F90 or another module).
- put the return value in DMK%XVAR_NEWDIAG (unpacking to all simulation points)
- As always, you can follow ACC_RAT to see what happens and how.

**The (near) end**:

Now your variable is ready and you can either

1. use *snowcro_diag.F90:* if you want to compute a diagnostic with output variables from snowcro,
2. and/or use *snowcro.F90:* if you need to set/get your variable in this main routine.

In both cases, your variable should appear in the subroutine arguments as OUT.
Below the case 1 is described.

**snowcro_diag.F90**:

This is the science part : you have to calculate NEW_DIAG with the variables you need. Of course, you have to declare this new variable as OUT and to put it in the call of the module.

**write_diag_misc_isban.F90**:

This is the script for writing in PRO file. (you can follow ACC_RAT)

- change the YCOMMENT (long name: "my beautiful new diag for good science")
- change the short name 'VAR_NEWDIAG' and name the variable (lot of % everywhere but you only have to put XVAR_NEWDIAG instead of XACC_RAT if you are copying this diag)

Now, you have to compile SURFEX and to test it (do not forget to add your diagnostic in the CSELECT of the namelist).

Finally, please also add some infos in this documentation (:ref:`Available diagnostics<avail_diagnostics>`).
