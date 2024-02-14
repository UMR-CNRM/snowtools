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

If you plan to code in SURFEX-Crocus and to share your work, you must respect 3 main ideas:

- CODE: Readability counts
- TEST: Errors should never pass silently
- COMMENT: Now is better than never

CODE
^^^^
    
- follow the general rules for SURFEX : https://www.umr-cnrm.fr/surfex/spip.php?rubrique93 and in particular the DOCTOR norm for prefixing variables.
- No spaces at end of lines
- No tabulation
- Always ENDIF after IF. Indeed, FORTRAN allows a IF instruction in only one line but it can disturb some IDE tools resulting in bad automatic indentation
- Same for DO/ENDDO and WHERE/ENDWHERE
- Two spaces for indentation in IF/DO/WHERE etc...
- Number of columns shouldn’t be more than 100
- Proposal : one line for each dummy variable in declaration. Always comment what is this variable, give the unit involved.
- For the CALL of routine, we should have a rule for the « & column » -> has to be decided.
- Please separate commit for indentation/rule coding and commit for science (implementation of new physical scheme, implementation of diagnostic).
- The actual SURFEX-Crocus code is not respecting these rules. We DO NEED YOU to help us getting the code as clean as possible. If you’re OK to do your part on rule coding, this would be awesome.

TEST
^^^^
Snowtools provides some developments test for surfex. Please use it to check that your devs don't break something and please add one of your test.

Test your modifications:

.. code-block:: bash

   cd $SNOWTOOLS_CEN/snowtools/tests
   python3 test_dev_surfex.py

Implement a new test in order to secure your developments :

We just need a namelist and a FORCING File to add your devs to test_dev_surfex.py
If it is possible to use the FORCING used in the First Test, this is better. Otherwise, never commit in the snowtools repository but put your test files in the ``snowtools.DATA.TESTBASE_DIR`` folder.

COMMENT
^^^^^^^

Please comment your code in the Fortran files. It is useful to have the units involved.

If some diagnostics are specific to your devs, please also add some infos in this documentation (Page Few informations on SURFEX-Crocus simulations).

Debug SURFEX-Crocus
-------------------

If you are implementing new developments or if you meet unexpected troubles during a run execution, please check the following instructions before contacting the support team :

Always compile SURFEX with the DEBUG option. Once the development is finished, recompile with O2 optimization level before running your long experiments.

For problems occurring inside the snow scheme, it is helpful to use a special debugging mode for Crocus, please read the following documentation:
http://www.cnrm.meteo.fr/surfex-lab/IMG/pdf/doc_crodebug.pdf

On Meteo-France supercomputers
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
To understand unexpected crashes during belenos runs, two options are possible:

For complex bugs, it is highly recommended to use use the ddt debugger from ARM-FORGE.

Full documentation available here: http://intradsi.meteo.fr/spip.php?article2007

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

If you need to ad a diagnostic, some guidelines are provided below. Depending of the science involved, it could be done in different parts of the SURFEX-Crocus code.
We imagine here the simpliest way for a first try, following an existing diagnostic ACC_RAT which is define on each layer of the snowpack.

Files to modify (a minima) :

- modd_diag_misc_isban.F90
- diag_isba_initn.F90
- snow3L_isba.F90
- snowcro_diag.F90
- write_diag_misc_isban.F90

What are these routines, what are me modifying inside ?

**modd_diag_misc_isban.F90**:

In this script, we declare the "ISBA Diagnostic" variable.

The nature of this variable is TYPE.

You can imagine the TYPE as a vector and you are adding a new coordinate to this vector.

In this script, the name of ISBA_Diagnostic is DMI. The way to access to a coordinate is via the % sign : DMI%XVAR_NEWDIAG.

If your diagnostic dimension is (number of points, number of layers), you can follow ACC_RAT for the declaration of your new diagnostic

**diag_isba_initn.F90**:

In this script, this is the initialisation of ISBA_Diagnostic, named DMA.

So we are working on DMA%XVAR_NEWDIAG

For a (number of points, number of layers) diagnostic, you can follow ACC_RAT also.

Beware, there are two dimensions possible : KLUA and KLUAP which depends on the behaviour of your diagnostic regarding to SURFEX Patches -> be sure that the existing diagnostic you are following has the same behaviour and dimensions.


**snow3L_isba.F90**:

In this script, you’re starting to work with ISBA_Diagnostic (named DMK here):

- initialise DMK%XVAR_NEWDIAG( :, : ) = XUNDEF
- define a local variable ZP_XVAR_NEWDIAG
- call snowcro_diag.F90 with the local variable (Of course, for specific diagnostic, the calculus could be done in another part of the code and maybe you would have to modify snowcro.F90 or another script).
- put the returning value in DMK%XVAR_NEWDIAG
- As always, you can follow ACC_RAT to see what happens and how.

**snowcro_diag.F90**:

This is the science part : you have to calculate NEW_DIAG with the fields you need. Of course, you have to declare this new variable as INOUT and to put it in the call of the script.

**write_diag_misc_isban.F90**:

This is the script for writing in PRO file. (you can follow ACC_RAT)

- change the YCOMMENT (long name: "my beautiful new diag for good science")
- change the short name 'VAR_NEWDIAG' and name the variable (lot of % everywhere but you only have to put XVAR_NEWDIAG instead of XACC_RAT if you are copying this diag)

Now, you have to compile SURFEX and to test it (do not forget to add your diagnostic in the CSELECT of the namelist).
