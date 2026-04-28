External use of Crocus for other Land Surface Models
====================================================

For users who want to couple Crocus with their own surface scheme, instead of downloading the whole project SURFEX-Crocus, you can download only Crocus and its dependencies.
We recommend to get the up-to-date source code from the private github repository `SURFEX_CEN <https://github.com/UMR-CNRM/SURFEX_CEN>`__, please ask for access by email to crocus at meteo dot fr.

To install only Crocus and its dependencies, use the following commands:

In the download directory:

.. code-block:: bash

   git init 
   git config core.sparseCheckout true 
   git remote add -f origin git@github.com:UMR-CNRM/SURFEX_CEN.git


Save the attached file `sparse-checkout <../_static/sparse-checkout>`__ in ``.git/info/``

Then

.. code-block:: bash

   git pull
   cd src/EXT_CROCUS
   make

CROCUS is ready to use. To test it, an executable "prog" is available after compilation just run it

.. code-block:: bash

   ./prog

For external import in a Fortran surface scheme, users should first compile externalized Crocus as mentioned above.
Then in their surface scheme makefile, add option ``-I $SRC_SURFEX/EXT_CROCUS/obj``
with setting of ``SRC_SURFEX`` variable to the appropriate path.

This way, the Fortran source files of the user's surface scheme can import Crocus by:

.. code-block:: fortran

   USE MODI_SNOWCRO


.. warning::
   Be Careful: before performing any local update of the repository, you need to manually check for updates of the ``sparse-checkout`` file attached above. In case changes have occurred, update your local copy of in ``.git/info`` manually prior to running ``git pull``
   
For SURFEX developers:

If during development we have to use others files from SURFEX-CROCUS project we add them in ``.git/info/sparse-checkout`` then tap

.. code-block:: bash


   git read-tree -mu HEAD

.. warning::
   Be Careful: the modi_* files are not generated automatically, you have to update them when their subroutine are updated in SURFEX source

