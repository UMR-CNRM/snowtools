A general documentation (in french) can be found in:
   http://intra.cnrm.meteo.fr/algopy/sphinx/vortex/current/technical/uget.html

There is a list of useful commands at the end

Create a User Environment from scratch
======================================
1. Put the file(s) you need in your UEnv in ``$HOME/.vortexrc/hack/uget/your_username/data``

Your file(s) should have a name and a number: ``AFirstFile.0``, ``ASecondFile.1``

2. Create a text file which is the name of your UEnv (ex ``MyFirstUenv.0``) in ``$HOME/.vortexrc/hack/uget/your_username/env``

3. In this text file, associate a key to each file:

.. code-block:: bash

   FIRST_KEY="uenv:AFirstFile.0@your_username"
   SECOND_KEY="uenv:ASecondFile.1@your_username"

4. UEnv is ready, you can access to file(s) with the command:

.. code-block:: bash

   toolbox.input(genv='uenv:MyFirstUenv.0@your_username', gvar='FIRST_KEY', unknown=True, filename='...')
   toolbox.input(genv='uenv:MyFirstUenv.0@your_username', gvar='SECOND_KEY', unknown=True, filename='...')

5. In order to store and share your UEnv (vortex1 command) :

.. code-block:: bash

   $VORTEX/bin/uget.py push env MyFirstUenv.0@your_username

Modifying an existing UEnv
==========================
1. Create a copy of existing UEnv:
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: bash

   $VORTEX/bin/uget.py hack env Existing_UEnv.0@username_uenv_owner into MyNewUenv.0@your_username

Example:

.. code-block:: bash

   $VORTEX/bin/uget.py hack env cen.14@CONST_CEN into MyNewUenv.0@vernaym

**NB:** you can also upgrade your uenv with

.. code-block:: bash

   $VORTEX/bin/uget.py hack env Existing_UEnv.0@your_username into Existing_UEnv.1@your_username

Now, A text file ``MyNewUenv.0`` is create in ``$HOME/.vortexrc/hack/uget/your_username/env``

2. Modify new UEnv ``MyNewUenv.0``:
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

a. Upgrade a file already in UEnv

  --> Identify the file and key in ``MyNewUenv.0`` (ex: SSA_PARAMS="uenv:drdt_bst_fit_60.nc@CONST_CEN")

  --> Place new version of your file in ``$HOME/.vortexrc/hack/uget/your_username/data`` (ex: drdt_bst_fit_60_new.nc)

  --> Modify corresponding line in file ``MyNewUenv.0`` (ex: SSA_PARAMS="uenv:drdt_bst_fit_60_new.nc@your_username")

b. Add a new file in ``MyNewUenv.0``

  --> Place the file ``New_File.0`` in ``$HOME/.vortexrc/hack/uget/your_username/data``

  --> Add a line to the file ``MyNewUenv.0`` (ex: NEW_KEY="uenv:New_File.0@your_username")

Concatenation of 2 existing UEnv
================================

1. Get both UEnv:

.. code-block:: bash

   $VORTEX/bin/uget.py hack env UEnv1@user_who_own_this_uenv into UEnv1_copy@your_username
   $VORTEX/bin/uget.py hack env UEnv2@maybe_another_user into UEnv2_copy@your_username

Now, the files ``UEnv1_copy`` and ``UEnv2_copy`` are copied in ``$HOME/.vortexrc/hack/uget/your_username/env``

2. Concatenate both files in a new UEnv ``UEnv1_UEnv2``:

.. code-block:: bash

   cat UEnv1_copy UEnv2_copy > UEnv1_UEnv2

**NB:** it is of course posible to just pick few lines of each files

Using UEnv ``TargetUEnv.X`` owned by another user
=================================================
1. Explore ``TargetUEnv.X``:

.. code-block:: bash

   $VORTEX/bin/uget.py check env TargetUEnv.X@the_other_user

2. Get the file of interest with his key ``Key_from_TargetUEnv.X``:

.. code-block:: bash

   toolbox.input(genv='uenv:TargetUEnv.X@the_other_user', gvar='Key_from_TargetUEnv.X', unknown=True, filename='...')

Examples of available User Environments (UEnv)
==============================================
Started by Ange Haddjeri, to be continued...

Digital elevations models available in "uenv:dem.2@vernaym"::

  DEM_ALP1KM_EPSG4326
  DEM_FRANCE25M_L93
  DEM_GRANDESROUSSES25M_L93
  DEM_PYR1KM_EPSG4326
  RELIEF_FRANGP0025
  RELIEF_GRANDESROUSSES250M_4326
  RELIEF_GRANDESROUSSES250M_L93

Shapefiles availables in "uenv:shapefiles.1@vernaym"::

  FRENCH_CITIES
  MASSIFS_SAFRAN
  WORLD_BOUNDARIES

Uenv to reproduce simulations from M.Vernay PhD : "edelweiss_gr250_pappus.2@vernaym"

Uenv for latest version of the ANTILOPE post-processing algorithm : "edelweiss.3@vernaym"