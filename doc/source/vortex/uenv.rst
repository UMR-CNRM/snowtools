Constant data managment with User Environments
==============================================

The general documentation of the uenv/uget tool is available here:
   https://cnrm-gmap.gitlab.meteo.fr/vortex-gco/index.html

There is a list of useful commands at the end of this section.

The main motivations to use User Environments are :
* allowing for reliable, standardized data archiving
* ensuring the simulations reproductibility
* facilitating and optimizing the exchange of data between users
* partitioning the data used in different simulations in a clear and secure way
* minimizing the number of circulating files

Install the vortex-gco plugin
-----------------------------

On MF HPC:

.. code-block:: bash

   pip install --find-links /home/verolive/wheels vortex-gco

On any other MF server:

.. code-block:: bash

   pip install vortex-gco


Definition of a user environment
--------------------------------

User environments refer to user-managed collections of coherent static data files. A user environment is the combination of:
* an ensemble of static data files, potentially owned by different users
* a catalogue text file associating a unique key (named "gvar") to identify each file. This catalogue file is refered to as "uenv" in this documentation.

.. _uenv_creation:

Create a User Environment from scratch
--------------------------------------

Step by step
^^^^^^^^^^^^

1. Put the file(s) you need in your UEnv in ``$HOME/.vortexrc/hack/uget/your_username/data``

Name your file(s) with a distinctive suffix (version number, git commit or tag,...) : ``AFirstFile.0``, ``ASecondFile.1``

.. note::

   Namelists are a special case : serveral namelist files can be added under ``$HOME/.vortexrc/hack/uget/your_username/data/<namelist_dir_XXX>``

2. Create a text file with a distinctive name which is the name of your UEnv (ex ``MyFirstUenv.0``) in ``$HOME/.vortexrc/hack/uget/your_username/env``

3. In this text file, associate a key to each file:

.. code-block:: bash

   FIRST_KEY="uenv:AFirstFile.0@your_username"
   SECOND_KEY="uenv:ASecondFile.1@your_username"

.. note::

   For namelists, the line looks like :
   NAMELISTS_<MODEL>="uenv:namelist_dir_XXX@your_username"

4. UEnv is ready, you can access to file(s) with the command:

.. code-block:: bash

   toolbox.input(genv='uenv:MyFirstUenv.0@your_username', gvar='FIRST_KEY', unknown=True, filename='...')
   toolbox.input(genv='uenv:MyFirstUenv.0@your_username', gvar='SECOND_KEY', unknown=True, filename='...')

.. note::

   For namelists, the additional *namelist_source* entry must be provided to specify the target namelist's name from the pool of namelists:
   toolbox.input(genv='uenv:MyFirstUenv.0@your_username', gvar='SECOND_KEY', unknown=True, filename='...', namelist_source=<name_of_namelist>)

5. In order to archive and share your UEnv :

.. code-block:: bash

   uget push env MyFirstUenv.0@your_username

**NB:** On HPC this can only be done from a TRANSFERT node

.. note::

   * By convention, the "keys" of SURFEX executables are MASTER_SURFEX_<NAME_OF_EXEC>_MPI or MASTER_SURFEX_<NAME_OF_EXEC>_NOMPI.
   * It is possible to use files from another user's UEnv in your own UEnv : simply copy the corresponding lines from their UEnv file into yours

User Environments for surfex simulations
----------------------------------------

Constant files for SURFEX simulations can be grouped in 3 different categories:

* files that do not change frequently, for which a default uenv is commonly used (eccoclimap*, drdt_bst_fit, SandDB, ClayDB).
  The associated default uenv for these files can be set with the "consts_surfex_uenv" configuration variable.

* SURFEX executables can be provided in a specific uenv identified by the "surfex_uenv" configuration variable.

* SURFEX namelists can be provided in a specific uenv identified by the "namelists_surfex_env" configuration variables.

.. note::

   Of course it is possible to group all these constant files into a single user environment and simply use the "uenv" configuration variable, following the instructions provided in the next section

.. _uenv_surfex:

Example : create a UEnv with SURFEX executables and namelists
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. _uenv_modification:

Modifying an existing UEnv
--------------------------

1. Create a copy of existing UEnv:
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: bash

   uget hack env Existing_UEnv.0@username_uenv_owner into MyNewUenv.0@your_username

Now, A text file ``MyNewUenv.0`` has been creates in ``$HOME/.vortexrc/hack/uget/your_username/env``

**NB:** On HPC this can only be done from a TRANSFERT node

Example:

.. code-block:: bash

   ssh belenostransfert
   source path/to/venv/with/vortex-gco/bin/activate
   uget hack env Existing_UEnv.0@username_uenv_owner into MyNewUenv.0@your_yousername

.. note::

    A copy of an existing UEnv should always have a different name than the original one to avoid overwriting it.
    The good practice is to increment a version number or give a new explicit name.


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
--------------------------------

1. Get both UEnv:

.. code-block:: bash

   uget hack env UEnv1@user_who_own_this_uenv into UEnv1_copy@your_username
   uget hack env UEnv2@maybe_another_user into UEnv2_copy@your_username

Now, the files ``UEnv1_copy`` and ``UEnv2_copy`` are copied in ``$HOME/.vortexrc/hack/uget/your_username/env``

2. Concatenate both files in a new UEnv ``UEnv1_UEnv2``:

.. code-block:: bash

   cat UEnv1_copy UEnv2_copy > UEnv1_UEnv2

**NB:** it is of course posible to just pick few lines of each files

Using UEnv ``TargetUEnv.X`` owned by another user
-------------------------------------------------

1. Explore ``TargetUEnv.X``:

.. code-block:: bash

   uget check env TargetUEnv.X@the_other_user

2. Get the file of interest with his key ``Key_from_TargetUEnv.X``:

.. code-block:: bash

   toolbox.input(genv='uenv:TargetUEnv.X@the_other_user', gvar='Key_from_TargetUEnv.X', unknown=True, filename='...')


Archiving a User Environment
----------------------------

Once you uenv is ready, check that all associated data files exist with :

.. code-block::

    uget check env <target_uenv_name>@<username>

The archive your uenv with :

.. code-block::

    uget push env <target_uenv_name>@<username>

**NB** : This "freezes" your uenv and makes it available to other users (just as a "git push"), from now on you should not modify anything from this uenv !
You should also remove the local copies to avoid inconsistencies between local and archived versions:

.. code-block::

    uget clean_hack

.. note::

   To ensure that archived files are not modified, their archive location is voluntarily "encrypted"


If you detect a mistake in one of the files associated to your uenv at this point, create a new version of your uenv with :

.. code-block::

    uget hack env <former_uenv_name>@<username> into <new_uenv_name>

.. note::

   A coment line at the begining of the <new_uenv_name> catalogue file is added to track its provenance

and create the new version of the faulty file, if necessary by "hacking" the previous one :

.. code-block::

    uget hack data <faulty_data_name>@<username> into <new_data_name>

Do not forget to change the line corresponding to the faulty data file in the <new_uenv_name> catalogue :

.. code-block::

    FAULTY_DATA_KEY="uget:<new_data_name>@<username>"

instead of :

.. code-block::

    FAULTY_DATA_KEY="uget:<faulty_data_name>@<username>"

.. note::

   * The "FAULTY_DATA_KEY" remains unchanged between the two versions

   * No need to change anything else, all other data files remain properly identified in the <new_uenv_name>


Data tree
---------

When retrieved with Vortex, data from a uenv can come from :

* The 'hack' data tree under $HOME/.vortexrc/hack/uget/<username>. This data tree should be used only during the creation and test of a new uenv.

* The 'Archive' date tree. This is the data tree from which valid and shared uenv data are retrieved, potentially from different users.

* The 'local' data tree. This is a local copy of the 'Archive' date tree, which is created when data is first fetched in order to act as a cache for future use.

.. note::

    Fetching data from the "Archive" or "Local" data trees is transparent to the user in most cases.
    The only exceptions are linked to issues with transfers from the archive (passord updates, call from a login or compute nodes on HPC,...).


Examples of available User Environments (UEnv)
----------------------------------------------

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

UEnv vs GEnv
------------

Constant files used in operationnal simulations are managed by the GCO team with the "genv" equivalent of user environments.
To access data from an operationnal "genv", simply provide the name of the genv without the 'uenv:' prefix nor the '@<username>' suffix:

.. code-block::

   uenv = cen01_cen@s2m-op2.13

identifies an opertional genv, whereas

.. code-block::

   uenv = uenv:cen01_cen@s2m-op2.13@vernaym

identifies the uenv of user "vernaym".

Usefull commands
----------------

List available user environments for a given user :

.. code-block::

    uget list env from <username>


Check data availability for a specific uenv :

.. code-block::

    uget check env <uenv_name>@<username>

This returns the location of the catalogue file ("Hack" and/or "Archive"), as well as the list of associated files and their location.

.. note::

    The location can be "Hack" (developments in progress) and/or Archive (target already pushed).

Cloning an existing uenv catalogue file:

.. code-block::

    uget hack env <target_uenv_name>@<username> into <new_uenv_name>

Cloning an existing genv catalogue file:

.. code-block::

    uget hack genv <target_genv_name> into <new_uenv_name>

.. note::

    It is recommended to clone an existing uenv / genv rather than to create a new one from scratch whenever possible because it adds a comment to track its provenance.

Cloning an existing data file:

.. code-block::

    uget hack data <target_data_name>@<username> into <new_data_name>

Cloning an existing operational data file:

.. code-block::

    uget hack gdata <target_data_name> into <new_data_name>

Commit / push a uenv and all its associated data files :

.. code-block::

    uget push env <target_uenv_name>@<username>

**NB** : once a uenv catalogue file and its associated data files have been pushed, remove the local copies to avoid inconsistencies between local and archived versions:

.. code-block::

    uget clean_hack

Commit / push a specific data file :

.. code-block::

    uget push data <target_data_name>@<username>

**NB** : 

Compare two catalogues (uenv and/or genv alike), or a uenv catalogue with its "parent" in case it has been properly "hacked" :

.. code-block::

    uget diff env <first_uenv_name>@<username> wrt env <second_uenv_name>@<username>
    uget diff env <uenv_name>@<username> wrt genv <genv_name>
    uget diff env <uenv_name>@<username> wrt parent

Get the list of data files that differ between two catalogues:

.. code-block::

    uget export env <first_uenv_name>@<username> wrt env <second_uenv_name>@<username>
    uget export env <uenv_name>@<username> wrt genv <genv_name>


FAQ - Frequent issues
---------------------

* On HPC, fetch/save data from a TRANSFERT node because the uget tool is based on ftget/ftput commands, which are unavailable on login/compute nodes and raise the following error:

.. code-block:: bash

    # [2026/04/21-12:40:16][vortex.tools.systems][spawn:1120][CRITICAL]: Could not call ['ftput', '-o', 'mkdir', '-q', '-h', 'hendrix.meteo.fr', '-u', 'vernaym', 'myuenv', 'uget/env/6/myuenv']
    Traceback (most recent call last):
    [...]
    FileNotFoundError: [Errno 2] No such file or directory: 'ftput'

* Team UEnv (@CONST_CEN, @SAFRAN_CEN and @SURFEX_CEN) are no longer possible with Vortex2 (see snowtools ticket #306 or https://gitlab.meteo.fr/cnrm-gmap/vortex-gco/-/work_items/4#note_93731 for more information)

* The name of the repository conatining the SURFEX namelists must be suffixed ".tar" in the uenv.
  The following works fine :

.. code-block::

   NAMELIST_SURFEX="uget:namelists_surfex9_0_crocus3_0_2_std.tar@lafaysse"

whereas

.. code-block::

   NAMELIST_SURFEX="uget:namelists_surfex9_0_crocus3_0_2_std@lafaysse"

raises the following error :

.. code-block::

    # [2026/08/17-12:36:48][vortex.data.abstractstores][incacheget:1180][INFO]: incacheget on uget://uget.hack.fr//data/namelists_surfex9_0_crocus3_0_2_std (to: namelists_surfex9_0_crocus3_0_2_std)
    # [2026/08/17-12:36:48][vortex.data.abstractstores][incacheget:1199][INFO]: incacheget retrieve rc=True location=/home/cnrm_other/cen/mrns/vernaym/.vortexrc/hack/uget/vernaym/data/namelists_surfex9_0_crocus3_0_2_std
    # [2026/08/17-12:36:48][vortex_gco.data.stores][ugetget:1664][ERROR]: 'namelists_surfex9_0_crocus3_0_2_std' should be a tarfile
