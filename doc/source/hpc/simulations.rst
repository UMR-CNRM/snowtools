Launching simulations on MF HPC
===============================

This section contains all necessary information to launch simulations on HPC.


Code organisation in applications and configurations
----------------------------------------------------

.. automodule:: vortex_cen.Crocus
   :members:

.. automodule:: vortex_cen.s2m
   :members:

.. automodule:: vortex_cen.edelweiss
   :members:

..
  .. automodule:: vortex_cen.meteo
     :members:

    Research configurations for the production of meteorological data.

Code organisation for a given configuration
-------------------------------------------

All configurations follow the following directory structure :

.. code::

   vortex_cen/
       vapp/
           vconf/
               drivers/
               conf/
               jobs/

The *drivers* repository contains all the drivers (a sequence of unit tasks) related to a given configuration.

The *conf* repository contains configuration files associated to specific experiments.
A configuration file contains all the relevant variable values for a specific set of tasks relating to a particular experiment.
Default configuration files can also provide the minimum set of variable values required to perform specific tasks, such as launching SURFEX/Crocus simulations.

The *jobs* repository contains files that provide the information needed to carry out one or several specific job(s) (the minimal information beiing the job(s) name(s) and the associated driver(s)).
The use of these files is optional, but it is recommended because it allows to set default CEN-specific launcher variables.


Launching HPC simulations
-------------------------

The mkjob launcher
^^^^^^^^^^^^^^^^^^

This section provides an overview of the mkjob launcher from a CEN perspective.
The full mkjob package documentation is available here: https://cnrm-gmap.gitlab.meteo.fr/mkjob/index.html

Mkjob is a job script generator. It combines a job templates and a user-defined configuration file to produce and launch jobs on MF's HPC.
Although several job types ("profiles") are available (see https://cnrm-gmap.gitlab.meteo.fr/mkjob/configuration.html), only the "rd-belenos-mt" profile is used at CEN.
This profile is based on the MTOOL tool that splits the execution in separate job submissions (called "steps"):

* **step.01**: on a transfert node to fetch the input files from a remote server
* **step.02**: on a compute node to do the actual computation
* **step.03**: on a transfert node to archive the output files on aremote server

Here is an overview of the mkjob launcher arguments used at CEN (see also "mkjob -h" for more information):

* **-c [mandatory]**: Set the absolute path to the configuration file
* **-f [optional but recommended]**: Absolute path or filename of the job description file (in which the job *name* and associated *task* name, as well as the *profile=rd-belenos-mt* information must be provided). The job description file can contain several job descriptions (one description per line). In this case, all the jobs will be launched.
* **-a [optional]**: Additionnal user defined command line variables (format "arg1=XXX arg2=YYY [...]") with the highest level of priority (the values provided after the "-a" option overwrite both the values provided in the "job" file and the configuration file). These additionnal values apply to all jobs described in the job description file. Although this argument is optional, it should be used to set variables such as *datebegin*, *dateend* or *geometry*.
* **-n [optional]**: In case the job description file (*-f* argument) provides several job decriptions, the *-n* argument allows to choose a subset of jobs to launch based on the jobs names.
* **-l [info]**: Return the list of job descriptions in the job description file (for example to find a specific job name to launch with option *-n*)

The following example of an mkjob command line allows to launch a SURFEX simulation with the minimal default configuration variables:

.. code-block::

   mkjob -f $SNOWTOOLS_CEN/vortex_cen/Crocus/deterministic/jobs/surfex.job -c $SNOWTOOLS_CEN/vortex_cen/Crocus/deterministic/conf/default_conf.ini -a xpid=first_test datebegin=2020080106 dateend=2021080106 geometry=cor2_allslopes

If the job description file contains several jobs and you want to launch only a subset, il is possible to access the list of available job names with option -l. For example :

.. code-block::

   > mkjob -f $SNOWTOOLS_CEN/vortex_cen/s2m/reanalysis/jobs/safran.jobs -c $SNOWTOOLS_CEN/vortex_cen/s2m/reanalysis/conf/s2m_reanalysis.ini -l
   {'name': 'safran_reanalysis_alp', 'package': 'drivers', 'task': 'safran', 'profile': 'rd-belenos-mt', 'geometry': 'alp27_flat'}
   {'name': 'safran_reanalysis_pyr', 'package': 'drivers', 'task': 'safran', 'profile': 'rd-belenos-mt', 'geometry': 'pyr24_flat'}
   {'name': 'safran_reanalysis_cor', 'package': 'drivers', 'task': 'safran', 'profile': 'rd-belenos-mt', 'geometry': 'cor2_flat'}
   {'name': 'safran_reanalysis_mac', 'package': 'drivers', 'task': 'safran', 'profile': 'rd-belenos-mt', 'geometry': 'mac11_flat'}
   {'name': 'safran_reanalysis_jur', 'package': 'drivers', 'task': 'safran', 'profile': 'rd-belenos-mt', 'geometry': 'jur4_flat'}
   {'name': 'safran_reanalysis_vog', 'package': 'drivers', 'task': 'safran', 'profile': 'rd-belenos-mt', 'geometry': 'vog3_flat'}

Then you can choose a subset of jobs to launch with the "-n" option :

.. code-block::

   mkjob -f $SNOWTOOLS_CEN/vortex_cen/s2m/reanalysis/safran.jobs -c $SNOWTOOLS_CEN/vortex_cen/s2m/reanalysis/conf/s2m_reanalysis.ini -n safran_reanalysis_alp safran_reanalysis_pyr safran_reanalysis_cor -a datebegin=... dateend=... xpid=...

If the job description file contains several jobs and you want to launch only a subset, il is possible to access the list of available job names with option -l :

.. code-block::

   > mkjob -f $SNOWTOOLS_CEN/vortex_cen/s2m/reanalysis/jobs/safran.jobs -c $SNOWTOOLS_CEN/vortex_cen/s2m/reanalysis/conf/s2m_reanalysis.ini -l
   {'name': 'safran_reanalysis_alp', 'package': 'drivers', 'task': 'safran', 'profile': 'rd-belenos-mt', 'geometry': 'alp27_flat'}
   {'name': 'safran_reanalysis_pyr', 'package': 'drivers', 'task': 'safran', 'profile': 'rd-belenos-mt', 'geometry': 'pyr24_flat'}
   {'name': 'safran_reanalysis_cor', 'package': 'drivers', 'task': 'safran', 'profile': 'rd-belenos-mt', 'geometry': 'cor2_flat'}
   {'name': 'safran_reanalysis_mac', 'package': 'drivers', 'task': 'safran', 'profile': 'rd-belenos-mt', 'geometry': 'mac11_flat'}
   {'name': 'safran_reanalysis_jur', 'package': 'drivers', 'task': 'safran', 'profile': 'rd-belenos-mt', 'geometry': 'jur4_flat'}
   {'name': 'safran_reanalysis_vog', 'package': 'drivers', 'task': 'safran', 'profile': 'rd-belenos-mt', 'geometry': 'vog3_flat'}

Then you can choose a subset of jobs to launch with the "-n" option :

.. code-block::

   mkjob -f $SNOWTOOLS_CEN/vortex_cen/s2m/reanalysis/safran.jobs -c $SNOWTOOLS_CEN/vortex_cen/s2m/reanalysis/conf/s2m_reanalysis.ini -n safran_reanalysis_alp safran_reanalysis_pyr safran_reanalysis_cor -a datebegin=... dateend=... xpid=...


Configuration files
^^^^^^^^^^^^^^^^^^^

The "mkjob" configuration files have 3 levels of reading : the "DEFAULT" level, the "job" level and the "task" level.

* Configuration variables provided in the `[DEFAULT]` section are made available at each use of the configuration file.
* Configuration variables provided in the `[job]` sections are made available only for the corresponding job. These sections must contain the configuration variables of the job itself (partition, nnodes, walltime,...) and the potential variables with a common value for all tasks associated to the job. The variables already provided in the `[DEFAULT]` section are overwritten.
* Configuration variables provided in the `[task]` sections are made available only for the corresponding task. The variables already provided in the `[DEFAULT]` and `[job]` sections are overwritten.

.. note::
   * The names of the `job` sections is the name of the job as provided in the "job" files (name=<jobname>) or directly in the mkjob command line
   * The names of the `task` sections is the tag of the task in the driver of tasks. If a given task is used in different drivers or several times in a single driver, different tags must be given

If the job description file contains several jobs and you want to launch only a subset, il is possible to access the jobname wit option -l :

.. code-block::

   > mkjob -f safran.jobs -c $SNOWTOOLS_CEN/vortex_cen/s2m/reanalysis/conf/s2m_reanalysis.ini -l
   {'name': 'safran_reanalysis_alp', 'package': 'drivers', 'task': 'safran', 'profile': 'rd-belenos-mt', 'geometry': 'alp27_flat'}
   {'name': 'safran_reanalysis_pyr', 'package': 'drivers', 'task': 'safran', 'profile': 'rd-belenos-mt', 'geometry': 'pyr24_flat'}
   {'name': 'safran_reanalysis_cor', 'package': 'drivers', 'task': 'safran', 'profile': 'rd-belenos-mt', 'geometry': 'cor2_flat'}
   {'name': 'safran_reanalysis_mac', 'package': 'drivers', 'task': 'safran', 'profile': 'rd-belenos-mt', 'geometry': 'mac11_flat'}
   {'name': 'safran_reanalysis_jur', 'package': 'drivers', 'task': 'safran', 'profile': 'rd-belenos-mt', 'geometry': 'jur4_flat'}
   {'name': 'safran_reanalysis_vog', 'package': 'drivers', 'task': 'safran', 'profile': 'rd-belenos-mt', 'geometry': 'vog3_flat'}

Then you can choose a subset of jobs to launch with the "-n" option :

.. code-block::

   mkjob -f safran.jobs -c $SNOWTOOLS_CEN/vortex_cen/s2m/reanalysis/conf/s2m_reanalysis.ini -n safran_reanalysis_alp safran_reanalysis_pyr safran_reanalysis_cor -a datebegin=... dateend=... xpid=...


The mkjob helper
^^^^^^^^^^^^^^^^

An mkjob command launches a job associated to a specific driver.
From a user point of view, the first question that arises is : what are the possible configuration variables associated to this scpecific driver ?
The "mkjob-help" command line is provided to answer this question.

There are two possible usage of the mkjob help command :

1. provide directly the absolute path to the driver to document with the "-p" argument
2. provide the target application ("-a" argument), configuration ("-c" argument) and driver name ("-d" argument)

By default, the information displayed include:
* The driver's documentation
* The driver's tree (the sequence of tasks)
* A compact list of mandatory and optional configuration variables for this specific driver

Two additionnal arguments allow to refine the information to display :

* "--bytask" displays the list of mandatory and optional configuration variables for each individual task of the driver
* "--verbose" displays additional information such as the individual task's documentation and the full list of mandatory and optional configuration variables

.. note::

   * If no argument is provided, the general documentation of available applications in the vortex-cen package is displayed.
   * If only the target application ("-a" argument) is provided, the general documentation of this specific application is displayed
   * If only the target application ("-a" argument) and configuration ("-c" argument) are provided, the general documentation of this specific configuration is displayed


The "assim" launcher
^^^^^^^^^^^^^^^^^^^^
..
  TODO:
  * "assim" script

Tutorial (user)
---------------

Launch a SURFEX/Crocus experiment
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Deterministic SURFEX/Crocus simulations can be launched with the following job description file ("-f" argument of the mkjob launcher) : $SNOWTOOLS_CEN/vortex_cen/Crocus/deterministic/jobs/surfex.job

Default SAFRAN-based simulations
""""""""""""""""""""""""""""""""

A default configuration file is also available ("-c" argument of the mkjob command : $SNOWTOOLS_CEN/vortex_cen/Crocus/deterministic/conf/default_conf.ini
By default, SURFEX/Crocus simulations use FORCING files from the S2M-reanalysis dataset and the minimum information to provide is:

* the simulation's experiment identifier : *xpid* can be any string of length different from 4
* the simulation's period : *datebegin* and *dateend* must be between 01/08/1940 and 01/08/2025 in case default S2M reanalysis FORCING files are used
* the simulation's *geometry* must be a valid S2M-reanalysis geometry in case default S2M reanalysis FORCING files are used

These arguments can either be added in the configuration file, or be provided to the mkjob command line through the "-a" option :

**NB** it is recomended to add all your configuration variables other than *datebegin*, *dateend* and *geometry* in a configuration file (in the "conf" directory of the configuration) named after your experiment identifier (*xpid*).

.. code-block::

   mkjob -f $SNOWTOOLS_CEN/vortex_cen/Crocus/deterministic/jobs/surfex.job -c $SNOWTOOLS_CEN/vortex_cen/Crocus/deterministic/conf/default_conf.ini -a xpid=first_test datebegin=2020080106 dateend=2021080106 geometry=cor2_allslopes

Reproductible simulations with a user-controlled SURFEX/Crocus configuration
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

The default SURFEX/Crocus simulations described on the sections above are based on reference SURFEX/Crocus executables and namelists.

You can use your own namelist and executables by creating a new user environment (:ref:`uenv_creation`) or by modifying an existing one (:ref:`uenv_modification`).

After compiling SURFEX, put the compiled executables in the $HOME/.vortexrc/hack/uget/<your_username>/data with the following naming convention <exec_name>_<MPI/NOMPI>_<SURFEX_git_commit>, where:

* *exec_name* is "OFFLINE", "PGD", "PREP" or "SODA"
* *SURFEX_git_commit* can be retrieved from the ".git_info" in your SURFEX root directory (make sure that the compiled executables match this commit)

.. note::
   This step can also be achived with the "surfex_uenv.py" script available under vortex_cen/scripts

To create a new UEnv, open a file in $HOME/.vortexrc/hack/uget/<your_username>/env with the name of your choice (for example "new_env_name").
To update an existing UEnv named "reference_uenv" owned by the user "uenv_owner", create a copy of this UEnv into a "new_env_name":

.. code-block::

  uget hack env <reference_uenv>@<uenv_owner> into <new_env_name>

This creates the file $HOME/.vortexrc/hack/uget/<your_username>/env/<new_env_name> containing the "reference_uenv" information.
Then add or modify the following lines in file $HOME/.vortexrc/hack/uget/<your_username>/env/<new_env_name> :

.. code-block::

  MASTER_OFFLINE_MPI="uget:OFFLINE_MPI_<SURFEX_git_commit>@<your_username>"
  MASTER_OFFLINE_NOMPI="uget:OFFLINE_NOMPI_<SURFEX_git_commit>@<your_username>"
  MASTER_PGD_MPI="uget:PGD_MPI_<SURFEX_git_commit>@<your_username>"
  MASTER_PGD_NOMPI="uget:PGD_NOMPI_<SURFEX_git_commit>@<your_username>"
  MASTER_PREP_MPI="uget:PREP_MPI_<SURFEX_git_commit>@<your_username>"
  MASTER_PREP_NOMPI="uget:PREP_NOMPI_<SURFEX_git_commit>@<your_username>"
  MASTER_SODA_MPI="uget:SODA_MPI_<SURFEX_git_commit>@<your_username>"
  MASTER_SODA_NOMPI="uget:SODA_NOMPI_<SURFEX_git_commit>@<your_username>"

Similarly, put your SURFEX namelists in the $HOME/.vortexrc/hack/uget/<your_username>/data/namelists_surfex_vXX directory, and add the following line to the file $HOME/.vortexrc/hack/uget/<your_username>/env/<new_env_name> :

.. code-block::

   NAMELIST_SURFEX="uget:namelists_surfex_vXX.tar@<your_username>"

You can now use your own executables and namelists by adding the "surfex_uenv=new_env_name" to your configuration file, as well as the target namelist from your pool of namelists with the *namelist_source* variable.
For example if your $HOME/.vortexrc/hack/uget/<your_username>/data/namelists_surfex_vXX directory contains two namelists named "OPTIONS_PAPPUS.nam" and "OPTIONS_NO_PAPPUS.nam", you can choose to use the "OPTIONS_PAPPUS.nam" namelist with the following block in a "first_test.ini" configuration file deriving from the "default_conf.ini" configuration file :

.. code-block::

   [surfex]
   surfex_uenv=new_env_name
   namelist_source=OPTIONS_PAPPUS.nam

and the associated mkjob command line would be :

.. code-block::

   mkjob -f surfex.job -c $SNOWTOOLS_CEN/vortex_cen/Crocus/deterministic/conf/first_test.ini -a xpid=first_test datebegin=2020080106 dateend=2021080106 geometry=cor2_allslopes

This allows you to clearly identify the specific SURFEX configuration associated with your *xpid*, and to keep a written and traceable record of your different simulations (in addition to shorten the
mkjob command lines).

.. note::

   To fine-tune your configurations, use the 'mkjob-help -a Crocus -c deterministic -d surfex --bytask' command.
   This will tell you the list of possible configuration variables for each task of the surfex driver.

..
    Non-reproductible simulations with a user-controlled SURFEX/Crocus configuration
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    .. warning::

        The reproductibility of your simulations can not be guaranteed if you follow the instructions of this section.
        To ensure the reproductibility of you simulations, you should provide your namelist and executables in a proper User Environment (see previous section)

    You can also use your own namelist by setting the *namelist_path* variable to point to your target namelist.

    .. code-block::

       mkjob -f $SNOWTOOLS_CEN/vortex_cen/Crocus/deterministic/jobs/surfex.job -c $SNOWTOOLS_CEN/vortex_cen/Crocus/deterministic/conf/default_conf.ini -a xpid=first_test datebegin=2017080106 dateend=2018080106 geometry=GrandesRousses250m forcing_xpid=ALPAGA forcing_user=vernaym forcing_member=0 forcing_vapp=edelweiss forcing_vconf=grandesrousses250m forcing_vortex1=True namelist_path=/home/cnrm_other/cen/mrns/vernaym/EDELWEISS/namelist_surfex/OPTIONS_NO_PAPPUS.nam

    Similarly, you can use your own SURFEX executables by setting the *exesurfex* variable:

    .. code-block::

       mkjob -f $SNOWTOOLS_CEN/vortex_cen/Crocus/deterministic/jobs/surfex.job -c $SNOWTOOLS_CEN/vortex_cen/Crocus/deterministic/conf/default_conf.ini -a xpid=first_test datebegin=2020080106 dateend=2021080107 geometry=cor2_allslopes exesurfex=/home/cnrm_other/cen/mrns/vernaym/SURFEX/exe


Simulations based on other FORCING files
""""""""""""""""""""""""""""""""""""""""

If you want to use a FORCING file not coming from the S2M reanalysis, you have to provide some of the following variables :

* the *forcing_vapp* : providing the vapp level of the target FORCING file (optional, by default the simulation's *vapp* : "Crocus")
* the *forcing_vconf* : providing the vconf level of the target FORCING file (optional, by default the simulation's *vconf* : "determinitic")
* the *forcing_xpid* : providing the xpid level of the target FORCING file (optional, by default the simulation's *xpid*)
* the *forcing_user* : providing the name of the user who produced the FORCING file (optional if you produced the FORCING file yourself)
* the *forcing_block* : providing the block level of the target FORCING file (optional, by default "meteo")

You can also provide additional information, such as:

* the *forcing_member* providing a specific member value if the target FORCING file is part of an ensemble
* the *forcing_source_app* and/or *forcing_source_app*, if relevant

.. note::
   The geometry of the FORCING file should be the same as the simulation's geometry, but you can make this explicit by setting the *forcing_geometry* variable.
   IMPORTANT : this geometry must be properly described in your "geometries.ini" file in case it is a custom geometry.

.. note::
   If the target FORCING file was produced before the migration to the version 2 of vortex, you also have to add  "forcing_vortex1=True"

The following example illustrates the launch of a SURFEX/Crocus simulation with a 2D FORCING file from the ensemble "ALPAGA" experiment.

First, create a copy of "default_conf.ini" into a "first_test.ini" configuration file.

.. code-block:: bash

   cd $SNOWTOOLS_CEN/vortex_cen/Crocus/deterministic/conf
   cp default_conf.ini first_test.ini

The add the target forcing description and your surfex uenv informations to the "first_test.ini" configuration file:

.. code-block::

   [surfex]
   surfex_uenv=new_env_name
   namelist_source=OPTIONS_PAPPUS.nam
   forcing_xpid=ALPAGA
   forcing_user=vernaym
   forcing_member=0  # or member = 0 if you want the output file stored in a "mb000" sub-directory.
   forcing_vapp=edelweiss
   forcing_vconf=grandesrousses250m
   forcing_vortex1=True

Then launch mkjob with the "first_test.ini" configuration file in the "-c" argument

.. code-block::

   mkjob -f $SNOWTOOLS_CEN/vortex_cen/Crocus/deterministic/jobs/surfex.job -c $SNOWTOOLS_CEN/vortex_cen/Crocus/deterministic/conf/first_test.ini -a xpid=first_test datebegin=2021080107 dateend=2022080106 geometry=GrandesRousses250m


Configuring your job
""""""""""""""""""""

You can set your job configuration with the following variables :

* *time* : the job's wall time (default : "0:20:00",  20 minutes)
* *nnodes* : the number of nodes to allocate to the job (default : 1)
* *partition* : the target partition (default : normal256)

For example, to increase your job's wall time to 1 hour, add "time=1:00:00" to your first_test.ini configuration file:

.. code-block::

   [surfex]
   surfex_uenv=new_env_name
   namelist_source=OPTIONS_PAPPUS.nam
   forcing_xpid=ALPAGA
   forcing_user=vernaym
   forcing_member=0  # or member = 0 if you want the output file stored in a "mb000" sub-directory.
   forcing_vapp=edelweiss
   forcing_vconf=grandesrousses250m
   forcing_vortex1=True
   time=1:00:00

..
  TODO : exemple de lancement d'une simu SURFEX

  * Force a specific PREP file

Reproductibility check
----------------------

In certain situations, you may wish to verify that the files produced are identical to the reference files produced by a previous experiment that you are attempting to replicate.
In this case, simply provide the reference experiment identifier in the "diff_xpid" configuration variable (and optionaly the username of the owner of this experiment in the "diff_user" configuration variable and the block in the "diff_block" configuration variable) :

.. code-block::

   mkjob -f $SNOWTOOLS_CEN/vortex_cen/Crocus/deterministic/jobs/surfex.job -c $SNOWTOOLS_CEN/vortex_cen/Crocus/deterministic/conf/default_conf.ini -a xpid=first_test datebegin=2020080106
   dateend=2021080106 geometry=cor2_allslopes diff_xpid=<reference_xpid> [diff_user=<username>] [diff_block=<block_of_reference_file>]

.. note::

   the `diff_*` variables can be parsed directly in the mkjob command line since they don't affect the simulation's output


Reproduce s2m test cases
------------------------


.. code-block:: bash

    # S2M reanalysis test case:
    s2m research -r alp_allslopes -b 20220801 -e 20230801 -m safran -f reanalysis2020.2 -o reanalysis_test -n $SNOWTOOLS_CEN/snowtools/DATA/OPTIONS_V9_reanalysis.nam

    # Equivalent command in snowtools3 :
    mkjob -c $SNOWTOOLS_CEN/vortex_cen/Crocus/deterministic/conf/s2m_reanalysis_testcase.ini -f $SNOWTOOLS_CEN/vortex_cen/Crocus/deterministic/jobs/surfex.job -a datebegin=20220801 dateend=20230801 geometry=alp_allslopes

.. code-block:: bash

    # S2M ESCROC test case:
    s2m research -r cdp -b 1994100101 -e 2014100100 -x 2014100100 -m ESM-SnowMIP -f obs@lafaysse -o E2_test --task=escroc --escroc=E2

    # Equivalent command in snowtools3 :
    mkjob -c $SNOWTOOLS_CEN/vortex_cen/Crocus/escroc/conf/s2m_escroc_testcase.ini -f $SNOWTOOLS_CEN/vortex_cen/Crocus/escroc/jobs/escroc.job -a datebegin=1994100101 dateend=2014100100 geometry=cdp

..
    # Stochastic perturbation test case:
    s2m research -r cor_flat -b 20200801 -e 20210801 -m s2m -f reanalysis2020.2 -o perturb --task='croco_perturb' --nmembers=80

    # Croco openloop test case (before running this test case, please define postes_12_csv geometry in $HOME/.vortexrc/geometries.ini as in ~lafaysse/.vortexrc/geometries.ini ) :
    s2m research -r postes_12_csv -b 2013080106 -e 2014063006 -x 20160801 -m safran -f forcing_20132014B_31D_11_t1500_160@fructusm -o testopenloop -n ~lafaysse/croco/OPTIONS_MOTHER_DEP.nam --task='croco' --croco='openloop' --escroc=E1notartes --nmembers=35 --nforcing=35 --conf=/home/lafaysse/croco/conf.ini -s ~lafaysse/SURFEX/cen/exe_mpi

    # Croco test case with assim of real observations:
    s2m research -r postes_12_csv -b 2013080106 -e 2014063006 -x 20160801 -m safran -f forcing_20132014B_31D_11_t1500_160@fructusm -o test0l -n ~lafaysse/croco/OPTIONS_MOTHER_DEP.nam --task='croco' --croco='real' --escroc=E1notartes --nmembers=35 --nforcing=35 --conf=/home/lafaysse/croco/conf.ini -s ~lafaysse/SURFEX/cen/exe_mpi --obsxpid=obs@lafaysse --sensor=bdclim

    # Replay operational analysis and forecast:
    s2m oper -b 2025091503 -r alp --dev
    s2m oper -b 2025091503 -r alp --task='forecast' --dev

    # Building of reforecast initial conditions test case:
    s2m research -r vog3_allslopes -b 20000801 -e 20010801 -a 400 -m s2m -f reanalysis_era5.2023 -p reanalysis_era5.2025.2 -o initialconditions_test -n $SNOWTOOLS_CEN/snowtools/DATA/OPTIONS_V9_reanalysis_forprep.nam --task='surfex_dailyprep' --walltime='00:45:00'

    # Reforecast test case
    s2m research -b 20000302 -e 20000327 -r vog3_allslopes -n /home/cnrm_other/cen/mrns/lafaysse/PycharmProjects/snowtools_git/snowtools/DATA/OPTIONS_reforecast.nam --task='reforecast' -m safran -f reforecast_2023 --nmembers=11 -p initdaily_era5.2025.2@lafaysse -o reforecast_test

