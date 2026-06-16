Launching simulations on MF HPC
===============================

This section contains all necessary information to launch simulations on HPC.


Code organisation in applications and configurations
----------------------------------------------------

.. automodule:: Crocus
   :members:

.. automodule:: s2m
   :members:

.. automodule:: edelweiss
   :members:

..
  .. automodule:: meteo
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

.. note::

   Alternatively, it is possible to avoid using a job description file (*-f* argument) and to provide the full job description (including the job name, the associated task and the profile) with the *-j* argument.

   The exact same result of the example above can be achieved with the following command line:

   mkjob -j profile=rd-belenos-mt name=surfex package=drivers task=surfex xpid=first_test datebegin=2020080106 dateend=2021080106 geometry=cor2_allslopes -c $SNOWTOOLS_CEN/vortex_cen/Crocus/deterministic/conf/default_conf.ini

If the job description file contains several jobs and you want to launch only a subset, il is possible to access the list of available job names with option -l :

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

For more information, try:

.. code-block::

    mkjob-help -h

..
  TODO: Compléter doc

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

.. code-block::

   mkjob -f $SNOWTOOLS_CEN/vortex_cen/Crocus/deterministic/jobs/surfex.job -c $SNOWTOOLS_CEN/vortex_cen/Crocus/deterministic/conf/default_conf.ini -a xpid=first_test datebegin=2020080106 dateend=2021080106 geometry=cor2_allslopes


Reproductible simulations with a user-controlled SURFEX/Crocus configuration
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

The default SURFEX/Crocus simulations described on the sections above are based on reference SURFEX/Crocus executables and namelists.

You can use your own namelist and executables by creating a new user environment (:ref:`uenv_creation`) or by modifying an existing one (:ref:`uenv_modification`).

After compiling SURFEX, put the compiled executables in the $HOME/.vortexrc/hack/uget/<your_username>/data with the following naming convention <exec_name>_<MPI/NOMPI>_<SURFEX_git_commit>, where:

* *exec_name* is "OFFLINE", "PGD", "PREP" or "SODA"
* *SURFEX_git_commit* can be retrieved from the ".git_info" in your SURFEX root directory (make sure that the compiled executables match this commit)

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

Similarly, put your SURFEX namelists in the $HOME/.vortexrc/hack/uget/<your_username>/data/namelists_surfex directory, and add the following line to the file $HOME/.vortexrc/hack/uget/<your_username>/env/<new_env_name> :

.. code-block::

   NAMELIST_SURFEX="uget:namelists_surfex.tar@<your_username>"

You can now use your own executables and namelists by adding the "surfex_uenv=new_env_name" to your mkjob command line, as well as the target namelist from your pool of namelists with the *namelist_source* argument.
For example if your $HOME/.vortexrc/hack/uget/<your_username>/data/namelists_surfex directory contains two namelists named "OPTIONS_PAPPUS.nam" and "OPTIONS_NO_PAPPUS.nam", you can choose to use the "OPTIONS_PAPPUS.nam" namelist with the following command line :

.. code-block::

   mkjob -f $SNOWTOOLS_CEN/vortex_cen/Crocus/deterministic/jobs/surfex.job -c $SNOWTOOLS_CEN/vortex_cen/Crocus/deterministic/conf/default_conf.ini -a xpid=first_test datebegin=2020080106 dateend=2021080106 geometry=cor2_allslopes surfex_uenv=new_env_name namelist_source=OPTIONS_PAPPUS.nam

**NB** it is recomended to add all your configuration variables other than *xpid*, *datebegin*, *dateend* and *geometry* in a configuration file (in the "conf" directory of the configuration) named after your experiment identifier (*xpid*).
In the previous example, a configuration files names "first_test.ini" deriving from the "default_conf.ini" configuration file would contain the additional following lines :

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
   If the target FORCING file was produced with a version of vortrex <2, you also have to add  "forcing_vortex1=True"

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

   mkjob -f surfex.job -c $SNOWTOOLS_CEN/vortex_cen/Crocus/deterministic/conf/first_test.ini -a xpid=first_test datebegin=2021080107 dateend=2022080106 geometry=GrandesRousses250m


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


Simulation log
--------------
..
  TODO : copie d'écran pour montrer ou trouver le numéro XXXX
  /scratch/mtool/<username>/depot/mstepXXXX --> actual jobs ("step.0[123]") + output logs ("step.0[123].out)
  /scratch/mtool/<username>/spool/XXXX --> répertoire d'exécution
  /scratch/mtool/<username>/abort/XXXX --> copie de l'état du répertoire d'exécution après un crash
  /scratch/mtool/<username>/cache --> local vortex (+ uenv) cache


