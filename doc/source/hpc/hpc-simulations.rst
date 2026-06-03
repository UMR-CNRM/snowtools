Launching simulations on MF HPC
===============================

Code organisation
-----------------

Available applications and configurations
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Crocus
""""""

Research snowpack simulations based on FORCING file(s) of any origin.
Three different configurations are available:

* **determinitic**: simulations involving a single meteorological FORCING and Crocus configuration
* **escroc**: multiphysic snowpack simulations involving a single meteorological FORCING
* **assim**: snowpack simulations based on an ensemble of FORCING files (and optionaly several Crocus configurations) with the assimilation of snow observations

s2m
"""

SAFRAN-SURFEX/Crocus-MEPRA simulations in a "SAFRAN-massif" geometry.
Configurations:

* **reanalysis**: production of the reference S2M reanalysis dataset
* **reforecast**: production of a S2M reforecast for the training of operationnal new snow forecasts post-processing algorithm
* **oper (alp, pyr, cor, mac, vog, jur)**: S2M operational model chain

Edelweiss
"""""""""

Ensemble, distributed snowpack simulations at 250m resolution with the assimilation of snow observations.

Configurations:

* **prafr**: Operational Edelweiss configuration for avalanche danger forecasting over France
* **reanalysis**: Edelweiss reanalysis
* **reforecast** Edelweiss reforecast

Meteo
"""""

Research configurations for the production of meteorological data.


The "drivers" repository
^^^^^^^^^^^^^^^^^^^^^^^^

The *drivers* repository contains all the drivers (a sequence of unit tasks) related to a given configuration.

The "conf" repository
^^^^^^^^^^^^^^^^^^^^^

The *conf* repository contains configuration files associated to specific experiments.
A configuration file contains all the relevant variable values for a specific set of tasks relating to a particular experiment.
Default configuration files can also provide the minimum set of variable values required to perform specific tasks, such as launching SURFEX/Crocus simulations.

The "jobs" repository
^^^^^^^^^^^^^^^^^^^^^

The job repository contains files that provide the information needed to carry out one or several specific job(s) (the minimal information beiing the job(s) name(s) and the associated driver(s)).
The use of these files is optional, but it is recommended because it allows to set default CEN-specific launcher variables.

The "mjkob" job launcher
------------------------

This section provides an overview of the mkjob launcher from a CEN perspective.
The full mkjob package documentation is available here: https://cnrm-gmap.gitlab.meteo.fr/mkjob/index.html

Mkjob is a job script generator. It combines a job templates and a user-defined configuration file to produce and launch jobs on MF's HPC.
Although several job types ("profiles") are available (see https://cnrm-gmap.gitlab.meteo.fr/mkjob/configuration.html), only the "rd-belenos-mt" profile is used at CEN.
This profile is based on the MTOOL tool that splits the execution in separate job submissions (called "steps"):

* **step.01**: on a transfert node to fetch the input files from a remote server
* **step.02**: on a compute node to do the actual computation
* **step.03**: on a transfert node to archive the output files on aremote server

Here is an overview of the mkjob launcher arguments used at CEN:

* **-c [mandatory]**: Set the absolute path to the configuration file
* **-f [optional but recommended]**: Set the path to the job description file (in which the job *name* and associated *task* name, as well as the *profile=rd-belenos-mt* information must be provided). The job description file can contain several job descriptions (one description per line). In this case, all the jobs will be launched.
* **-a [optional]**: Additionnal user defined command line variables (format "arg1=XXX arg2=YYY [...]") with the highest level of priority (the values provided after the "-a" option overwrite both the values provided in the "job" file and the configuration file). These additionnal values apply to all jobs described in the job description file. Although this argument is optional, it should be used to set variables such as *datebegin*, *dateend* or *geometry*.
* **-n [optional]**: In case the job description file (*-f* argument) provides several job decriptions, the *-n* argument allows to choose a subset of jobs to launch based on the jobs names.

The following example of an mkjob command line allows to launch a SURFEX simulation with the minimal default configuration variables:

.. code-block::

   mkjob -f $SNOWTOOLS_CEN/vortex-cen/Crocus/deterministic/jobs/surfex.job -c $SNOWTOOLS_CEN/vortex-cen/Crocus/deterministic/conf/default_conf.ini -a xpid=first_test datebegin=2020080106 dateend=2021080106 geometry=cor2_allslopes

.. note::

   Alternatively, it is possible to avoid using a job description file (*-f* argument) and to provide the full job description (including the job name, the associated task and the profile) with the *-j* argument.

   The exact same result of the example above can be achieved with the following command line:

   mkjob -j profile=rd-belenos-mt name=surfex package=drivers task=surfex xpid=first_test datebegin=2020080106 dateend=2021080106 geometry=cor2_allslopes -c $SNOWTOOLS_CEN/vortex-cen/Crocus/deterministic/conf/default_conf.ini


Specific launchers
------------------

..
  TODO:
  * "assim" script

Tutorial (user)
---------------
..
  TODO : exemple de lancement d'une simu SURFEX
  * Lancement à partir d'un fichier de conf existant
      --> changement d'xpid
      --> changement de datebegin / dateend
      --> changement de geometry (création d'une nouvelle géométrie par extraction d'un sous domaine ?)

  * Lancement en modifiant un fichier de conf existant
      --> modification de l'exécutable (modification de UEnv)
      --> modification de la namelist (modification de UEnv)
      --> modification du FORCING
      --> modification de la configuration du job (walltime)


Simulation outputs
------------------
..
  TODO : copie d'écran pour montrer ou trouver le numéro XXXX
  /scratch/mtool/<username>/depot/mstepXXXX --> actual jobs ("step.0[123]") + output logs ("step.0[123].out)
  /scratch/mtool/<username>/spool/XXXX --> répertoire d'exécution
  /scratch/mtool/<username>/abort/XXXX --> copie de l'état du répertoire d'exécution après un crash
  /scratch/mtool/<username>/cache --> local vortex (+ uenv) cache


Tutorial (developer)
--------------------
..
  TODO :
  * Création d'une nouvelle tâche unitaire et du cas test associé
  * Création d'un driver utilisant la nouvelle tâche et du fichier de configuration associé
