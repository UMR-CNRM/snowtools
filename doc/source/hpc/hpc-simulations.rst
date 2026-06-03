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

Research configurations for the production of meteorological data


The "tasks" repository
^^^^^^^^^^^^^^^^^^^^^^

The task repository contains all the unit tasks related to a given configuration. Multiple tasks can be grouped into a driver to form a sequential chain.

The "conf" repository
^^^^^^^^^^^^^^^^^^^^^

The configuration repository contains configuration files associated to specific experiments.
A default configuration file defines default values for all variables of the configuration's tasks.

The "jobs" repository
^^^^^^^^^^^^^^^^^^^^^

The job repository contains files with pre-defined job descriptions.

The "mjkob" job launcher
------------------------

TODO:
* lien doc
* principe
* mtool

Specific launchers
------------------

TODO:
* "assim" script

Tutorial (user)
---------------

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

TODO : copie d'écran pour montrer ou trouver le numéro XXXX
/scratch/mtool/<username>/depot/mstepXXXX --> actual jobs ("step.0[123]") + output logs ("step.0[123].out)
/scratch/mtool/<username>/spool/XXXX --> répertoire d'exécution
/scratch/mtool/<username>/abort/XXXX --> copie de l'état du répertoire d'exécution après un crash
/scratch/mtool/<username>/cache --> local vortex (+ uenv) cache


Tutorial (developer)
--------------------

TODO :
* Création d'une nouvelle tâche unitaire et du cas test associé
* Création d'un driver utilisant la nouvelle tâche et du fichier de configuration associé
