Launching simulations on MF HPC
===============================

Code organisation
-----------------

Application and configuration levels
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

TODO:
* Research
    Snowpack simulation --> vapp=Crocus
        1 member --> vconf = deterministic
        Snow data assimilation --> vconf = assim
        Multiphysics --> vconf = escroc
    Production of meteorological FORCING --> vapp=meteo
        ...

* oper / real-time / reference dataset production :
    meteo SAFRAN --> vapp = s2m
        reference reanalysis dataset --> vconf = reanalysis
        reference reforecast dataset -->  vconf = reforecast
        real-time --> vconf = alp / pyr / cor / mac / vog / jur

    meteo 250m --> vapp = edelweiss
        real-time --> vconf = prafr
        reanalysis of the operationnal configuration --> vconf = reanalysis
        reforecast of the operationnal configuration --> vconf = reforecast

The "jobs" repository
^^^^^^^^^^^^^^^^^^^^^

The "tasks" repository
^^^^^^^^^^^^^^^^^^^^^^

The "conf" repository
^^^^^^^^^^^^^^^^^^^^^

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
