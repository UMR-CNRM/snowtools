#! /usr/bin/python
# -*- coding: utf-8 -*-

'''
Created on 9 oct. 2012
@author: lafaysse
'''

import os


class SystemException(Exception):

    def __init__(self, status, command, errorcode=None):
        self.status = status
        self.command = command

    def __str__(self):
        return "The following command fails with error code " + str(self.status) + ":\n" + self.command


def callSystemOrDie(commande, errorcode=None):
    '''Execute a system command and kill the current program if it fails.'''

    status = os.system(commande)
    if status != 0:
        raise SystemException(status, commande)
    return status


def callSurfexOrDie(commande, moderun="NORMAL", nproc=1, errorcode=None):
    '''Execute a SURFEX binary'''

    os.environ["OMP_NUM_THREADS"] = "1"
    if ("PGD" in commande or "PREP" in commande) and moderun in ["MPI", "MPIRUN"]:
        moderun = "MPISINGLE"

    if moderun in ["MPI", "MPIRUN"]:
        if moderun == "MPIRUN":
            callSystemOrDie("mpirun -np " + str(nproc) + " " + commande, errorcode=errorcode)
        else:
            if "SLURM_NODES" in os.environ.keys():
                callSystemOrDie("srun --nodes=" + os.environ["SLURM_NNODES"] + " --ntasks=" + str(nproc) + " " + commande, errorcode=errorcode)
            else:
                callSystemOrDie("srun --ntasks=" + str(nproc) + " " + commande, errorcode=errorcode)

    elif moderun == "MPISINGLE":
        nproc = 1
        callSystemOrDie("mpirun -np " + str(nproc) + " " + commande, errorcode=errorcode)
    else:
        callSystemOrDie(commande, errorcode=errorcode)