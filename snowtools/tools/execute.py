# -*- coding: utf-8 -*-

'''
Created on 9 oct. 2012
@author: lafaysse
'''

import os
import subprocess
import sys


class SystemException(Exception):
    """Exception for a system command"""

    def __init__(self, status, command, errorcode=None):
        self.status = status
        self.command = command

    def __str__(self):
        return "The following command fails with error code " + str(self.status) + ":\n" + self.command


def callSystemOrDie(commande, errorcode=None):
    """Method to execute a system command and kill the current program if it fails."""

    status = subprocess.call(commande.split(), stdout=sys.stdout, stderr=sys.stderr)

    if status != 0:
        raise SystemException(status, commande)
    return status


def callSurfexOrDie(commande, moderun="NORMAL", nproc=1, errorcode=None):
    """Method to execute a SURFEX binary and kill the current program if it fails.
    Include the setting of MPI and OpenMP tasks, and the extension of stack memory.
    """

    set_stack_unlimited()
    os.environ["OMP_NUM_THREADS"] = "1"
    if ("PGD" in commande or "PREP" in commande) and moderun in ["MPI", "MPIRUN"]:
        moderun = "MPISINGLE"

    if moderun in ["MPI", "MPIRUN"]:
        if moderun == "MPIRUN":
            callSystemOrDie("mpirun -np " + str(nproc) + " " + commande, errorcode=errorcode)
        else:
            if "SLURM_NODES" in list(os.environ.keys()):
                callSystemOrDie("srun --nodes=" + os.environ["SLURM_NNODES"] + " --ntasks="
                                + str(nproc) + " " + commande, errorcode=errorcode)
            else:
                callSystemOrDie("srun --ntasks=" + str(nproc) + " " + commande, errorcode=errorcode)

    elif moderun == "MPISINGLE":
        nproc = 1
        callSystemOrDie("mpirun -np " + str(nproc) + " " + commande, errorcode=errorcode)
    else:
        callSystemOrDie(commande, errorcode=errorcode)


def set_stack_unlimited():
    # Without the following lines, the worse segmentation faults you can ever imagine
    # Equivalent to bash command ulimit -s unlimited
    # Take care : some systems do not allow to force resource.RLIMIT_STACK at resource.RLIM_INFINITY,
    # so it is safer to get the system hard limit first.
    # Moreover, this can not be done on MacOS
    if sys.platform != 'darwin':
        import resource
        soft, hard = resource.getrlimit(resource.RLIMIT_STACK)  # @UnusedVariable
        resource.setrlimit(resource.RLIMIT_STACK, (hard, hard))