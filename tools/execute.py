#! /usr/bin/python
# -*- coding: utf-8 -*-

'''
Created on 9 oct. 2012
@author: lafaysse
'''

import os
import sys

def callSystemOrDie(commande,errorcode=None):
    status=os.system(commande)
    if status!=0:        
        if type(errorcode) is int:
            print("The following command fails with error code "+str(status)+":\n"+commande)
            sys.exit(errorcode)
        else:
#             sys.exit("The following command fails with error code "+str(status)+":\n"+commande)
            raise BaseException("The following command fails with error code "+str(status)+":\n"+commande)
    return status

def callSurfexOrDie(commande,moderun="NORMAL",nproc=1,errorcode=None):
    if moderun in ["MPI","MPIRUN"]:
        if os.environ["MODERUN"]=="MPIRUN":
            callSystemOrDie("mpirun -np "+nproc+" "+commande,errorcode=errorcode)
        else:
            if "SLURM_NODES" in os.environ.keys():                   
                callSystemOrDie("srun --nodes="+os.environ["SLURM_NNODES"]+" --ntasks="+nproc+" "+commande,errorcode=errorcode)
            else:
                callSystemOrDie("srun --ntasks="+nproc+" "+commande,errorcode=errorcode)

    elif moderun=="MPISINGLE":
        nproc="1"
        callSystemOrDie("mpirun -np "+nproc+" "+commande,errorcode=errorcode)
    else:        
        callSystemOrDie(commande,errorcode=errorcode)            

   