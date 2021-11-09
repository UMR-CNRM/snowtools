# -*- coding: utf-8 -*-
"""
Created on 21  mars 2019
Run a CRAMPON assimilation sequence on a multinode
@author: cluzetb
"""
import os
import shutil

import numpy as np

from snowtools.utils.resources import InstallException
from snowtools.tasks.vortex_kitchen import Vortex_conf_file
from snowtools.tools.update_namelist import update_surfex_namelist_file
from snowtools.tasks.vortex_kitchen import vortex_kitchen
from snowtools.utils.ESCROCsubensembles import ESCROC_subensembles


class crampon_vortex_kitchen(vortex_kitchen):
    """
    Interface between s2m command line and vortex utilities (tasks and mk_jobs)
    crampon_multinode
    based on vortex_kitchen from M. Lafaysse
    """

    def __init__(self, options):
        """
        Constructor
        """
        # Check if a vortex installation is defined
        self.check_vortex_install()
        self.options = options
        # Initialization of vortex variables
        self.vapp = "s2m"
        self.vconf = self.options.region

        self.workingdir = self.options.workdir + "/" + self.vapp + "/" + self.vconf

        self.xpid = self.options.diroutput
        self.jobtemplate = "job-vortex-default.py"

        self.create_env()
        self.enforce_nmembers()  # this must be moved to the preprocess step of offline I think.
        self.create_conf()

    def check_vortex_install(self):
        """Vortex install check"""
        if "VORTEX" not in os.environ.keys():
            raise InstallException("VORTEX environment variable must be defined towards a valid vortex install.")

    def create_env(self):
        """Prepare environment"""
        if not os.path.isdir(self.workingdir):
            os.makedirs(self.workingdir)
        os.chdir(self.workingdir)

        if not os.path.islink("vortex"):
            os.symlink(os.environ["VORTEX"], "vortex")
        if not os.path.islink("tasks"):
            os.symlink(os.environ["SNOWTOOLS_CEN"] + "/tasks", "tasks")

        for directory in ["conf", "jobs"]:
            if not os.path.isdir(directory):
                os.mkdir(directory)

        os.chdir("jobs")
        if not os.path.islink(self.jobtemplate):
            os.symlink(os.environ["SNOWTOOLS_CEN"] + "/jobs/" + self.jobtemplate, self.jobtemplate)

    def enforce_nmembers(self):
        """enforce NENS in the namelist to the presecribed s2m argument value. Mandatory for SODA"""
        # options.namelist is already an absolute path.
        update_surfex_namelist_file(self.options.datedeb, namelistfile=self.options.namelist,
                                    dateend=self.options.datefin,
                                    updateloc=False, nmembers=self.options.nmembers)

    def create_conf(self):
        """ Prepare configuration file from s2m options"""

        confname = "../conf/" + self.vapp + "_" + self.vconf + ".ini"  # this file already exists if
        # self.options.crampon

        if os.path.exists(confname):
            print('remove vortex conf_file')
            os.remove(confname)

        print('copy conf file to vortex path')
        shutil.copyfile(self.options.crampon, confname)

        conffile = Vortex_conf_file(self.options, confname, 'a')

        conffile.set_field("DEFAULT", 'meteo', self.options.model)
        conffile.set_field("DEFAULT", 'geometry', self.vconf)
        conffile.set_field("DEFAULT", 'forcing', self.options.forcing)
        conffile.set_field("DEFAULT", 'nforcing', self.options.nforcing)
        conffile.set_field("DEFAULT", 'datedeb', self.options.datedeb)
        conffile.set_field("DEFAULT", 'datefin', self.options.datefin)

        # ########### READ THE USER-PROVIDED conf file ##########################
        # -> in order to append datefin to assimdates and remove the exceding dates.
        # -> in order to check if membersIDs were specified.

        # local import since there are dependencies with vortex.
        from snowtools.assim.utilcrocO import read_conf
        import bisect

        confObj = read_conf(confname)
        intdates = [int(adate) for adate in confObj.assimdates]
        intdatefin = int(self.options.datefin.strftime("%Y%m%d%H"))
        intdates.sort()
        bisect.insort(intdates, intdatefin)
        intdates = np.array(intdates)
        intdates = intdates[intdates <= intdatefin]
        stopdates = ",".join(map(str, intdates))
        print('stopdates', stopdates)
        conffile.set_field("DEFAULT", 'stopdates', stopdates)

        # check if members ids were specified
        # if so, do nothing (later in the script, will be reparted between the nodes)
        # else, draw it.
        allmembers = range(1, self.options.nmembers + 1)
        conffile.set_field("DEFAULT", 'members', 'rangex(start:1 end:' + str(self.options.nmembers) + ')')
        if 'E1' in self.options.escroc:
            if hasattr(confObj, 'membersId'):
                membersId = confObj.membersId
            else:
                escroc = ESCROC_subensembles(self.options.escroc, allmembers, randomDraw=True)
                membersId = escroc.members
        else:
            escroc = ESCROC_subensembles(self.options.escroc, allmembers)
            membersId = escroc.members

        # ######################################################################
        conffile.set_field("DEFAULT", 'allids', ','.join(map(str, membersId)))
        conffile.set_field("DEFAULT", 'subensemble', self.options.escroc)
        if self.options.threshold:
            conffile.set_field("DEFAULT", 'threshold', self.options.threshold)
        if not self.options.cramponmonthly:  # for now on CRAMPON only works with yearly forcing files
            conffile.set_field("DEFAULT", 'duration', 'yearly')
        else:
            conffile.set_field("DEFAULT", 'duration', 'monthly')

        conffile.set_field("DEFAULT", 'xpid', self.xpid + '@' + os.getlogin())

        if self.options.openloop:
            self.options.op = 'on'
        else:
            self.options.op = 'off'
        conffile.set_field("DEFAULT", 'openloop', self.options.op)

        if self.options.crampon:
            conffile.set_field("DEFAULT", 'sensor', self.options.sensor)
        conffile.set_field("DEFAULT", 'openmp', 1)
        if self.options.namelist:
            conffile.set_field("DEFAULT", 'namelist', self.options.namelist)
        if self.options.exesurfex:
            conffile.set_field("DEFAULT", 'exesurfex', self.options.exesurfex)
        if self.options.writesx:
            conffile.set_field("DEFAULT", 'writesx', self.options.writesx)

        conffile.set_field("DEFAULT", 'threshold', self.options.threshold)
        if self.options.datespinup:
            conffile.set_field("DEFAULT", 'datespinup', self.options.datespinup.strftime("%Y%m%d%H%M"))
        else:
            conffile.set_field("DEFAULT", 'datespinup', self.options.datedeb.strftime("%Y%m%d%H%M"))

        conffile.set_field("DEFAULT", 'nmembers', self.options.nmembers)
        conffile.set_field("DEFAULT", 'nnodes', self.options.nnodes)

        # new entry for Loopfamily on offline parallel tasks:
        conffile.set_field("DEFAULT", 'offlinetasks', ','.join(map(str, range(1, self.options.nnodes + 1))))
        
        # this line is mandatory to ensure the use of subjobs:
        # place it in the field offline for parallelization of the offlines LoopFamily only
        conffile.add_block('offline')
        conffile.set_field('offline', 'paralleljobs_kind', 'slurm:ssh')

        if self.options.crampon and self.options.nmembers and self.options.op:

            # soda works with all members at the same time on one node only.
            conffile.add_block('soda')
            conffile.set_field('soda', 'nmembersnode', self.options.nmembers)
            conffile.set_field('soda', 'startmember', 1)
            conffile.set_field('soda', 'ntasks', 1)  # one task only for sure

            # BC 18/04/19 nprocs could be set to 40
            # but I doubt this would save much time and it would be risky too.
            # so far, SODA should work in MPI, but it's risky...
            conffile.set_field('soda', 'nprocs', 1)

        else:
            raise Exception('please specify a conf file and a number of members to run.')

        conffile.close()

    def mkjob_crampon(self):
        """create mkjob command for crampon"""
        jobname = 'crampon'
        reftask = 'crampon_driver'
        nnodes = self.options.nnodes
        return ["../vortex/bin/mkjob.py -j name=" + jobname + " task=" + reftask
                + " profile=rd-belenos-mt jobassistant=cen datebegin=" + self.options.datedeb.strftime("%Y%m%d%H%M")
                + " dateend=" + self.options.datefin.strftime("%Y%m%d%H%M") + " template=" + self.jobtemplate
                + " time=" + self.walltime() + " nnodes=" + str(nnodes)]

    def run(self):
        """run job"""
        mkjob_list = self.mkjob_crampon()
        for mkjob in mkjob_list:
            print("Run command: " + mkjob + "\n")
            os.system(mkjob)
