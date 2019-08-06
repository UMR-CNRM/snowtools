#! /usr/bin/env python
# -*- coding: utf-8 -*-
'''
Created on 21  mars 2019
Run a CRAMPON assimilation sequence on a multinode
@author: cluzetb
'''
import os
import shutil

from utils.resources import InstallException
from tasks.vortex_kitchen import vortex_conf_file, walltime
from tools.update_namelist import update_surfex_namelist_file
import numpy as np
from utils.ESCROCsubensembles import ESCROC_subensembles
import time
import random


class crampon_vortex_kitchen(object):
    '''
    Interface between s2m command line and vortex utilities (tasks and mk_jobs)
    crampon_multinode
    based on vortex_kitchen from M. Lafaysse
    '''

    def __init__(self, options):
        '''
        Constructor
        '''
        # Check if a vortex installation is defined
        self.check_vortex_install()

        # Initialization of vortex variables
        self.vapp = "s2m"
        self.vconf = options.region

        self.xpid = options.diroutput + '@' + os.getlogin()
        self.workingdir = options.dirwork + "/" + self.xpid + "/" + self.vapp + "/" + self.vconf
        self.jobtemplate = "job-vortex-default.py"

        options.confcomplement = self.xpid + options.datedeb.strftime("%Y")

        self.create_env(options)
        self.prepare_namelist(options)  # this must be moved to the preprocess step of offline I think.
        self.create_conf(options)

    def check_vortex_install(self):

        if "VORTEX" not in os.environ.keys():
            raise InstallException("VORTEX environment variable must be defined towards a valid vortex install.")

    def create_env(self, options):
        # Prepare environment
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
        if not os.path.isfile(self.jobtemplate):
            os.symlink(os.environ["SNOWTOOLS_CEN"] + "/jobs/" + self.jobtemplate, self.jobtemplate)

    def prepare_namelist(self, options):
        self.namelist = self.workingdir + '/OPTIONS_' + options.confcomplement + '.nam'
        shutil.copyfile(options.namelist, self.namelist)

        self.enforce_nmembers(options)

    def enforce_nmembers(self, options):
        """enforce NENS in the namelist to the presecribed s2m argument value. Mandatory for SODA"""
        # options.namelist is already an absolute path.
        update_surfex_namelist_file(options.datedeb, namelistfile=self.namelist, dateend=options.datefin, updateloc=False, nmembers = options.nmembers)

    def create_conf(self, options):
        ''' Prepare configuration file from s2m options'''
        # append naming specificities to prevent concurrent tasks from erasing files.
        confname = "../conf/" + self.vapp + "_" + self.vconf + "_" + options.confcomplement + ".ini"

        if os.path.exists(confname):
            print ('remove vortex conf_file')
            os.remove(confname)

        print ('copy conf file to $WORKDIR')
        shutil.copyfile(options.crampon, confname)

        conffile = vortex_conf_file(confname, 'a')

        conffile.write_field('meteo', options.model)
        conffile.write_field('geometry', self.vconf)
        conffile.write_field('forcing', options.forcing)
        conffile.write_field('nforcing', options.nforcing)
        conffile.write_field('datedeb', options.datedeb)
        conffile.write_field('datefin', options.datefin)
        conffile.write_field('confcomplement', options.confcomplement)  # put confcomplement un the conf itself for hendrix fetching -_-

        # ########### READ THE USER-PROVIDED conf file ##########################
        # -> in order to append datefin to assimdates and remove the exceding dates.
        # -> in order to check if membersIDs were specified.

        # local import since there are dependencies with vortex.
        from assim.utilcrocO import read_conf
        import bisect

        confObj = read_conf(confname)
        intdates = map(int, confObj.assimdates)
        intdatefin = int(options.datefin.strftime("%Y%m%d%H"))
        intdates.sort()
        bisect.insort(intdates, intdatefin)
        intdates = np.array(intdates)
        intdates = intdates[intdates <= intdatefin]
        stopdates = ",".join(map(str, intdates))
        print('stopdates', stopdates)
        conffile.write_field('stopdates', stopdates)

        # check if members ids were specified
        # if so, do nothing (later in the script, will be reparted between the nodes)
        # else, draw it.
        allmembers = range(1, options.nmembers + 1)
        conffile.write_field('members', 'rangex(start:1 end:' + str(options.nmembers) + ')')
        if 'E1' in options.escroc:
            if hasattr(confObj, 'membersId'):
                membersId = np.array(map(int, confObj.membersId))

                # in case of synthetic assimilation, need to:
                #    - replace the synthetic escroc member
                #    - draw a substitution forcing
                if options.synth is not None:
                    # warning in case of misspecification of --synth
                    print('\n\n\n')
                    print(' /!\/!\/!\/!\ CAUTION /!\/!\/!\/!\/!\ ')
                    print('Please check that the --synth argument')
                    print('corresponds to the openloop member    ')
                    print('used to generate the observations     ')
                    print('otherwise this would artificially     ')
                    print('generate excellent results            ')
                    print('by letting the truth member to stay   ')
                    print('in the assimilation experiment        ')
                    print('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
                    print('\n\n\n')
                    time.sleep(2)
                    # workaround to know the size of the ensemble
                    sizeE1 = ESCROC_subensembles(options.escroc, allmembers, randomDraw = True).size
                    # draw a member, excluding any ESCROC member already present in the ensemble.
                    membersId[options.synth - 1] = np.random.choice([e for e in range(1, sizeE1 + 1) if e not in membersId])

                    conffile.write_field('synth', options.synth)

                    # draw a substitution forcing
                    meteo_members = {str(m): ((m - 1) % int(options.nforcing)) + 1 for m in range(options.nmembers)}
                    if hasattr(confObj, 'meteo_draw'):
                        meteo_draw = confObj.meteo_draw
                    else:
                        meteo_draw = meteo_members[str(options.synth)]
                    while meteo_draw == meteo_members[str(options.synth)]:
                        meteo_draw = random.choice(range(1, int(options.nforcing) + 1))
                    print('mto draw', meteo_draw)
                    conffile.write_field('meteo_draw', meteo_draw)
                else:  # real observations assimilation
                    # warning in case of performing a synthetic experiment
                    # but forgetting to specify the synthetic member
                    print('\n\n\n')
                    print(' /!\/!\/!\/!\ CAUTION /!\/!\/!\/!\/!\ ')
                    print("If you're performing a SYNTHETIC EXPERIMENT")
                    print('you MUST specify the synthetic member     ')
                    print('to eliminate from the ensemble')
                    print('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
                    print('\n\n\n')
                    time.sleep(2)
            else:
                escroc = ESCROC_subensembles(options.escroc, allmembers, randomDraw = True)
                membersId = escroc.members
        else:
            escroc = ESCROC_subensembles(options.escroc, allmembers)
            membersId = escroc.members

        # ######################################################################
        conffile.write_field('membersId', ','.join(map(str, membersId)))
        conffile.write_field('subensemble', options.escroc)
        if options.threshold:
            conffile.write_field('threshold', options.threshold)
        if not options.cramponmonthly:  # for now on CRAMPON only works with yearly forcing files
            conffile.write_field('duration', 'yearly')
        else:
            conffile.write_field('duration', 'monthly')

        conffile.write_field('xpid', self.xpid)
        conffile.write_field('workingdir', self.workingdir)

        if options.openloop:
            options.op = 'on'
        else:
            options.op = 'off'
        conffile.write_field('openloop', options.op)

        if options.crampon:
            conffile.write_field('sensor', options.sensor)
        conffile.write_field('openmp', 1)
        if options.namelist:
            conffile.write_field('namelist', self.namelist)
        if options.exesurfex:
            conffile.write_field('exesurfex', options.exesurfex)
        if options.writesx:
            conffile.write_field('writesx', options.writesx)

        conffile.write_field('threshold', options.threshold)
        if options.datespinup:
            conffile.write_field('datespinup', options.datespinup.strftime("%Y%m%d%H%M"))
        else:
            conffile.write_field('datespinup', options.datedeb.strftime("%Y%m%d%H%M"))

        conffile.write_field('nmembers', options.nmembers)
        conffile.write_field('nnodes', options.nnodes)

        # new entry for Loopfamily on offline parallel tasks:
        conffile.write_field('offlinetasks', ','.join(map(str, range(1, options.nnodes + 1))))

        # this line is mandatory to ensure the use of subjobs:
        # place it in the field offline for parallelization of the offlines LoopFamily only
        conffile.new_class('offline')
        conffile.write_field('paralleljobs_kind', 'slurm:ssh')

        if options.crampon and options.nmembers and options.op:

            # soda works with all members at the same time on one node only.
            conffile.new_class('soda')
            conffile.write_field('nmembersnode', options.nmembers)
            conffile.write_field('startmember', 1)
            conffile.write_field('ntasks', 1)  # one task only for sure

            # BC 18/04/19 nprocs could be set to 40
            # but I doubt this would save much time and it would be risky too.
            # so far, SODA should work in MPI, but it's risky...
            conffile.write_field('nprocs', 1)

        else:
            raise Exception('please specify a conf file and a number of members to run.')

        conffile.close()

    def mkjob_crampon(self, options):
        jobname = 'crampon'
        reftask = 'crampon_driver'
        nnodes = options.nnodes

        return ["../vortex/bin/mkjob.py -j name=" + jobname + " task=" + reftask + " profile=rd-beaufix-mt jobassistant=cen datebegin=" +
                options.datedeb.strftime("%Y%m%d%H%M") + " dateend=" + options.datefin.strftime("%Y%m%d%H%M") + " template=" + self.jobtemplate +
                " time=" + walltime(options) +
                " nnodes=" + str(nnodes) + " taskconf=" + options.confcomplement]

    def run(self, options):
        mkjob_list = self.mkjob_crampon(options)
        for mkjob in mkjob_list:
            print ("Run command: " + mkjob + "\n")
            os.system(mkjob)
