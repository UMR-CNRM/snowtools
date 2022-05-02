# -*- coding: utf-8 -*-
'''
Created on 21  mars 2019
Run a CrocO assimilation sequence on a multinode
@author: cluzetb
'''
import os
import random
import shutil

import numpy as np

from snowtools.tasks.vortex_kitchen import Vortex_conf_file
from snowtools.tools.update_namelist import update_surfex_namelist_file
from snowtools.tools.read_conf import read_conf
from snowtools.tasks.vortex_kitchen import vortex_kitchen
from snowtools.utils.ESCROCsubensembles import ESCROC_subensembles
from snowtools.DATA import SNOWTOOLS_DIR


class crocO_vortex_kitchen(vortex_kitchen):
    '''
    Interface between s2m command line and vortex utilities (tasks and mk_jobs)
    crocO_multinode
    based on vortex_kitchen from M. Lafaysse
    '''

    def __init__(self, options):
        '''
        Constructor
        '''
        # Check if a vortex installation is defined
        self.check_vortex_install()
        self.options = options

        self.adapt_machine()
        # check the options consistency
        self.check_options()

        # Initialization of vortex variables
        self.vapp = "s2m"
        self.vconf = self.options.region

        self.xpid = self.options.diroutput + '@' + os.getlogin()
        self.workingdir = self.options.workdir + "/" + self.xpid + "/" + self.vapp + "/" + self.vconf
        self.jobtemplate = "job-vortex-default.py"

        self.options.confcomplement = self.xpid + self.options.datedeb.strftime("%Y")

        self.create_env()
        self.prepare_namelist()  # this must be moved to the preprocess step of offline I think.
        self.create_conf()

    def check_options(self):
        if self.options.openloop:
            if (self.options.synth is not None) or (self.options.real is True):
                print('''
                --openloop, --synth <mbid> and --real are exclusive arguments.
                so please chose one of them only.
                      '''
                      )
                raise Exception
        elif (self.options.synth is None and self.options.real is False):
            raise Exception('''
            Either you\'re running an assimilation experiment with (1) real data or (2) with synthetic from previous openloop
            (1) specify --real to s2m
            (2) use --synth <mbid> (starting from 1)
                to specify which member is the synthetic one in order to remove and replace it.
                 ''')
        if self.options.nnodes * self.options.ntasks > self.options.nmembers:
            print(' be careful, you are trying to run ' + str(self.options.nmembers) + ' members on ' +
                  str(self.options.nnodes * self.options.ntasks) + ' cores (40 cores per nodes)' +
                  ' please reduce --nnodes so that n_cores<=nmembers' )

    def create_env(self):
        # Prepare environment
        if not os.path.isdir(self.workingdir):
            os.makedirs(self.workingdir)
        os.chdir(self.workingdir)

        if not os.path.islink("vortex"):
            os.symlink(os.environ["VORTEX"], "vortex")
        if not os.path.islink("tasks"):
            os.symlink(SNOWTOOLS_DIR + "/tasks", "tasks")
        if 'CrocO_toolbox' in os.environ['PYTHONPATH']:
            from consts import CROCO
            if not os.path.exists('CrocO_tools'):
                os.symlink(CROCO, "CrocO_tools")
        for directory in ["conf", "jobs"]:
            if not os.path.isdir(directory):
                os.mkdir(directory)

        os.chdir("jobs")
        if not os.path.isfile(self.jobtemplate):
            os.symlink(os.environ["SNOWTOOLS_CEN"] + "/jobs/" + self.jobtemplate, self.jobtemplate)

    def prepare_namelist(self):
        self.namelist = self.workingdir + '/OPTIONS_' + self.options.confcomplement + '.nam'
        shutil.copyfile(self.options.namelist, self.namelist)

        self.enforce_nmembers()

    def enforce_nmembers(self):
        """enforce NENS in the namelist to the presecribed s2m argument value. Mandatory for SODA"""
        # options.namelist is already an absolute path.
        if self.options.nmembers is None:
            raise Exception('please specify the number of members for this run')
        update_surfex_namelist_file(self.options.datedeb, namelistfile=self.namelist, dateend=self.options.datefin, updateloc=False, nmembers = self.options.nmembers)

    def replace_member(self, allmembers, members_id):
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
        # workaround to know the size of the ensemble
        sizeE1 = ESCROC_subensembles(self.options.escroc, allmembers, randomDraw = True).size
        # draw a member, excluding any ESCROC member already present in the ensemble.
        members_id[self.options.synth - 1] = np.random.choice([e for e in range(1, sizeE1 + 1) if e not in members_id])
        return members_id

    def draw_meteo(self, confObj):
        meteo_members = {str(m): ((m - 1) % int(self.options.nforcing)) + 1 for m in range(self.options.nmembers)}
        if hasattr(confObj, 'meteo_draw'):
            meteo_draw = confObj.meteo_draw
        else:
            meteo_draw = meteo_members[str(self.options.synth)]
        while meteo_draw == meteo_members[str(self.options.synth)]:
            meteo_draw = random.choice(list(range(1, int(self.options.nforcing) + 1)))
        print('mto draw', meteo_draw)
        return meteo_draw

    def create_conf(self):
        ''' Prepare configuration file from s2m options'''
        # append naming specificities to prevent concurrent tasks from erasing files.
        confname = "../conf/" + self.vapp + "_" + self.vconf + "_" + self.options.confcomplement + ".ini"

        if os.path.exists(confname):
            print('remove vortex conf_file')
            os.remove(confname)

        print('copy conf file to $WORKDIR')
        shutil.copyfile(self.options.croco, confname)

        conffile = Vortex_conf_file(self.options, confname, 'a')

        conffile.set_field('DEFAULT', 'meteo', self.options.model)
        conffile.set_field('DEFAULT', 'geometry', self.vconf)
        conffile.set_field('DEFAULT', 'forcing', self.options.forcing)
        conffile.set_field('DEFAULT', 'nforcing', self.options.nforcing)
        conffile.set_field('DEFAULT', 'datedeb', self.options.datedeb)
        conffile.set_field('DEFAULT', 'datefin', self.options.datefin)
        conffile.set_field('DEFAULT', 'confcomplement', self.options.confcomplement)  # put confcomplement un the conf itself for hendrix fetching -_-

        # ########### READ THE USER-PROVIDED conf file ##########################
        # -> in order to append datefin to assimdates and remove the exceding dates.
        # -> in order to check if members_ids were specified.

        # local import since there are dependencies with vortex.
        import bisect

        confObj = read_conf(confname)
        intdates = list(map(int, confObj.assimdates))
        intdatefin = int(self.options.datefin.strftime("%Y%m%d%H"))
        intdates.sort()
        bisect.insort(intdates, intdatefin)
        intdates = np.array(intdates)
        intdates = intdates[intdates <= intdatefin]
        print('stopdates', intdates)
        intdates = intdates.tolist()
        # BC june 2020 bug in driver when stopdates has only one item
        if len(intdates) == 1:
            conffile.set_field('DEFAULT', 'stopdates', 'list(' + str(intdates[0]) + ')')
        else:
            conffile.set_field('DEFAULT', 'stopdates', intdates)

        # check if members ids were specified
        # if so, do nothing (later in the script, will be reparted between the nodes)
        # else, draw it.
        allmembers = list(range(1, self.options.nmembers + 1))

        # BC 01/04/20: this rangeX will cause us some trouble...
        conffile.set_field('DEFAULT', 'members', 'rangex(start:1 end:' + str(self.options.nmembers) + ')')
        if 'E1' in self.options.escroc:
            if hasattr(confObj, 'members_id'):
                members_id = np.array(list(map(int, confObj.members_id)))

                # in case of synthetic assimilation, need to:
                #    - replace the synthetic escroc member
                #    - draw a substitution forcing
                if self.options.synth is not None:

                    # strange case when the synth member comes from a larger openloop (ex 160) than the current experiment (ex 40)
                    if self.options.synth > self.options.nmembers:
                        conffile.set_field('DEFAULT', 'synth', self.options.synth)
                    else:
                        # replace ESCROC member
                        members_id = self.replace_member(allmembers, members_id)
                        conffile.set_field('DEFAULT', 'synth', self.options.synth)

                        # draw a substitution forcing
                        meteo_draw = self.draw_meteo(confObj)
                        conffile.set_field('DEFAULT', 'meteo_draw', meteo_draw)

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
            else:
                if self.options.synth is not None:
                    # here, no prescribed members: only need to substitute meteo.
                    escroc = ESCROC_subensembles(self.options.escroc, allmembers, randomDraw = True)
                    members_id = escroc.members

                    # draw a substitution forcing
                    meteo_draw = self.draw_meteo(confObj)
                    conffile.set_field('DEFAULT', 'meteo_draw', meteo_draw)
                else:
                    escroc = ESCROC_subensembles(self.options.escroc, allmembers, randomDraw = True)
                    members_id = escroc.members
                conffile.set_field('DEFAULT', 'members_id', members_id)
        else:
            escroc = ESCROC_subensembles(self.options.escroc, allmembers)
            members_id = escroc.members
            conffile.set_field('DEFAULT', 'members_id', members_id)

        conffile.set_field('DEFAULT', 'subensemble', self.options.escroc)
        if self.options.threshold:
            conffile.set_field('DEFAULT', 'threshold', self.options.threshold)
        # for now on CrocO only works with yearly forcing files
        conffile.set_field('DEFAULT', 'duration', 'yearly')

        conffile.set_field('DEFAULT', 'xpid', self.xpid)
        conffile.set_field('DEFAULT', 'workingdir', self.workingdir)

        if self.options.openloop:
            self.options.op = 'on'
        else:
            self.options.op = 'off'
        conffile.set_field('DEFAULT', 'openloop', self.options.op)
        if self.options.pickleit:
            self.options.pickl = 'on'
        else:
            self.options.pickl = 'off'
        conffile.set_field('DEFAULT', 'pickleit', self.options.pickl)

        if self.options.croco:
            if hasattr(self.options, 'sensor'):
                conffile.set_field('DEFAULT', 'sensor', self.options.sensor)
        conffile.set_field('DEFAULT', 'openmp', 1)
        if self.options.namelist:
            conffile.set_field('DEFAULT', 'namelist', self.namelist)
        if self.options.exesurfex:
            conffile.set_field('DEFAULT', 'exesurfex', self.options.exesurfex)
        if self.options.writesx:
            self.options.sx = 'on'
        else:
            self.options.sx = 'off'
        conffile.set_field('DEFAULT', 'writesx', self.options.sx)

        if self.options.datespinup:
            conffile.set_field('DEFAULT', 'datespinup', self.options.datespinup.strftime("%Y%m%d%H%M"))
        else:
            conffile.set_field('DEFAULT', 'datespinup', self.options.datedeb.strftime("%Y%m%d%H%M"))

        conffile.set_field('DEFAULT', 'nmembers', self.options.nmembers)
        conffile.set_field('DEFAULT', 'nnodes', self.options.nnodes)

        # new entry for Loopfamily on offline parallel tasks:
        conffile.set_field('DEFAULT', 'offlinetasks', list(range(1, self.options.nnodes + 1)))

        # this line is mandatory to ensure the use of subjobs:
        # place it in the field offline for parallelization of the offlines LoopFamily only
        conffile.set_field('offline', 'paralleljobs_kind', 'slurm:ssh')

        if self.options.croco and self.options.nmembers and self.options.op:

            # soda works with all members at the same time on one node only.
            conffile.set_field('soda', 'nmembersnode', self.options.nmembers)
            conffile.set_field('soda', 'startmember', 1)
            conffile.set_field('soda', 'ntasks', 1)  # one task only for sure

            # BC 18/04/19 nprocs could be set to 40
            # but I doubt this would save much time and it would be risky too.
            # so far, SODA should work in MPI, but it's risky...
            conffile.set_field('soda', 'nprocs', 1)

        else:
            raise Exception('please specify a conf file and a number of members to run.')
        conffile.write_file()
        conffile.close()

    def mkjob_crocO(self):
        jobname = 'croco'
        reftask = 'crocO_driver'
        nnodes = self.options.nnodes
        # BC add in order to use the generic job template
        self.period = " rundate=" + self.options.datedeb.strftime("%Y%m%d")

        return ["python ../vortex/bin/mkjob.py -j name=" + jobname + " task=" + reftask + " profile=" + self.profile + " jobassistant=cen datebegin=" +
                self.options.datedeb.strftime("%Y%m%d%H%M") + " dateend=" + self.options.datefin.strftime("%Y%m%d%H%M") +
                # " template=" + self.jobtemplate +
                " time=" + self.walltime() +
                self.period +
                " nnodes=" + str(nnodes) + " taskconf=" + self.options.confcomplement]

    def run(self):
        mkjob_list = self.mkjob_crocO()
        for mkjob in mkjob_list:
            print("Run command: " + mkjob + "\n")
            os.system(mkjob)
