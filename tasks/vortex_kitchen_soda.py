#! /usr/bin/env python
# -*- coding: utf-8 -*-
'''
Created on 14 juin 2018

@author: cluzetb
'''
import os, sys
import datetime
import shutil

from utils.resources import InstallException
from utils.dates import WallTimeException
from tasks.vortex_kitchen import vortex_conf_file, walltime


class vortex_kitchen_soda(object):
    '''
    Interface between s2m command line and vortex utilities (tasks and mk_jobs)
    SODA case
    based on vortex_soda from M. Lafaysse
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

        self.workingdir = options.dirwork + "/" + self.vapp + "/" + self.vconf

        self.xpid = options.diroutput
        self.jobtemplate = "job-vortex-default.py"

        self.create_env(options)
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

    def create_conf(self, options):
        ''' Prepare configuration file from s2m options'''

        confname = "../conf/" + self.vapp + "_" + self.vconf + ".ini"  # this file already exists if options.soda

        if os.path.exists(confname):
            print ('remove vortex conf_file')
            os.remove(confname)

        print ('copy conf file to vortex path')
        shutil.copyfile(options.soda, confname)

        conffile = vortex_conf_file(confname, 'a')

        conffile.write_field('meteo', options.model)
        conffile.write_field('geometry', self.vconf)
        conffile.write_field('nforcing', options.nforcing)

        if not options.sodamonthly:  # for now on soda works only with yearly forcing files
            conffile.write_field('subensemble', options.escroc)
            conffile.write_field('duration', 'yearly')
        else:
            conffile.write_field('subensemble', options.escroc)
            conffile.write_field('duration', 'monthly')

        conffile.write_field('xpid', self.xpid + '@' + os.getlogin())
        conffile.write_field('ntasks', 40 )
        conffile.write_field('nprocs', 40 )

        if options.openloop:
            conffile.write_field('openloop', 'on')
        else:
            conffile.write_field('openloop', 'off')
        conffile.write_field('openmp', 1)
        if options.namelist:
            conffile.write_field('namelist', options.namelist)
        if options.exesurfex:
            conffile.write_field('exesurfex', options.exesurfex)
        conffile.write_field('threshold', options.threshold)
        if options.datespinup:
            conffile.write_field('datespinup', options.datespinup.strftime("%Y%m%d%H%M"))
        else:
            conffile.write_field('datespinup', options.datedeb.strftime("%Y%m%d%H%M"))

        """
        if options.escroc and options.nnodes > 1 and options.nmembers:
            nmembers_per_node = options.nmembers / options.nnodes + 1
            startmember = options.startmember if options.startmember else 1
            for node in range(1, options.nnodes + 1):
                nmembers_this_node = min(nmembers_per_node, options.nmembers - startmember + 1)
                conffile.new_class('escrocN' + str(node))
                conffile.write_field('nnodes', 1)
                conffile.write_field('startmember', startmember)
                conffile.write_field('nmembers', nmembers_this_node)
                startmember += nmembers_this_node
        """

        conffile.write_field('nmembers', options.nmembers)
        conffile.write_field('nnodes', options.nnodes)
        if options.soda and options.nnodes > 1 and options.nmembers:
            nmembers_per_node = options.nmembers / options.nnodes + 1
            startmember = options.startmember if options.startmember else 1
            for node in range(1, options.nnodes + 1):
                nmembers_this_node = min(nmembers_per_node, options.nmembers - startmember + 1)
                conffile.new_class('sodaN' + str(node))
                conffile.write_field('startmember', startmember)
                conffile.write_field('nmembersnode', nmembers_this_node)
                startmember += nmembers_this_node

        else:
            conffile.write_field('nnodes', options.nnodes)
            if options.nmembers:
                conffile.write_field('nmembersnode', options.nmembers)
            if options.startmember:
                conffile.write_field('startmember', options.startmember)
        
        if options.writesx:
            conffile.write_field('writesx', options.writesx)
        
        conffile.close()

    def mkjob_soda(self, options):
        jobname = 'escroc_soda'
        reftask = 'soda_snow_tasks'
        nnodes = options.nnodes
        return ["../vortex/bin/mkjob.py -j name=" + jobname + " task=" + reftask + " profile=rd-beaufix-mt jobassistant=cen datebegin=" +
                options.datedeb.strftime("%Y%m%d%H%M") + " dateend=" + options.datefin.strftime("%Y%m%d%H%M") + " template=" + self.jobtemplate +
                " time=" + walltime(options) +
                " nnodes=" + str(nnodes)]

    def run(self, options):
        mkjob_list = self.mkjob_soda(options)
        for mkjob in mkjob_list:
            print ("Run command: " + mkjob + "\n")
            os.system(mkjob)

