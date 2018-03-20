#! /usr/bin/env python
# -*- coding: utf-8 -*-

'''
Created on 23 févr. 2018

@author: lafaysse
'''

import os, sys
import datetime

from utils.resources import InstallException
from utils.dates import WallTimeException


class vortex_kitchen(object):
    '''
    Interface between s2m command line and vortex utilities (tasks and mk_jobs)
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

        self.create_env()
        self.create_conf(options)

    def check_vortex_install(self):

        if "VORTEX" not in os.environ.keys():
            raise InstallException("VORTEX environment variable must be defined towards a valid vortex install.")

    def create_env(self):
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
        confname = "../conf/" + self.vapp + "_" + self.vconf + ".ini"
        conffile = vortex_conf_file(confname, 'w')

        if options.model == 'safran' and options.forcing == 'reanalyse':
            forcinglogin = 'lafaysse'
        else:
            forcinglogin = os.getlogin()

        conffile.new_class("DEFAULT")
        conffile.write_field('meteo', options.model)
        conffile.write_field('geometry', self.vconf)
        conffile.write_field('forcingid', options.forcing + '@' + forcinglogin)

        if options.escroc:
            conffile.write_field('subensemble', options.escroc)
            conffile.write_field('duration', 'full')
        else:
            conffile.write_field('duration', 'yearly')
        conffile.write_field('xpid', self.xpid + '@' + os.getlogin())
        conffile.write_field('ntasks', 40 )
        conffile.write_field('nprocs', 40 )

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

        # ESCROC on several nodes
        if options.escroc and options.nnodes > 1 and options.nmembers:
            nmembers_per_node = options.nmembers / options.nnodes + 1
            startmember = options.startmember if options.startmember else 1
            for node in range(1, options.nnodes + 1):
                nmembers_this_node = min(nmembers_per_node, options.nmembers - startmember + 1)
                conffile.write_field('nnodes', 1)
                conffile.new_class('escrocN' + str(node))
                conffile.write_field('startmember', startmember)
                conffile.write_field('nmembers', nmembers_this_node)
                startmember += nmembers_this_node

        else:
            conffile.write_field('nnodes', options.nnodes)
            if options.nmembers:
                conffile.write_field('nmembers', options.nmembers)
            if options.startmember:
                conffile.write_field('startmember', options.startmember)

        conffile.close()

    def mkjob_command(self, options, jobname=None):
        if options.escroc:
            jobname = jobname if jobname else 'escroc'
            reftask = "escroc_tasks"
            nnodes = 1
        else:
            jobname = 'rea_s2m'
            reftask = "vortex_tasks"
            nnodes = options.nnodes
        return "../vortex/bin/mkjob.py -j name=" + jobname + " task=" + reftask + " profile=rd-beaufix-mt jobassistant=cen datebegin="\
            + options.datedeb.strftime("%Y%m%d%H%M") + " dateend=" + options.datefin.strftime("%Y%m%d%H%M") + " template=" + self.jobtemplate\
            + " time=" + self.walltime(options)\
            + " nnodes=" + str(nnodes)

    def mkjob_list_commands(self, options):
        if options.escroc and options.nnodes > 1:
            mkjob_list = []
            print "loop"
            for node in range(1, options.nnodes + 1):
                mkjob_list.append(self.mkjob_command(options, jobname='escrocN' + str(node)))

            return mkjob_list
        else:
            return [self.mkjob_command(options)]

    def run(self, options):

        mkjob_list = self.mkjob_list_commands(options)

        for mkjob in mkjob_list:
            print "Run command: " + mkjob + "\n"
            os.system(mkjob)

    def walltime(self, options):

        if options.escroc:
            if options.nmembers:
                nmembers = options.nmembers
            elif options.escroc == "E2":
                nmembers = 35
        else:
            nmembers = 1

        minutes_peryear = dict(alp_allslopes = 15, pyr_allslopes = 15, alp_flat = 2, pyr_flat = 2, cor_allslopes = 2, cor_flat = 1, postes = 2)

        for site_snowmip in ["cdp", "oas", "obs", "ojp", "rme", "sap", "snb", "sod", "swa", "wfj"]:
            minutes_peryear[site_snowmip] = 2

        key = options.region if options.region in minutes_peryear.keys() else "alp_allslopes"

        estimation = datetime.timedelta(minutes=minutes_peryear[key]) * max(1, (options.datefin.year - options.datedeb.year)) * (1 + nmembers / (40 * options.nnodes) )

        if estimation >= datetime.timedelta(hours=24):
            raise WallTimeException(estimation)
        else:
            return str(estimation)


class vortex_conf_file(file):

    def new_class(self, name):
        self.write("[" + name + "]\n")

    def write_field(self, fieldname, value):
        self.write(fieldname + " = " + str(value) + "\n")
