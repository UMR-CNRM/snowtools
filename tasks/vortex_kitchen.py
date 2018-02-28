#! /usr/bin/env python
# -*- coding: utf-8 -*-

'''
Created on 23 févr. 2018

@author: lafaysse
'''

import os
import datetime

from utils.resources import InstallException


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
        conffile.write_field('duration', 'yearly')
        conffile.write_field('xpid', self.xpid + '@' + os.getlogin())
        conffile.write_field('ntasks', 40)
        conffile.write_field('nprocs', 40)
        conffile.write_field('nnodes', 1)
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
        conffile.close()

    def mkjob_command(self, options):
        return "../vortex/bin/mkjob.py -j name=rea_s2m task=vortex_tasks profile=rd-beaufix-mt jobassistant=cen datebegin="\
            + options.datedeb.strftime("%Y%m%d%H%M") + " dateend=" + options.datefin.strftime("%Y%m%d%H%M") + " template=" + self.jobtemplate\
            + " time=" + self.walltime(options)

    def run(self, options):
        mkjob = self.mkjob_command(options)
        print "Run command: " + mkjob + "\n"
        os.system(self.mkjob_command(options))

    def walltime(self, options):
        if options.region in ["alp_allslopes", "pyr_allslopes"]:
            minutes_peryear = 15
        else:
            minutes_peryear = 15

        return str(datetime.timedelta(minutes=minutes_peryear) * max(1, (options.datefin.year - options.datedeb.year)))


class vortex_conf_file(file):

    def new_class(self, name):
        self.write("[" + name + "]\n")

    def write_field(self, fieldname, value):
        self.write(fieldname + " = " + str(value) + "\n")
