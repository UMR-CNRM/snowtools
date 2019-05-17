#! /usr/bin/env python
# -*- coding: utf-8 -*-

'''
Created on 23 févr. 2018

@author: lafaysse
'''

import os
import datetime
import shutil

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
        self.options = options

        # Initialization of vortex variables
        if self.options.safran:
            self.options.vapp = "safran"
        else:
            self.options.vapp = "s2m"
        self.options.vconf = self.options.region
        if hasattr(self.options, 'diroutput'):
            self.options.xpid = self.options.diroutput
        self.workingdir = "/".join([self.options.workdir, self.options.xpid, self.options.vapp, self.options.vconf])
        self.confdir    = "/".join([self.workingdir, 'conf'])
        self.jobdir     = "/".join([self.workingdir, "jobs"])

        if self.options.oper:
            self.jobtemplate = "job-s2m-oper.py"
            self.profile = "rd-prolix-mt"
        else:
            self.jobtemplate = "job-vortex-default.py"
            self.profile = "rd-beaufix-mt"

        self.execute()

    def execute(self):

        self.create_env()
        self.set_conf_file()
        self.run()

    def check_vortex_install(self):

        if "VORTEX" not in list(os.environ.keys()):
            raise InstallException("VORTEX environment variable must be defined towards a valid vortex install.")

    def create_env(self):
        # Prepare environment
        if not os.path.isdir(self.workingdir):
            os.makedirs(self.workingdir)
        os.chdir(self.workingdir)

        if not os.path.islink("vortex"):
            os.symlink(os.environ["VORTEX"], "vortex")

        if not os.path.islink("snowtools"):
            os.symlink(os.environ["SNOWTOOLS_CEN"], "snowtools")

        if not os.path.islink("tasks"):
            if self.options.oper:
                os.symlink(os.environ["SNOWTOOLS_CEN"] + "/tasks/oper", "tasks")
            else:
                os.symlink(os.environ["SNOWTOOLS_CEN"] + "/tasks", "tasks")

        for directory in ["conf", "jobs"]:
            if not os.path.isdir(directory):
                os.mkdir(directory)

        os.chdir("jobs")
        if not os.path.isfile(self.jobtemplate):
            os.symlink(os.environ["SNOWTOOLS_CEN"] + "/jobs/" + self.jobtemplate, self.jobtemplate)
        os.chdir(self.workingdir)

    def set_conf_file(self):

        os.chdir(self.confdir)
        if self.options.surfex:
            if self.options.oper:
                conffilename = self.options.vapp + "_" + self.options.vconf + ".ini"
                if not os.path.islink(conffilename):
                    # Operational case : the configuration files are provided : only create a symbolic link in the appropriate directory
                    if os.path.exists("../snowtools/DATA/OPER/" + conffilename):
                        os.symlink("../snowtools/DATA/OPER/" + conffilename, conffilename)
                    elif os.path.exists("../snowtools/conf/" + conffilename):
                        os.symlink("../snowtools/conf/" + conffilename, conffilename)
                    else:
                        print("WARNING : No conf file found in snowtools")
            else:
                if hasattr(self.options, 'confname'):
                    conffilename = self.options.confname.rstrip('.ini') + ".ini"
                else:
                    conffilename = self.options.vapp + "_" + self.options.vconf + "_" + self.options.xpid + self.options.datedeb.strftime("%Y") + ".ini"

            if hasattr(self.options, 'soda'):

                if os.path.exists(conffilename):
                    print ('remove vortex conf_file')
                    os.remove(conffilename)

                if self.options.soda:
                    print ('copy conf file to vortex path')
                    shutil.copyfile(self.options.soda, conffilename)

        elif self.options.safran:
            if self.options.oper:
                pass
            else:
                conffilename = self.options.vapp + "_" + self.options.vconf + ".ini"

        fullname = '/'.join([self.confdir, conffilename])

        self.conf_file = Vortex_conf_file(self.options, fullname)
        self.conf_file.create_conf()
        os.chdir(self.workingdir)

    def mkjob_command(self, jobname=None):

        if self.options.oper:
            self.reftask = "ensemble_surfex_tasks"
            nnodes = 1
            period = "rundate=" + self.options.datedeb.strftime("%Y%m%d%H%M")
            # Note that the jobname is used to discriminate self.conf.previ in vortex task
            if self.options.reinit:
                self.jobname = "surfex_reinit"
            elif self.options.forecast:
                self.jobname = "surfex_forecast"
            else:
                self.jobname = "surfex_analysis"
        else:
            period = "datebegin=" + self.options.datedeb.strftime("%Y%m%d%H%M") + " dateend=" + self.options.datefin.strftime("%Y%m%d%H%M")
            if self.options.escroc:
                self.jobname = jobname if jobname else 'escroc'
                if self.options.scores:
                    # self.reftask  = "scores_task"
                    # self.reftask = "optim_task"
                    self.reftask = "crps_task"
                else:
                    self.reftask = "escroc_tasks"
                nnodes = 1
            elif self.options.forecast:
                self.jobname = "surfex_forecast"
                self.reftask = "ensemble_surfex_reforecast"
                nnodes = 1
            else:
                self.jobname = 'rea_s2m'
                self.reftask = "vortex_tasks"
                nnodes = self.options.nnodes

        return "../vortex/bin/mkjob.py -j name=" + self.jobname + " task=" + self.reftask + " profile=" + self.profile + " jobassistant=cen " + period +\
               " template=" + self.jobtemplate + " time=" + self.walltime() + " nnodes=" + str(nnodes) +\
               " taskconf=" + self.options.xpid + self.options.datedeb.strftime("%Y")

    def mkjob_list_commands(self):

        if self.options.escroc and self.options.nnodes > 1:
            mkjob_list = []
            print("loop")
            for node in range(1, self.options.nnodes + 1):
                mkjob_list.append(self.mkjob_command(jobname='escrocN' + str(node)))

            return mkjob_list
        else:
            return [self.mkjob_command()]

    def mkjob_safran(self):

        if self.options.oper:
            pass
        else:
            self.jobname = "ana_saf"
            self.reftask = "safran_analysis"

        print("../vortex/bin/mkjob.py -j name=" + self.jobname + " task=" + self.reftask + " profile=" + self.profile + " jobassistant=cen " +
              " template=" + self.jobtemplate + " time=" + self.walltime() + " datebegin=" + self.options.datedeb.strftime("%Y%m%d") +
              " dateend=" + self.options.datefin.strftime("%Y%m%d"))

        return "../vortex/bin/mkjob.py -j name=" + self.jobname + " task=" + self.reftask + " profile=" + self.profile + " jobassistant=cen " + \
               " template=" + self.jobtemplate + " time=" + self.walltime() + " datebegin=" + self.options.datedeb.strftime("%Y%m%d") + \
               " dateend=" + self.options.datefin.strftime("%Y%m%d")

    def run(self):

        if self.options.surfex:
            mkjob_list = self.mkjob_list_commands()
        else:
            mkjob_list = [self.mkjob_safran(), ]

        self.conf_file.add_block(self.jobname)
        self.conf_file.add_block(self.reftask)
        self.conf_file.write_file()
        self.conf_file.close()

        os.chdir(self.jobdir)
        for mkjob in mkjob_list:
            print("Run command: " + mkjob + "\n")
            os.system(mkjob)

    def walltime(self):

        if self.options.walltime:
            return self.options.walltime

        elif self.options.oper:
            return str(datetime.timedelta(minutes=10))

        else:
            if self.options.escroc:
                if self.options.nmembers:
                    nmembers = self.options.nmembers
                elif self.options.escroc == "E2":
                    nmembers = 35
                else:
                    raise Exception("don't forget to specify escroc ensemble or --nmembers")

            else:
                nmembers = 1
            # minutes per year for one member computing all points
            minutes_peryear = dict(alp_allslopes = 15, pyr_allslopes = 15, alp_flat = 2, pyr_flat = 2, cor_allslopes = 2, cor_flat = 1, postes = 2,
                                   lautaret = 120, lautaretreduc = 5)

            for site_snowmip in ["cdp", "oas", "obs", "ojp", "rme", "sap", "snb", "sod", "swa", "wfj"]:
                if self.options.scores:
                    minutes_peryear[site_snowmip] = 0.2
                else:
                    minutes_peryear[site_snowmip] = 4

            for massif_safran in range(1, 100):
                minutes_peryear[str(massif_safran)] = 90

            key = self.options.region if self.options.region in list(minutes_peryear.keys()) else "alp_allslopes"

            estimation = datetime.timedelta(minutes=minutes_peryear[key]) * max(1, (self.options.datefin.year - self.options.datedeb.year)) * (1 + nmembers / (40 * self.options.nnodes) )

            if estimation >= datetime.timedelta(hours=24):
                raise WallTimeException(estimation)
            else:
                return str(estimation)


class Vortex_conf_file(object):
    # NB: Inheriting from file object is not allowed in python 3
    def __init__(self, options, filename, mode='w'):
        self.name = filename
        self.options = options
        self.fileobject = open(filename, mode)
        self.blocks = dict()

    def add_block(self, name):
        self.blocks[name] = dict()

    def set_field(self, blockname, fieldname, value):
        if blockname not in self.blocks.keys():
            self.add_block(blockname)
        # WARNING : we reset the field value if it already existed
        self.blocks[blockname][fieldname] = value

    def write_file(self):
        for blockname in self.blocks:
            self.fileobject.write("[" + blockname + "]\n")
            for fieldname, value in self.blocks[blockname].items():
                self.fileobject.write(fieldname + " = " + str(value) + "\n")
            self.fileobject.write("\n")

    def close(self):
        self.fileobject.close()

    def create_conf(self):
        ''' Prepare configuration file from s2m options'''
        self.default_variables()
        if self.options.surfex:
            self.create_conf_surfex()
        elif self.options.safran:
            self.create_conf_safran()

    def create_conf_surfex(self):
        self.surfex_variables()
        # ESCROC on several nodes
        if self.options.escroc:
            self.escroc_variables()
        else:
            self.set_field("DEFAULT", 'nnodes', self.options.nnodes)
            if self.options.nmembers:
                self.set_field("DEFAULT", 'nmembers', self.options.nmembers)
            if self.options.startmember:
                self.set_field("DEFAULT", 'startmember', self.options.startmember)
        if self.options.soda:
            self.soda_variables(self.options)

    def create_conf_safran(self):
        self.safran_variables()

    def default_variables(self):

        self.set_field("DEFAULT", 'xpid', self.options.xpid + '@' + os.getlogin())
        self.set_field("DEFAULT", 'ntasks', 40 )
        self.set_field("DEFAULT", 'nprocs', 40 )
        self.set_field("DEFAULT", 'openmp', 1)
        self.set_field("DEFAULT", 'geometry', self.options.vconf)

    def escroc_variables(self):

        self.set_field("DEFAULT", 'subensemble', self.options.escroc)
        self.set_field("DEFAULT", 'duration', 'full')

        if self.options.nnodes > 1 and self.options.nmembers:

            nmembers_per_node = self.options.nmembers / self.options.nnodes + 1
            startmember = self.options.startmember if self.options.startmember else 1
            for node in range(1, self.options.nnodes + 1):
                nmembers_this_node = min(nmembers_per_node, self.options.nmembers - startmember + 1)
                self.set_field("DEFAULT", 'nnodes', 1)
                newblock = "escrocN" + str(node)
                self.set_field(newblock, 'startmember', startmember)
                self.set_field(newblock, 'nmembers', nmembers_this_node)
                startmember += nmembers_this_node

    def surfex_variables(self):

        if self.options.model == 'safran':
            forcinglogin = 'vernaym'
        else:
            forcinglogin = os.getlogin()

        self.set_field("DEFAULT", 'meteo', self.options.model)
        self.set_field("DEFAULT", 'forcingid', self.options.forcing + '@' + forcinglogin)
        self.set_field("DEFAULT", 'duration', 'yearly')

        if self.options.namelist:
            self.set_field("DEFAULT", 'namelist', self.options.namelist)
        if self.options.exesurfex:
            self.set_field("DEFAULT", 'exesurfex', self.options.exesurfex)
        self.set_field("DEFAULT", 'threshold', self.options.threshold)
        if self.options.datespinup:
            self.set_field("DEFAULT", 'datespinup', self.options.datespinup.strftime("%Y%m%d%H%M"))
        else:
            self.set_field("DEFAULT", 'datespinup', self.options.datedeb.strftime("%Y%m%d%H%M"))

        if self.options.ground:
            self.set_field("DEFAULT", 'climground', self.options.ground)

        if self.options.dailyprep:
            self.set_field("DEFAULT", 'dailyprep', self.options.dailyprep)

    def safran_variables(self):

        self.set_field("DEFAULT", 'cumul', 6)
        self.set_field("DEFAULT", 'cutoff', self.options.cutoff)
        self.set_field("DEFAULT", 'model', 'safran')
        self.set_field("DEFAULT", 'cycle', 'uenv:s2m.01@vernaym')
        self.set_field("DEFAULT", 'namespace', 'vortex.multi.fr')
        if self.options.namelist:
            self.set_field("DEFAULT", 'namelist', self.options.namelist)
        if self.options.guess:
            self.set_field("DEFAULT", 'guess_path', self.options.guess)
        else:
            self.set_field("DEFAULT", 'guess_block', 'guess')
        if self.options.executables:
            self.set_field("DEFAULT", 'executables', self.options.executables)
        if self.options.savedir:
            self.set_field("DEFAULT", 'savedir', self.options.savedir)
        if self.options.cpl_model:
            self.set_field("DEFAULT", 'source_app', self.options.cpl_model[0])
            self.set_field("DEFAULT", 'source_conf', self.options.cpl_model[1])
