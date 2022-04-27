# -*- coding: utf-8 -*-

"""
Created on 23 févr. 2018

@author: lafaysse
"""

import datetime
import os
import sys
import subprocess
import shutil

from snowtools.utils.dates import WallTimeException
from snowtools.utils.resources import InstallException
from snowtools.DATA import SNOWTOOLS_DIR, SNOWTOOLS_CEN
from bronx.stdtypes.date import Period


class vortex_kitchen(object):
    """
    Interface between s2m command line and vortex utilities (tasks and mk_jobs)
    """

    def __init__(self, options):
        """
        Constructor
        """
        # Check if a vortex installation is defined
        self.check_vortex_install()
        self.options = options

        # Initialization of vortex variables
        if self.options.safran:
            self.options.vapp = "safran"
        else:
            self.options.vapp = "s2m"

        self.split_geo()

        if hasattr(self.options, 'diroutput'):
            self.options.xpid = self.options.diroutput
        self.workingdir = "/".join([self.options.workdir, self.options.xpid, self.options.vapp, self.options.vconf])
        self.confdir    = "/".join([self.workingdir, 'conf'])
        self.jobdir     = "/".join([self.workingdir, "jobs"])

        machine = os.uname()[1]
        if 'taranis' in machine:
            self.profile = "rd-taranis-mt"
        elif 'belenos' in machine:
            self.profile = "rd-belenos-mt"
        self.define_ntasks(machine)

        self.execute()

    def define_ntasks(self, machine):
        if not self.options.ntasks:
            if 'taranis' in machine or 'belenos' in machine:
                self.options.ntasks = 80
                # optimum constaté pour la réanalyse Alpes avec léger dépeuplement parmi les 128 coeurs.

    def execute(self):

        self.create_env()
        self.init_job_task()
        self.set_conf_file()
        self.run()

    def check_vortex_install(self):

        if "VORTEX" not in list(os.environ.keys()):
            raise InstallException("VORTEX environment variable must be defined towards a valid vortex install.")

    def create_env(self):
        """Prepare environment"""
        if not os.path.isdir(self.workingdir):
            os.makedirs(self.workingdir)
        os.chdir(self.workingdir)

        if not os.path.islink("vortex"):
            os.symlink(os.environ["VORTEX"], "vortex")

        if not os.path.islink("snowtools"):
            os.symlink(SNOWTOOLS_DIR, "snowtools")

        if not os.path.islink("tasks"):
            if self.options.oper:
                os.symlink(SNOWTOOLS_DIR + "/tasks/oper", "tasks")
            else:
                if self.options.safran:
                    os.symlink(SNOWTOOLS_DIR + "/tasks/research/safran", "tasks")
                elif self.options.soda:
                    os.symlink(SNOWTOOLS_DIR + "/tasks/research/crampon", "tasks")
                elif self.options.surfex:
                    os.symlink(SNOWTOOLS_DIR + "/tasks/research/surfex", "tasks")

        for directory in ["conf", "jobs"]:
            if not os.path.isdir(directory):
                os.mkdir(directory)

    def init_job_task(self, jobname=None):
        if self.options.surfex:
            self.init_job_task_surfex(jobname=jobname)
        else:
            self.init_job_task_safran()

    def init_job_task_surfex(self, jobname=None):
        if self.options.oper:
            if self.options.monthlyreanalysis:
                self.reftask = "monthly_surfex_reanalysis"
            elif self.options.monthlyreanalysissytron:
                self.reftask = "monthly_surfex_reanalysis_sytron"
            elif self.options.forecast:
                self.reftask = "ensemble_surfex_tasks_forecast"
            else:
                self.reftask = "ensemble_surfex_tasks_analysis"
            self.nnodes = 1
            self.period = "rundate=" + self.options.datedeb.strftime("%Y%m%d%H%M")
            # Note that the jobname is used to discriminate self.conf.previ in vortex task
            if self.options.reinit:
                self.jobname = "surfex_reinit"
            elif self.options.forecast:
                self.jobname = "surfex_forecast"
            else:
                self.jobname = "surfex_analysis"
            self.confcomplement = ''
        else:
            self.period = " rundate=" + self.options.datedeb.strftime("%Y%m%d%H%M") + " datebegin=" + \
                          self.options.datedeb.strftime("%Y%m%d%H%M") + " dateend=" + \
                          self.options.datefin.strftime("%Y%m%d%H%M")
            if self.options.escroc:
                self.jobname = jobname if jobname else 'escroc'
                if self.options.scores:
                    # self.reftask  = "scores_task"
                    # self.reftask = "optim_task"
                    self.reftask = "crps_task"
                else:
                    self.reftask = "escroc_tasks"
                self.nnodes = 1
            elif self.options.forecast:
                self.jobname = "surfex_forecast"
                self.reftask = "ensemble_surfex_reforecast"
                self.nnodes = 1
            elif self.options.debug:
                self.jobname = 'debug_s2m'
                self.reftask = 'debug_tasks'
                self.nnodes = self.options.nnodes
            else:
                self.jobname = 'rea_s2m'
                self.reftask = "surfex_task"
                self.nnodes = self.options.nnodes
            self.confcomplement = " taskconf=" + self.options.datedeb.strftime("%Y")

    def init_job_task_safran(self):
        if self.options.oper:
            pass
        else:
            self.jobname = "ana_saf"
            self.reftask = "safran_analysis"
            self.period = " rundate=" + self.options.datedeb.strftime("%Y%m%d") + \
                " datebegin=" + self.options.datedeb.strftime("%Y%m%d") + \
                " dateend=" + self.options.datefin.strftime("%Y%m%d")
        self.nnodes = 1
        self.confcomplement = ""

    def set_conf_file(self):

        os.chdir(self.confdir)
        if self.options.surfex:
            if self.options.oper:
                conffilename = self.options.vapp + "_" + self.options.vconf + ".ini"
                if self.options.dev:
                    conffilename_in = self.options.vapp + "dev_" + self.options.vconf + ".ini"
                else:
                    conffilename_in = conffilename

                # Remove existing link (to allow to switch from one configuration file to another one)
                if os.path.islink(conffilename):
                    os.remove(conffilename)

                # Operational case : the configuration files are provided :
                # only create a symbolic link in the appropriate directory
                if os.path.exists("../snowtools/conf/" + conffilename_in):
                    os.symlink("../snowtools/conf/" + conffilename_in, conffilename)
                else:
                    print("WARNING : No conf file found in snowtools")
            else:
                if hasattr(self.options, 'confname'):
                    conffilename = self.options.confname.rstrip('.ini') + ".ini"
                else:
                    conffilename = self.options.vapp + "_" + self.options.vconf + "_" + \
                                   self.options.datedeb.strftime("%Y") + ".ini"

            if self.options.soda:
                print('copy conf file to vortex path')
                shutil.copyfile(self.options.soda, conffilename)

        elif self.options.safran:
            if self.options.oper:
                pass
            else:
                conffilename = self.options.vapp + "_" + self.options.vconf + ".ini"

        fullname = '/'.join([self.confdir, conffilename])

        if not self.options.oper:
            self.conf_file = Vortex_conf_file(self.options, fullname)
            self.conf_file.create_conf(jobname=self.jobname)
            self.conf_file.write_file()
            self.conf_file.close()

        os.chdir(self.workingdir)

    def mkjob_command(self):

        return "../vortex/bin/mkjob.py -j name=" + self.jobname + " task=" + self.reftask + " profile=" + \
               self.profile + " jobassistant=cen " + self.period +\
            " time=" + self.walltime() + " nnodes=" + str(self.nnodes) + self.confcomplement

    def mkjob_list_commands(self):

        if not self.options.safran and (self.options.escroc and self.options.nnodes > 1):
            mkjob_list = []
            print("loop")
            for node in range(1, self.options.nnodes + 1):
                mkjob_list.append(self.mkjob_command(jobname='escrocN' + str(node)))

            return mkjob_list
        else:
            return [self.mkjob_command()]

    def run(self):

        mkjob_list = self.mkjob_list_commands()

        os.chdir(self.jobdir)
        for mkjob in mkjob_list:
            print("Run command: " + mkjob + "\n")
            os.system(mkjob)

    def walltime(self):

        if self.options.walltime:
            return self.options.walltime

        elif self.options.oper:
            return Period(minutes=10).hms

        else:
            if self.options.escroc:
                if self.options.nmembers:
                    nmembers = self.options.nmembers
                elif len(self.options.escroc) >= 2 and self.options.escroc[0:2] == "E2":
                    # E2, E2MIP, E2tartes, E2MIPtartes
                    nmembers = 35
                else:
                    raise Exception("don't forget to specify escroc ensemble or --nmembers")

            else:
                nmembers = 1
            # minutes per year for one member computing all points
            minutes_peryear = dict(alp_allslopes=15, pyr_allslopes=15, alp_flat=5, pyr_flat=5,
                                   alp27_allslopes=20, pyr23_allslopes=20, alp27_flat=7, pyr23_flat=7,
                                   cor_allslopes=5, cor_flat=1, postes = 5,
                                   lautaret=120, lautaretreduc=5, grandesrousses250=35)

            for site_snowmip in ["cdp", "oas", "obs", "ojp", "rme", "sap", "snb", "sod", "swa", "wfj"]:
                if self.options.scores:
                    minutes_peryear[site_snowmip] = 0.2
                else:
                    minutes_peryear[site_snowmip] = 4

            for massif_safran in range(1, 100):
                minutes_peryear[str(massif_safran)] = 90

            key = self.options.vconf if self.options.vconf in list(minutes_peryear.keys()) else "alp_allslopes"

            estimation = Period(minutes=minutes_peryear[key]) * \
                         max(1, (self.options.datefin.year - self.options.datedeb.year)) * \
                         (1 + nmembers / (40 * self.options.nnodes))

            #!!!! Ne marche pas à tous les coups... 

            if estimation >= datetime.timedelta(hours=24):
                raise WallTimeException(estimation)
            else:
                return estimation.hms

    def split_geo(self):
        if ':' in self.options.region:
            splitregion = self.options.region.split(':')
            self.options.geoin = splitregion[0]
            self.options.vconf = splitregion[1]
            self.options.interpol = len(splitregion) == 3
            if self.options.interpol:
                self.options.gridout = splitregion[2]
        else:
            self.options.interpol = False
            self.options.vconf = self.options.region


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

    def create_conf(self, jobname):
        """ Prepare configuration file from s2m options"""
        self.default_variables()
        self.add_block(jobname)
        if self.options.surfex:
            self.create_conf_surfex()
            self.setwritesx()
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
        self.set_field("DEFAULT", 'ntasks', self.options.ntasks)
        self.set_field("DEFAULT", 'nprocs', self.options.ntasks)
        self.set_field("DEFAULT", 'openmp', 1)
        self.set_field("DEFAULT", 'geometry', self.options.vconf)
        if hasattr(self.options, 'addmask'):
            self.set_field("DEFAULT", 'addmask', self.options.addmask)
        if hasattr(self.options, 'prep_xpid'):
            if self.options.prep_xpid:
                if '@' not in self.options.prep_xpid:
                    self.options.prep_xpid = self.options.prep_xpid + '@' + os.getlogin()
                self.set_field("DEFAULT", 'prep_xpid', self.options.prep_xpid)

    def escroc_variables(self):

        self.set_field("DEFAULT", 'subensemble', self.options.escroc)
        # self.set_field("DEFAULT", 'duration', 'full')

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

        else:
            self.set_field("DEFAULT", 'nnodes', self.options.nnodes)
            if self.options.nmembers:
                self.set_field("DEFAULT", 'nmembers', self.options.nmembers)
            if self.options.startmember:
                self.set_field("DEFAULT", 'startmember', self.options.startmember)

    def surfex_variables(self):

        if self.options.debug:
            self.set_field("DEFAULT", 'forcingid', self.options.forcing)
        else:
            if '@' in self.options.forcing:
                self.options.forcing, forcinglogin = self.options.forcing.split('@')
            elif self.options.model == 'safran':
                forcinglogin = 'nativesafran_CEN'
            else:
                forcinglogin = os.getlogin()

            lf = self.options.forcing.split('/')
            self.set_field("DEFAULT", 'forcingid', lf[0] + '@' + forcinglogin)

            if len(lf) > 1:
                self.set_field("DEFAULT", 'blockin', '/'.join(lf[1:]))

        self.set_field("DEFAULT", 'meteo', self.options.model)

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

        if self.options.interpol:
            self.set_field("DEFAULT", 'interpol', self.options.interpol)
            self.set_field("DEFAULT", 'gridout', self.options.gridout)

        if hasattr(self.options, 'geoin'):
            self.set_field("DEFAULT", 'geoin', self.options.geoin)

    def safran_variables(self):

        self.set_field("DEFAULT", 'cumul', 6)
        self.set_field("DEFAULT", 'cutoff', self.options.cutoff)
        self.set_field("DEFAULT", 'model', 'safran')
        # Default cycle corresponding to the "official" reanalysis one, to be updated...
        self.set_field("DEFAULT", 'cycle', 'uenv:s2m.reanalysis2020.2@nativesafran_CEN')
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

    def setwritesx(self):
        if self.options.writesx:
            self.set_field("DEFAULT", 'writesx', self.options.writesx)
