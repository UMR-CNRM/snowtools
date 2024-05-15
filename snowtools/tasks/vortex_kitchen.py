# -*- coding: utf-8 -*

"""
Created on 23 févr. 2018

@author: lafaysse
"""

import datetime
import os
import shutil
import glob
import numpy as np
import filecmp

from snowtools.utils.dates import WallTimeException
from snowtools.utils.resources import InstallException, absolute_path
from snowtools.utils.ESCROCsubensembles import ESCROC_subensembles
from snowtools.tools.execute import callSystemOrDie
from snowtools.DATA import SNOWTOOLS_DIR
from bronx.stdtypes.date import Period


class vortex_kitchen(object):
    """
    Interface between s2m command line and vortex utilities (tasks and mk_jobs)
    """

    def __init__(self, options, snowtools_command):
        """
        Constructor
        """
        # Check if a vortex installation is defined
        self.check_vortex_install()
        self.options = options
        self.snowtools_command = snowtools_command

        # Initialization of vortex variables
        self.vapp = 's2m'
        self.split_geo()

        if self.options.command == 'oper':
            if self.options.dev:
                self.options.xpid = 'dev'
            else:
                self.options.xpid = 'oper'
        else:
            self.options.xpid = self.options.diroutput  # diroutput is now always defined in research cases

        self.workingdir = "/".join([self.options.workdir, self.options.xpid, self.vapp, self.options.vconf])
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
        if hasattr(self.options, 'ntasks'):
            define_ntasks = not self.options.ntasks
        else:
            define_ntasks = True
        if define_ntasks:
            if 'taranis' in machine or 'belenos' in machine:
                self.options.ntasks = 80
                # optimum constaté pour la réanalyse Alpes avec léger dépeuplement parmi les 128 coeurs.

    def execute(self):
        self.create_env()
        self.init_job_task()
        self.set_conf_file()
        self.run()

    @staticmethod
    def check_vortex_install():

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
            if self.options.command == 'oper':
                os.symlink(SNOWTOOLS_DIR + "/tasks/oper", "tasks")
            else:
                if self.options.safran:
                    os.symlink(SNOWTOOLS_DIR + "/tasks/research/safran", "tasks")
                elif self.options.surfex:
                    if self.options.task in ['croco', 'croco_perturb']:
                        os.symlink(SNOWTOOLS_DIR + "/tasks/research/crocO", "tasks")
                    else:
                        os.symlink(SNOWTOOLS_DIR + "/tasks/research/surfex", "tasks")

        for directory in ["conf", "jobs"]:
            if not os.path.isdir(directory):
                os.mkdir(directory)

        if hasattr(self.options, 'uenv') and self.options.uenv is not None:
            if self.options.uenv.startswith('uenv'):
                # The user already created a uenv and parsed the formatted uenv name
                envname = self.options.uenv.split(':')[1].split('@')[0]
                datadir = None
            else:
                # The uenv needs to be generated
                # Set up a uenv with all files in self.options.uenv repository
                envname = '_'.join([self.vapp, self.options.vconf, self.options.xpid])
                user = os.environ['USER']
                datadir = self.options.uenv
            user = os.environ['USER']
            self.uenv = UserEnv(envname, targetdir=datadir)
            env_name, uenv_entries = self.uenv.run()  # {'FILE_NAME_EXTENSION':'my.file_name.extension.version',...}
            # uenv and udata variables will be written in the task's configuration file
            self.options.uenv = f'uenv:{env_name}@{user}'

            self.options.udata = 'dict(' + ' '.join([f'{key}:{".".join(value.split(".")[:-1])}'
                                                    for key, value in uenv_entries.items()]) + ')'
            # ex : self.options.udata = dict('FILE_NAME_EXTENSION':'my.file_name.extension',...)
            # ==> gvar='FILE_NAME_EXTENSION', local='my.file_name.extension'

    def init_job_task(self, jobname=None):
        if self.options.surfex:
            self.init_job_task_surfex(jobname=jobname)
        else:
            self.init_job_task_safran()

    def init_job_task_surfex(self, jobname=None):
        if self.options.command == 'oper':

            reftask = dict(
                analysis = "ensemble_surfex_tasks_analysis",
                forecast = "ensemble_surfex_tasks_forecast",
                monthlyreanalysis = "monthly_surfex_reanalysis",
                monthlyreanalysissytron = "monthly_surfex_reanalysis_sytron",
            )

            # Note that the jobname is used to discriminate self.conf.previ in vortex task
            defaultjobname = dict(
                analysis = "anasurf_s2m" + self.options.vconf[:3],
                forecast = "prvsurf_s2m" + self.options.vconf[:3],
                monthlyreanalysis = "monthlyanasurf_s2m" + self.options.vconf[:3],
                monthlyreanalysissytron = "monthlysytronanasurf_s2m" + self.options.vconf[:3],
            )

            self.nnodes = 1
            self.period = "rundate=" + self.options.datedeb.strftime("%Y%m%d%H%M")
            self.confcomplement = ''
        else:  # research tasks
            self.period = " rundate=" + self.options.datedeb.strftime("%Y%m%d%H%M") + " datebegin=" + \
                          self.options.datedeb.strftime("%Y%m%d%H%M") + " dateend=" + \
                          self.options.datefin.strftime("%Y%m%d%H%M")

            reftask = dict(
                surfex = "surfex_task",
                surfex_dailyprep = "surfex_task",
                escroc = "escroc_tasks",
                escroc_scores = "crps_task",  # older values scores_task optim_task
                croco = "crocO_driver",
                croco_perturb = 'crocO_perturb',
                reforecast = "ensemble_surfex_reforecast",
                debug = 'debug_tasks',
                refill = "refill",
                interpol="interpol_task",
            )

            defaultjobname = dict(
                surfex = 'rea_s2m',  # TODO : remplacer par "surfex" ou rea_surfex ?
                surfex_dailyprep = 'rea_s2m',  # # TODO : remplacer par "surfex" ou rea_surfex ?
                escroc = 'escroc',
                escroc_scores = "scores_escroc",
                croco = 'croco',
                croco_perturb = 'perturb_forcing',
                reforecast = "surfex_forecast",
                debug = 'debug_s2m',  # TODO : remplacer par "debug_surfex" ?
                refill = "refill_surfex_output",
                interpol="interpolator",
            )

            if self.options.task in ['escroc', 'croco', 'croco_perturb', 'reforecast', 'refill']:
                # In this case Taylorism prevents from using several nodes on the same run
                # But several runs can be done separately
                self.nnodes = 1
            else:
                self.nnodes = self.options.nnodes

            self.confcomplement = " taskconf=" + self.options.datedeb.strftime("%Y")
            # take care to not overlap in case of simultaneous executions with meteo and pro blocks
            if hasattr(self.options, 'interpol_blocks'):
                self.confcomplement = self.confcomplement + self.options.interpol_blocks.replace(',', '')

        # Following common between oper and research
        self.reftask = reftask[self.options.task]
        if jobname:
            self.jobname = jobname
        else:
            self.jobname = defaultjobname[self.options.task]

    def init_job_task_safran(self):
        if self.options.command == 'oper':
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

        conffilename = None
        os.chdir(self.confdir)
        if self.options.surfex:
            if self.options.command == 'oper':
                conffilename = self.vapp + "_" + self.options.vconf + ".ini"
                if self.options.dev:
                    conffilename_in = self.vapp + "devnew_" + self.options.vconf + ".ini"
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
                    suffix = '.ini'
                    if hasattr(self.options, 'interpol_blocks'):
                        suffix = self.options.interpol_blocks.replace(',', '') + suffix

                    conffilename = self.vapp + "_" + self.options.vconf + "_" + \
                        self.options.datedeb.strftime("%Y") + suffix

        elif self.options.safran:
            if self.options.command == 'oper':
                pass
            else:
                conffilename = self.vapp + "_" + self.options.vconf + ".ini"

        if conffilename:
            fullname = '/'.join([self.confdir, conffilename])
        else:
            fullname = None

        if not self.options.command == 'oper':
            self.conf_file = Vortex_conf_file(self.options, self.snowtools_command, fullname)
            self.conf_file.create_conf(jobname=self.jobname)
            # Do not write the configuration file now, additionnal informations may be added later
            # self.conf_file.write_file()
            # self.conf_file.close()

        os.chdir(self.workingdir)

    def write_conf_file(self):
        if not self.options.command == 'oper':
            self.conf_file.write_file()
            self.conf_file.close()

    def mkjob_command(self, jobname):

        return "../vortex/bin/mkjob.py -j name=" + jobname + " task=" + self.reftask + " profile=" + \
               self.profile + " jobassistant=cen " + self.period +\
            " time=" + self.walltime() + " nnodes=" + str(self.nnodes) + self.confcomplement

    def mkjob_list_commands(self):

        if not self.options.safran and (self.options.task in ['escroc', 'croco', 'croco_perturb',
                                                              'reforecast'] and self.options.nnodes > 1):
            mkjob_list = []
            for node in range(1, self.options.nnodes + 1):
                mkjob_list.append(self.mkjob_command(jobname=self.jobname + str(node)))

            self.write_conf_file()  # The configuration file is now complete, time to write it

            return mkjob_list

        elif hasattr(self.options, 'nforcing') and self.options.nforcing > 1 and self.options.nnodes > 1:
            # Case when surfex_task is forced by an ensemble
            # Condition on options.nnodes allow to not use this case for croco tasks
            # The use of options.nnodes must not be used to define the nb of nodes per domain in this case.
            mkjob_list = []
            for node in range(1, self.options.nforcing + 1):
                jobname = self.jobname + str(node)
                # Add a "jobname" block in the configuration file that will contain
                # job-specific configuration variables
                self.conf_file.set_field(jobname, 'member', node)  # Each job is assicated to 1 ensemble member
                # It is necessary to overwrite the number of proc that is automatically set to natsks*nnodes :
                self.conf_file.set_field(jobname, 'nprocs', self.options.ntasks)  # TODO : check if really necessary
                mkjob_list.append(self.mkjob_command(jobname=jobname))

            self.write_conf_file()  # The configuration file is now complete, time to write it

            return mkjob_list

        else:
            self.write_conf_file()  # The configuration file is now complete, time to write it
            return [self.mkjob_command(jobname=self.jobname)]

    def run(self):

        mkjob_list = self.mkjob_list_commands()

        os.chdir(self.jobdir)
        for mkjob in mkjob_list:
            print("Run command: " + mkjob + "\n")
            callSystemOrDie(mkjob)

    def walltime(self):

        if self.options.walltime:
            return self.options.walltime

        elif self.options.command == 'oper':
            return Period(minutes=10).hms

        else:
            if self.options.task in ['escroc', 'croco', 'croco_perturb', 'reforecast']:
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
                                   alp27_allslopes=20, pyr24_allslopes=20, alp27_flat=7, pyr24_flat=7,
                                   mac11_allslopes=5, jur4_allslopes=2, vog3_allslopes=2,
                                   mac11_flat = 2, jur4_flat = 1, vog3_flat = 1,
                                   cor2_allslopes=2, cor2_flat=1, postes = 5, postes_2022 = 5,
                                   cor_allslopes=2, cor_flat=1,
                                   lautaret=120, lautaretreduc=5, grandesrousses250=35)

            minutes_perforecast = dict(alp_allslopes=1.5, pyr_allslopes=1.5, alp_flat=0.5, pyr_flat=0.5,
                                       alp27_allslopes=2, pyr24_allslopes=2, alp27_flat=0.7, pyr24_flat=0.7,
                                       cor2_allslopes=0.2, cor2_flat=0.1, cor_allslopes=0.2, cor_flat=0.1,
                                       mac11_allslopes = 0.5, mac11_flat=0.2,
                                       jur4_allslopes = 0.2, jur4_flat = 0.1,
                                       vog3_allslopes = 0.2, vog3_flat = 0.1,
                                       postes = 0.5, postes_2022 = 0.5)

            for site_snowmip in ["cdp", "oas", "obs", "ojp", "rme", "sap", "snb", "sod", "swa", "wfj"]:
                if self.options.task == 'escroc_scores':
                    minutes_peryear[site_snowmip] = 0.2
                else:
                    minutes_peryear[site_snowmip] = 4

            for massif_safran in range(1, 100):
                minutes_peryear[str(massif_safran)] = 90

            key = self.options.vconf if self.options.vconf in list(minutes_peryear.keys()) else "alp_allslopes"

            if self.options.task in ['reforecast']:
                time1forecast = Period(minutes=minutes_perforecast[key])
                nforecast_per_year = 365 / 5 * 2
                estimation = time1forecast * nforecast_per_year * \
                    max(1, (self.options.datefin.year - self.options.datedeb.year))
            else:
                estimation = Period(minutes=minutes_peryear[key]) * \
                    max(1, (self.options.datefin.year - self.options.datedeb.year)) * \
                    (1 + nmembers / (40 * self.options.nnodes))

            # !!!! Ne marche pas à tous les coups...
            if estimation >= datetime.timedelta(hours=24):
                raise WallTimeException(estimation.hms)
            else:
                return estimation.hms

    def split_geo(self):
        if ':' in self.options.region:
            splitregion = self.options.region.split(':')
            self.options.geoin = splitregion[0].lower()
            self.options.vconf = splitregion[1].lower()
            self.options.interpol = len(splitregion) == 3
            if self.options.interpol:
                self.options.gridout = absolute_path(splitregion[2])
        else:
            self.options.interpol = False
            self.options.vconf = self.options.region.lower()


class Vortex_conf_file(object):
    def __init__(self, options, snowtools_command, filename, mode='w'):
        self.name = filename
        self.options = options
        self.snowtools_command = snowtools_command
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
                if not isinstance(value, list):
                    self.fileobject.write(fieldname + " = " + str(value) + "\n")
                else:
                    self.fileobject.write(fieldname + " = " + ','.join(map(str, value)) + "\n")
            self.fileobject.write("\n")

    def close(self):
        self.fileobject.close()

    def create_conf(self, jobname):
        """ Prepare task configuration file from s2m command options"""
        self.default_variables()
        self.add_block(jobname)
        self.jobname = jobname
        if self.options.surfex:
            self.create_conf_surfex()
            self.setwritesx()
        elif self.options.safran:
            self.create_conf_safran()

    def create_conf_interpol(self):
        self.get_forcing_variables()

        if self.options.namelist:
            self.set_field("DEFAULT", 'namelist', self.options.namelist)

        self.set_field("DEFAULT", 'gridout', self.options.gridout)
        self.set_field("DEFAULT", 'geoin', self.options.geoin)

        self.set_field("DEFAULT", 'drhook', self.options.drhook)

        if hasattr(self.options, 'save_pro'):
            self.set_field("DEFAULT", 'save_pro', self.options.save_pro)

        if hasattr(self.options, 'interpol_blocks'):
            self.set_field("DEFAULT", 'interpol_blocks', self.options.interpol_blocks)

    def create_conf_surfex(self):
        if self.options.task in ['interpol']:
            self.create_conf_interpol()
        else:
            self.surfex_variables()
            # ESCROC on several nodes
            if self.options.task in ['escroc', 'escroc_scores', 'croco', 'croco_perturb']:
                self.escroc_variables()
                if self.options.task in ['croco']:
                    self.croco_variables()
            else:
                self.set_field("DEFAULT", 'nnodes', self.options.nnodes)
                if self.options.nmembers:
                    self.set_field("DEFAULT", 'nmembers', self.options.nmembers)
                if self.options.startmember:
                    self.set_field("DEFAULT", 'startmember', self.options.startmember)

    def create_conf_safran(self):
        self.safran_variables()

    def default_variables(self):
        self.set_field("DEFAULT", 'xpid', self.options.xpid + '@' + os.getlogin())
        self.set_field("DEFAULT", 'ntasks', self.options.ntasks)
        self.set_field("DEFAULT", 'nprocs', self.options.ntasks * self.options.nnodes)
        self.set_field("DEFAULT", 'openmp', 1)
        self.set_field("DEFAULT", 'geometry', self.options.vconf)
        if hasattr(self.options, 'addmask'):
            self.set_field("DEFAULT", 'addmask', self.options.addmask)
        if hasattr(self.options, 'prep_xpid'):
            if self.options.prep_xpid:
                if '@' not in self.options.prep_xpid:
                    self.options.prep_xpid = self.options.prep_xpid + '@' + os.getlogin()
                self.set_field("DEFAULT", 'prep_xpid', self.options.prep_xpid)
        if hasattr(self.options, 'uenv'):
            if self.options.uenv is not None:
                self.set_field("DEFAULT", 'uenv', self.options.uenv)
                self.set_field("DEFAULT", 'udata', self.options.udata)
        # Archive the command instruction only for reproductibility
        self.set_field("DEFAULT", 'snowtools_command', self.snowtools_command)

    def escroc_variables(self):

        self.set_field("DEFAULT", 'subensemble', self.options.escroc)

        if self.options.nnodes > 1 and self.options.nmembers:

            nmembers_per_node = self.options.nmembers / self.options.nnodes + 1
            startmember = self.options.startmember if self.options.startmember else 1
            for node in range(1, self.options.nnodes + 1):
                nmembers_this_node = min(nmembers_per_node, self.options.nmembers - startmember + 1)
                self.set_field("DEFAULT", 'nnodes', 1)
                newblock = self.jobname + str(node)
                self.set_field(newblock, 'startmember', startmember)
                self.set_field(newblock, 'nmembers', nmembers_this_node)
                startmember += nmembers_this_node

        else:
            self.set_field("DEFAULT", 'nnodes', self.options.nnodes)
            if self.options.nmembers:
                self.set_field("DEFAULT", 'nmembers', self.options.nmembers)
            if self.options.startmember:
                self.set_field("DEFAULT", 'startmember', self.options.startmember)

    def get_forcing_variables(self):

        if self.options.task == 'debug':
            self.set_field("DEFAULT", 'forcingid', self.options.forcing)
        else:
            if '@' in self.options.forcing:
                self.options.forcing, forcinglogin = self.options.forcing.split('@')
            elif self.options.model == 'safran':
                forcinglogin = 'nativesafran_CEN'
            else:
                forcinglogin = os.getlogin()

            lf = self.options.forcing.split('/')
            addlogin = '' if lf[0] == 'oper' else '@' + forcinglogin
            self.set_field("DEFAULT", 'forcingid', lf[0] + addlogin)

            if len(lf) > 1:
                self.set_field("DEFAULT", 'blockin', '/'.join(lf[1:]))

        self.set_field("DEFAULT", 'meteo', self.options.model)

        duration = {'ESM-SnowMIP': 'full', 's2m': 'yearly', 'safran': 'yearly'}

        if self.options.model not in duration.keys():
            # Default: yearly forcing files
            duration[self.options.model] = 'yearly'

        self.set_field("DEFAULT", 'duration', duration[self.options.model])

    def surfex_variables(self):

        self.get_forcing_variables()

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

        if self.options.task == 'surfex_dailyprep':
            self.set_field("DEFAULT", 'dailyprep', True)

        if self.options.geotype == 'grid':
            self.set_field("DEFAULT", 'simu2D', True)

        if self.options.interpol:
            self.set_field("DEFAULT", 'interpol', self.options.interpol)
            self.set_field("DEFAULT", 'gridout', self.options.gridout)

        if hasattr(self.options, 'geoin'):
            self.set_field("DEFAULT", 'geoin', self.options.geoin)

        self.set_field("DEFAULT", 'drhook', self.options.drhook)

        if hasattr(self.options, 'save_pro'):
            self.set_field("DEFAULT", 'save_pro', self.options.save_pro)

        if hasattr(self.options, 'postprocess_exe'):
            self.set_field("DEFAULT", 'postprocess_exe', self.options.postprocess_exe)

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

    def perturb_variables(self):
        self.get_forcing_variables()
        if self.options.nmembers:
            self.set_field("DEFAULT", 'nmembers', self.options.nmembers)

    def croco_variables(self):
        from snowtools.tools.read_conf import read_conf
        import bisect

        # Default configuration if not prescribed
        default_attr = dict(
            # PGD xpid can be either prescribed in the conf file or taken by default to a reference spinup
            spinup_xpid = 'spinup@' + os.getlogin(),
            obsxpid = 'obs@' + os.getlogin(),
            openloop = False
        )

        # ########### READ THE USER-PROVIDED conf file ##########################
        # -> in order to append datefin to assimdates and remove the exceding dates.
        # -> in order to check if members_ids were specified.

        # local import since there are dependencies with vortex.

        confObj = read_conf(self.options.conf)

        # Attributes directly transfered to vortex conf file from s2m command options or config file or default values
        for direct_attr in ['sensor', 'scope', 'spinup_xpid', 'obsxpid', 'openloop', 'nforcing']:
            if hasattr(self.options, direct_attr):
                self.set_field('DEFAULT', direct_attr, getattr(self.options, direct_attr))
            elif hasattr(confObj, direct_attr):
                self.set_field('DEFAULT', direct_attr, getattr(confObj, direct_attr))
            elif direct_attr in default_attr.keys():
                self.set_field('DEFAULT', direct_attr, default_attr[direct_attr])

        if hasattr(confObj, 'assimdates'):
            if type(confObj.assimdates) is list:
                # case for only 1 assimilation date --> confObj.assimdates is list
                intdates = list(map(int, confObj.assimdates))
            else:
                # case for only 1 assimilation date --> confObj.assimdates is str
                intdates = [int(confObj.assimdates)]

            intdatefin = int(self.options.datefin.strftime("%Y%m%d%H"))
            intdates.sort()
            bisect.insort(intdates, intdatefin)
            intdates = np.array(intdates)
            intdates = intdates[intdates <= intdatefin]
            print('stopdates', intdates)
            intdates = intdates.tolist()
            # BC june 2020 bug in driver when stopdates has only one item
            if len(intdates) == 1:
                self.set_field('DEFAULT', 'stopdates', 'list(' + str(intdates[0]) + ')')
            else:
                self.set_field('DEFAULT', 'stopdates', intdates)

        # check if members ids were specified
        # if so, do nothing (later in the script, will be reparted between the nodes)
        # else, draw it.
        allmembers = list(range(1, self.options.nmembers + 1))

        # BC 01/04/20: this rangeX will cause us some trouble...
        self.set_field('DEFAULT', 'members', 'rangex(start:1 end:' + str(self.options.nmembers) + ')')
        if 'E1' in self.options.escroc:
            if hasattr(confObj, 'members_id'):
                members_id = np.array(list(map(int, confObj.members_id)))

                # in case of synthetic assimilation, need to:
                #    - replace the synthetic escroc member
                #    - draw a substitution forcing
                if self.options.croco == 'synth':

                    # case when the synth member comes from a larger openloop (ex 160)
                    # than the current experiment (ex 40)
                    if self.options.synthmember > self.options.nmembers:
                        self.set_field('DEFAULT', 'synth', self.options.synthmember)
                    else:
                        # replace ESCROC member
                        members_id = self.replace_member(allmembers, members_id)
                        self.set_field('DEFAULT', 'synth', self.options.synthmember)

                        # draw a substitution forcing
                        meteo_draw = self.draw_meteo(confObj)
                        self.set_field('DEFAULT', 'meteo_draw', meteo_draw)

                else:  # real observations assimilation
                    # warning in case of performing a synthetic experiment
                    # but forgetting to specify the synthetic member
                    print('\n\n\n')
                    print('************* CAUTION ****************')
                    print("If you're performing a SYNTHETIC EXPERIMENT")
                    print('you MUST specify the synthetic member     ')
                    print('to eliminate from the ensemble')
                    print('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
                    print('\n\n\n')
            else:
                if self.options.croco == 'synth':
                    # here, no prescribed members: only need to substitute meteo.
                    escroc = ESCROC_subensembles(self.options.escroc, allmembers, randomDraw=True)
                    members_id = escroc.members

                    # draw a substitution forcing
                    meteo_draw = self.draw_meteo(confObj)
                    self.set_field('DEFAULT', 'meteo_draw', meteo_draw)
                else:
                    escroc = ESCROC_subensembles(self.options.escroc, allmembers, randomDraw=True)
                    members_id = escroc.members
        else:
            escroc = ESCROC_subensembles(self.options.escroc, allmembers)
            members_id = escroc.members

        self.set_field('DEFAULT', 'members_id', list(members_id))

        # new entry for Loopfamily on offline parallel tasks:
        self.set_field('DEFAULT', 'offlinetasks', list(range(1, self.options.nnodes + 1)))

        # this line is mandatory to ensure the use of subjobs:
        # place it in the field offline for parallelization of the offlines LoopFamily only
        self.set_field('offline', 'paralleljobs_kind', 'slurm:ssh')

        if self.options.nmembers:

            # soda works with all members at the same time on one node only.
            self.set_field('soda', 'nmembersnode', self.options.nmembers)
            self.set_field('soda', 'startmember', 1)
            self.set_field('soda', 'ntasks', 1)  # one task only for sure

            # BC 18/04/19 nprocs could be set to 40
            # but I doubt this would save much time and it would be risky too.
            # so far, SODA should work in MPI, but it's risky...
            self.set_field('soda', 'nprocs', 1)

        else:
            raise Exception('please specify a conf file and a number of members to run.')

    def replace_member(self, allmembers, members_id):

        # warning in case of misspecification of --synth
        print('\n\n\n')
        print('************* CAUTION ****************')
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
        members_id[self.options.synthmember - 1] = np.random.choice([e for e in range(1, sizeE1 + 1)
                                                                    if e not in members_id])
        return members_id

    def draw_meteo(self, confObj):
        meteo_members = {str(m): ((m - 1) % int(self.options.nforcing)) + 1 for m in range(self.options.nmembers)}
        if hasattr(confObj, 'meteo_draw'):
            meteo_draw = confObj.meteo_draw
        else:
            meteo_draw = meteo_members[str(self.options.synthmember)]
        while meteo_draw == meteo_members[str(self.options.synthmember)]:
            meteo_draw = np.random.choice(list(range(1, int(self.options.nforcing) + 1)))
        print('mto draw', meteo_draw)
        return meteo_draw


class UserEnv(object):
    """
    Class to manage a Vortex-like user environment (UEnv) in a transparent way for the user.

    The basic mode is to prescribe the absolute path of a repository containing all
    files used for SURFEX experiments (launched through Vortex) not already included
    in surfex_task.py.
    In this case a Vortex-like UEnv is created including all the files under {targetdir}
    and named after the experiment vapp/vconf/xpid or {envname} argument if provided.

    This class can also be used in a more advanced way for UEnvs versioning.
    Different possibilities are provided :
    - Check an existing UEnv
    - Create a new version of the UEnv (adding or incrementing a version number)
      with files under {targetdir}
    [- Append an existing UEnv with files under {targetdir} not already in the UEnv]

    In any case, a dict of the UEnv's entries is returned providing the list of keys/values
    that make up the UEnv.
    """

    def __init__(self, envname, targetdir=None, verbose=True, max_size=1000000000, max_files=1000, append=False):
        self.targetdir = targetdir
        self.target_data = self.list_files(targetdir, max_size=max_size, max_files=max_files)
        self.user = os.environ['USER']
        self.envdir = os.path.join(os.environ['HOME'], '.vortexrc', 'hack', 'uget', self.user, 'env')
        self.datadir = os.path.join(os.environ['HOME'], '.vortexrc', 'hack', 'uget', self.user, 'data')
        self.envname = envname
        self.envfile = os.path.join(self.envdir, envname)
        self.verbose = verbose
        self.append = append  # Avoid to allow the extension of an existing UEnv
        self.valid_entries = dict()

    def run(self):
        """
        Main method.
        If the UEnv dos not exist --> create it
        If the UEnv exists :
            * If files in the UEnv match the user-defined ones --> nothing to do
            * If the UEnv contains files that differ from those in the user-defined directory:
              --> version the existing UEnv (backup) and update (do not duplicate common files)
            * If the UEnv is OK but misses some user-defined files --> append the existing UEnv

        Returns the list of valid entries associated to the uenv.

        WARNING : does not check data already pushed on Hendrix (DEV mode only !)
        """
        self.get_current_version()  # First get version number of the uenv (=1 if this is a new uenv)
        if not os.path.exists(f'{self.envfile}.{self.version}'):
            if self.verbose:
                print(f'Uenv {self.envname} does not exist')
            self.create_env()
        else:
            if self.verbose:
                print(f'Uenv {self.envname} already exist')
            self.check_data()  # Return the list of data files associated to the current UEnv

            current_files = self.current_files.keys()

            missing_files = list()
            different_files = list()
            # Look for missing files or differences
            for file in self.target_data:
                filename = file.split('/')[-1]
                if filename in current_files:
                    # reconstruct current file name with its version number
                    current_name = f'{filename}.{self.current_files[filename]}'
                    if not filecmp.cmp(file, os.path.join(self.datadir, current_name)):
                        different_files.append(file)  # Files differ
                else:
                    missing_files.append(file)  # Missing file

            if len(different_files) > 0 or (len(missing_files) > 0 and not self.append):
                # UEnv contains files different than those in the user-defined directory --> create a new version
                self.update_env(diff=different_files, new=missing_files)
            elif len(missing_files) > 0:
                # UEnv misses some user-defined files and the user allows an extension of the current UEnv
                self.extend_env(missing_files)

        actual_name = f'{self.envname}.{self.version}'

        self.user_info()

        return actual_name, self.valid_entries

    def get_current_version(self):

        self.version = 1
        if os.path.exists(self.envdir):
            if len(glob.glob(f'{self.envfile}.*')) > 0:
                versions = list()
                for file in glob.glob(f'{self.envfile}.*'):
                    versions.append(int(file.split('.')[-1]))
                self.version = max(versions)

    def list_files(self, directory, max_size=None, max_files=None):
        """
        Return list of absolute paths of files under 'directory'
        """
        if not os.path.exists(directory):
            raise OSError(f'Directory {directory} doe not exist.')
        elif len(glob.glob(os.path.join(directory, '*'))) == 0:
            raise OSError(f'Directory {directory} is empty.')

        out = list()
        for fic in glob.glob(os.path.join(directory, '*')):
            if os.path.basename(fic) != 'README':
                if (max_size and os.path.getsize(fic) > max_size):
                    raise OSError(f'File {fic} too large (>1Gb). If you want to increase the maximum size allowed,'
                                  'please use the "max_size" keyword argument.')
                out.append(fic)

        if (max_files and len(out) > max_files):
            raise OSError(f'Too many files in the {directory} directory (>{self.max_files}). If you want to increase '
                          'the maximum number of files allowed, please use the "max_files" keyword argument.')

        if len(out) == 0 and self.verbose:
            print(f'WARNING : directory {directory} is empty')

        return out

    def create_env(self):
        """
        Create a uenv file under the user's hack 'env' directory and add all data
        under 'targetdir' in it, and copy them into the user's hack data directory.
        """
        # Create user's hack environment if it does not exist
        if not os.path.isdir(self.envdir):
            os.makedirs(self.envdir)
        if not os.path.isdir(self.datadir):
            os.makedirs(self.datadir)

        if self.verbose:
            print(f'Creating UEnv {self.envname}.{self.version}')

        self.valid_entries = dict()
        for src in self.target_data:
            self.copyfile(src)

        self.write_env()

    def update_env(self, diff=list(), new=list()):
        if self.verbose:
            print(f'Updating UEnv {self.envname}.{self.version}')
        self.increment_version()
        for src in self.target_data:
            basename = src.split('/')[-1]
            version = self.current_files[basename] if basename in self.current_files.keys() else 1
            if src in diff:
                # Update file with a new version number
                self.copyfile(src, version=version + 1)
            elif src in new:
                # create with a version=1
                self.copyfile(src, version=1)
            else:
                # Use existing data
                key = basename.replace('.', '_')
                self.valid_entries[key] = f'{basename}.{version}'

        self.write_env()

    def extend_env(self, list_files):
        """
        Append the existing uenv with the given list of files.
        """
        if self.verbose:
            print(f'Extending UEnv {self.envname}.{self.version}')
        fileobject = open(f'{self.envfile}.{self.version}', 'a')
        for src in list_files:
            key, value = self.copyfile(src)
            fileobject.write(f'{key.upper()}="uget:{value}@{self.user}"\n')
            if self.verbose:
                print(f'Adding file {key} to uenv {self.envname}.{self.version}')

        fileobject.close()

    def increment_version(self):
        """
        Save previous uenv file with a suffix indicating the version number
        """
        # First get the list of existing version of the uenv
        self.version = self.version + 1
        if self.verbose:
            print(f'UEnv saved under : {self.envfile}.{self.version}')

        self.valid_entries = dict()

    def copyfile(self, src, version=1):
        """
        Copy the 'src' file in the user's hack data directory
        """
        basename = src.split('/')[-1]
        key = basename.replace('.', '_')
        fullname = f'{basename}.{version}'
        dst = os.path.join(self.datadir, fullname)
        if os.path.exists(dst):
            print(f'Error : file {dst} already exists, this should not happen')
        else:
            shutil.copyfile(src, dst)
            self.valid_entries[key] = fullname

        return key, fullname

    def write_env(self):
        fileobject = open(f'{self.envfile}.{self.version}', 'w')
        for key, value in self.valid_entries.items():
            fileobject.write(f'{key.upper()}="uget:{value}@{self.user}"\n')
        fileobject.close()

        if self.verbose:
            print(f'Uenv {self.envname}.{self.version} has been created under {self.envdir}')

    def check_data(self):
        """
        Check the presence of all data defined in the uenv under the 'hack' data directory
        Returns a dict conataining the list of files associated to the current UEnv and their version number
        """
        current = open(f'{self.envfile}.{self.version}', mode='r')
        self.current_files = dict()  # {'filename': version}
        self.valid_entries = dict()
        for line in current.readlines():
            if not line.startswith('#'):
                key, value = line.split('=')
                filename = value.split(':')[1].split('@')[0]  # Includes version number
                if os.path.exists(os.path.join(self.datadir, filename)):
                    # store to return the list of valid UEnv entries
                    self.valid_entries[key] = filename
                    basename = '.'.join(filename.split('.')[:-1])  # remove version
                    version = int(filename.split('.')[-1])
                    self.current_files[basename] = version

    def user_info(self):
        outname = os.path.join(self.targetdir, 'README')
        if os.path.exists(outname):
            out = open(outname, mode='a')
        else:
            out = open(outname, mode='w')
            out.write("S2M/Vortex INFORMATION\n")
            out.write("======================\n")
            out.write("This file has been automatically generated when the s2m command ")
            out.write("has been called with this repository in the --uenv option, and an automatic versioning ")
            out.write("of the files present at the execution time has been done.\n")
            out.write("This files provides all necessary informations on the versionning (and the previous ones).\n")
            out.write("It can be used to retrieve the files used in a specific experiment.\n")

        out.write('\n')

        date = datetime.datetime.today().strftime('%Y-%m-%d %H:%M:%S')
        out.write(f"Execution time  {date}:\n")
        out.write("------------------------------------\n")
        out.write(f"The following user environment has been created : {self.envfile}.{self.version}\n")
        out.write(f"The files present in this directory have been versionned under {self.datadir}:\n")
        for key, value in self.valid_entries.items():
            out.write(f"{self.datadir}/{value}\n")

    def infos(self):
        print(f'Uenv {self.envname} is stored under {self.envfile}.{self.version}')
        print(f'Corresponding data are store in {self.datadir}')
