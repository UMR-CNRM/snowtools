# -*- coding: utf-8 -*

"""
Created on 28 march. 2024

@author: Vernay
"""

import os
import shutil

from snowtools.tasks.vortex_kitchen import vortex_kitchen, UserEnv
from snowtools.utils.resources import InstallException
from snowtools.tools.execute import callSystemOrDie
from snowtools.DATA import SNOWTOOLS_DIR
from bronx.stdtypes.date import Date

from vortex.util.config import GenericConfigParser


class Edelweiss_kitchen(vortex_kitchen):
    """
    Interface between Edelweiss command line and vortex utilities (tasks and mk_jobs)
    """

    def __init__(self, options):
        """
        Constructor
        """
        # Check if a vortex installation is defined
        self.check_vortex_install()
        # Fetch user-defined options
        self.options   = options
        self.datebegin = Date(self.options.datebegin)
        self.dateend   = Date(self.options.dateend)

        self.vapp  = self.options.vapp  # edelweiss by default, this is an Edelweiss-specific launcher
        # The vconf is defined by the geometry for all CEN experiments by convention
        self.vconf = self.options.geometry.lower()

        self.user = os.environ["USER"]

        machine = os.uname()[1]
        if 'taranis' in machine:
            self.profile = "rd-taranis-mt"
        elif 'belenos' in machine:
            self.profile = "rd-belenos-mt"
        else:
            self.profile = 'rd'

        self.define_ntasks(machine)
        self.execute()  # Inherited from vortex_kitchen

    def define_ntasks(self, machine):
        if hasattr(self.options, 'ntasks'):
            define_ntasks = not self.options.ntasks
        else:
            define_ntasks = True
        if define_ntasks:
            if 'taranis' in machine or 'belenos' in machine:
                self.options.ntasks = 80
                # optimum constaté pour la réanalyse Alpes avec léger dépeuplement parmi les 128 coeurs.

    @staticmethod
    def check_vortex_install():

        if "VORTEX" not in list(os.environ.keys()):
            raise InstallException("VORTEX environment variable must be defined towards a valid vortex install.")

    def create_env(self):
        """Prepare environment"""

        # Check for a working directory
        if 'WORKDIR' in os.environ.keys():
            workdir = os.environ['WORKDIR']
        else:
            # Set default working directory
            workdir = os.path.join('/scratch', 'work', self.user)

        self.workingdir = os.path.join(workdir, self.options.xpid, self.vapp, self.vconf)
        self.confdir    = os.path.join(self.workingdir, 'conf')
        self.jobdir     = os.path.join(self.workingdir, "jobs")

        if not os.path.isdir(self.workingdir):
            os.makedirs(self.workingdir)
        os.chdir(self.workingdir)

        # Ensure that the Vortex module is the right one
        if os.path.islink("vortex"):
            os.remove("vortex")
        os.symlink(os.environ["VORTEX"], "vortex")

        # Ensure that the snowtools module is the right one
        if os.path.islink("snowtools"):
            os.remove("snowtools")
        os.symlink(SNOWTOOLS_DIR, "snowtools")

        # Ensure that the tasks are those in the current snowtools module
        if os.path.islink("tasks"):
            os.remove('tasks')
        os.symlink(os.path.join("snowtools", "tasks", "research", self.vapp), "tasks")

        # Add required repositories
        for directory in ["conf", "jobs"]:
            if not os.path.isdir(directory):
                os.mkdir(directory)

        # Put the default 'geometries.ini' file (from snowtools/conf) in the user's '.vortexrc' directory
        geometries = os.path.join(os.environ["HOME"], ".vortexrc", 'geometries.ini')
        if os.path.isfile(geometries):
            # Save existing file (TODO : Let a user-defined geometries.ini overwrite the default one)
            shutil.move(geometries, f'{geometries}_save')
        elif os.path.islink(geometries):
            # Remove an existing link to ensure that the geometries.ini is up to date with the one in snowtools
            os.remove(geometries)
        os.symlink(os.path.join(SNOWTOOLS_DIR, "conf", "geometries.ini"), geometries)

        if self.options.uenv is not None:
            self.set_uenv()

    def set_uenv(self):
        """
        Manage the "uenv" argument. There are 2 possibilities :
        - The user provided a valid UEnv --> Use it directly
        - The user provided an absolute path to the data --> create the UEnv
        """
        if self.options.uenv.startswith('uenv'):
            # The user already created a uenv and parsed the formatted uenv name
            # In this cas we trust the user (we could check the uenv ?)
            envname = self.options.uenv.split(':')[1].split('@')[0]
            datadir = None
        else:
            # The uenv needs to be generated
            # Set up a uenv with all files in self.options.uenv repository
            envname = '_'.join([self.options.vapp, self.options.vconf, self.options.xpid])
            datadir = self.options.uenv
            self.uenv = UserEnv(envname, targetdir=datadir)
            env_name, uenv_entries = self.uenv.run()  # {'FILE_NAME_EXT':'my.file_name.ext.version',...}
            # uenv and udata variables will be written in the task's configuration file
            self.options.uenv = f'uenv:{env_name}@{self.user}'
            self.options.udata = 'dict(' + ' '.join([f'{key}:{".".join(value.split(".")[:-1])}'
                                                    for key, value in uenv_entries.items()]) + ')'
            # ex : self.options.udata = dict('FILE_NAME_EXTENSION':'my.file_name.extension',...)
            # ==> gvar='FILE_NAME_EXTENSION', local='my.file_name.extension'

    def init_job_task(self):

        # TODO : gérer le nb de node/ membre / membre par node,...  proprement
        if self.options.task in ['escroc', 'croco', 'croco_perturb', 'reforecast', 'refill']:
            # In this case Taylorism prevents from using several nodes on the same run
            # But several runs can be done separately
            self.nnodes = 1
        else:
            self.nnodes = self.options.nnodes

        # Gérer les families (liste de taches)
        self.reftask = self.options.task
        self.jobname = self.options.task

    def set_conf_file(self):

        conffilename = None
        os.chdir(self.confdir)

        # TODO : lire / modifier le fichier de conf par défaut

        conffilename = f'{self.vapp}_{self.vconf}.ini'
        fullname     = os.path.join(self.confdir, conffilename)

        if os.path.exists(fullname):
            os.remove(fullname)

        default_conf = os.path.join(SNOWTOOLS_DIR, 'conf', f'{self.vapp}.ini')
        shutil.copyfile(default_conf, conffilename)
        # TODO : read a potential user-defined configuration file (either provided in the command line or in .vortexrc)
        user_conf    = None

        options = vars(self.options)  # convert 'Namespace' object to 'dictionnary'
        # WARNING section named after $task is actually the JOB section !
        task = options.pop('task')  # Get user-defined task. TODO : check if task exists !

        iniparser = GenericConfigParser(inifile=conffilename, defaultinifile=user_conf)
        print(iniparser.sections())

        default        = iniparser.as_dict(merged=False)  # Merge=False preserves the "DEFAULT" section

#        if task in iniparser.sections():
#            default = iniparser.as_dict()  # Each 'section' includes the default values
#        else:  # security in case the default conf file is not up to date
#            print(f"WARNING : Task {task} is not in the default configuration file '{self.vapp}.ini'."
#                  "This file needs to be updated.")
#            # Add all variables from "DEFAULT" section in the new task's section
#            default        = iniparser.as_dict(merged=False)  # Merge=False preserves the "DEFAULT" section

        for key, value in options.items():
            # Overwrite defaults values with user's command line arguments
            # Update an existing default value only if the new value is not None
            if value is None and (key in default['defaults'].keys() or
                    (task in default.keys() and key in default[task].keys())):
                pass
            else:
                iniparser.set(task, key, str(value))

        # overwrite of iniparser 'save' methods that does not work (file open with 'rb') --> BUG ?
        # iniparser.save()
        with open(conffilename, 'w') as configfile:
            iniparser.write(configfile)

        os.chdir(self.workingdir)

    def write_conf_file(self):
        self.conf_file.write_file()
        self.conf_file.close()

    def mkjob_command(self, jobname):

        mkjob = "../vortex/bin/mkjob.py"
        cmd = f"{mkjob} -j name={jobname} task={self.reftask} profile={self.profile} jobassistant=cen"

        return cmd

    def mkjob_list_commands(self):

        # TODO : gérer les lancements d'ensemble sur plusieurs noeuds proprement
        return [self.mkjob_command(jobname=self.jobname)]

    def run(self):

        mkjob_list = self.mkjob_list_commands()

        os.chdir(self.jobdir)
        for mkjob in mkjob_list:
            print("Run command: " + mkjob + "\n")
            callSystemOrDie(mkjob)
