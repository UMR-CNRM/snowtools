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

import vortex
from vortex.util.config import GenericConfigParser, load_template


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
        self.options = options

        if self.options.tasksinfo:
            # Only print information from the default configuration file
            self.tasksinfo()

        else:
            # Actual execution
            self.set_class_variables()
            self.execute()

    @staticmethod
    def check_vortex_install():

        if "VORTEX" not in list(os.environ.keys()):
            raise InstallException("VORTEX environment variable must be defined towards a valid vortex install.")

    def tasksinfo(self):

        # Read default configuration file
        default_conf = os.path.join(SNOWTOOLS_DIR, 'conf', f'{self.options.vapp}.ini')
        iniparser = GenericConfigParser(inifile=default_conf)
        conf = iniparser.as_dict()  # Merge=False preserves the "DEFAULT" section
        for task, section in conf.items():
            for key, value in section.items():
                if key == 'info':
                    print(f'{task} : {value}')

    def set_class_variables(self):

        self.vapp  = self.options.vapp  # edelweiss by default, this is an Edelweiss-specific launcher
        # The vconf is defined by the geometry for all CEN experiments by convention
        self.vconf = self.options.geometry.lower()

        self.datebegin = Date(self.options.datebegin)
        self.dateend   = Date(self.options.dateend)

        self.user = os.environ["USER"]

        machine = os.uname()[1]
        if 'taranis' in machine:
            self.profile = "rd-taranis-mt"
        elif 'belenos' in machine:
            self.profile = "rd-belenos-mt"
        else:
            self.profile = 'rd'

        self.define_ntasks(machine)

        if isinstance(self.options.task, dict):
            # self.options.task = dict(taskname=list(task1, task2,...))
            self.taskname = list(self.options.task.keys())[0]
            self.create_driver = True
        elif isinstance(self.options.task, list):
            pass
        else:
            # Standard case
            self.taskname = self.options.task
            self.create_driver = False
        self.jobname  = self.taskname

        self.nnodes = self.options.nnodes

    def define_ntasks(self, machine):
        if self.options.ntasks is None:
            if ('taranis' in machine or 'belenos' in machine):
                # optimum constaté pour la réanalyse Alpes avec léger dépeuplement parmi les 128 coeurs.
                self.options.ntasks = 80
                self.options.nprocs = self.options.ntasks * self.options.nnodes
            else:
                # TODO
                pass
        else:
            self.options.nprocs = self.options.ntasks * self.options.nnodes

    def execute(self):
        self.create_env()
        self.init_conf_file()
        if self.create_driver:
            self.make_driver()
        self.run()

    def init_conf_file(self):

        self.conffilename = os.path.join(self.confdir, f'{self.vapp}_{self.vconf}.ini')

        # Ensure that the configuration file is up to date
        if os.path.exists(self.conffilename):
            os.remove(self.conffilename)

        self.default_conf = os.path.join(SNOWTOOLS_DIR, 'conf', f'{self.vapp}.ini')
        shutil.copyfile(self.default_conf, self.conffilename)
        # TODO : read a potential user-defined configuration file (either provided in the command line or in .vortexrc)
        user_conf    = None

        self.iniparser = GenericConfigParser(inifile=self.conffilename, defaultinifile=user_conf)

        self.set_task_conf(self.taskname)

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

        # Add required repositories
        # for directory in ["conf", "jobs", "tasks"]:
        for directory in ["conf", "jobs"]:
            if not os.path.isdir(directory):
                os.mkdir(directory)

        if os.path.islink("tasks"):
            os.remove("tasks")
        os.symlink(os.path.join(SNOWTOOLS_DIR, "tasks", "research", self.vapp, "drivers"), "tasks")
#        for fic in glob.glob(os.path.join(SNOWTOOLS_DIR, "tasks", "research", self.vapp, "*")):
#            basename = os.path.basename(fic)
#            os.symlink(fic, f"tasks/{basename}")

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

    def make_driver(self, tplfile='driver.tpl'):
        t = vortex.ticket()
        # TODO : réfléchir à un endroit plus approprié pour mettre le template
        template = load_template(t, tplfile=os.path.join('tasks', 'drivers', tplfile))
        dst = os.path.join(self.workingdir, 'tasks', f'{self.taskname}.py')

        fill = dict()

        fill['DriverTag'] = self.jobname  # =self.options.xpid

        # TODO : load template
        nodes = []
        for task in self.options.task:
            # TODO : gérer les cas particulier (if Offline in task --> refillprep)
            nodes.append(f"            {task}(tag='{task.lower()}', ticket=t, **kw),")
            self.set_task_conf(task.lower())

        fill['nodes'] = '\n'.join(nodes)
        # Fill template and conver it into a string object
        driver_core = template(**fill)
        with open(dst, 'w') as driver:
            driver.write(driver_core)

    def set_task_conf(self, taskname):
        """
        Task-specific configuration variables
        """
        if not self.iniparser.has_section(taskname):
            # Create the section if it is not already in the dafault conf file
            self.iniparser.add_section(taskname)

    def set_job_conf(self, options):
        """
        Fill the "self.jobname" section in the configuration file that will contain
        job-specific configuration variables.
        Variables in the default "self.jobname" are used to fill variables not
        provided by the user.
        """
        # Ensure that a default section for this job exists (even if empty)
        if not self.iniparser.has_section(self.jobname):
            self.iniparser.add_section(self.jobname)

        default = self.iniparser.as_dict(merged=False)  # Merge=False preserves the "DEFAULT" section

        # Update default configuration values without replacing default values with *None* :
        for key, value in options.items():
            # Overwrite defaults values with user's command line arguments
            # Update an existing default value only if the new value is not None
            if not (value is None and (key in list(default['defaults']) + list(default[self.jobname]))):
                self.iniparser.set(self.jobname, key, str(value))

    def write_conf_file(self):

        # overwrite of iniparser 'save' methods that does not work (file open with 'rb') --> BUG ?
        # self.iniparser.save()
        with open(self.conffilename, 'w') as configfile:
            self.iniparser.write(configfile)

    def mkjob_command(self, jobname):

        mkjob = "../vortex/bin/mkjob.py"
        cmd = f"{mkjob} -j name={jobname} task={self.taskname} profile={self.profile} jobassistant=cen"

        return cmd

    def mkjob_list_commands(self):
        """
        Method to construct the actual list of job creation commands.

        Let's consider that we want to launch an execution of
        SURFEX with an MPI parallelisation for every FORCING file of an N
        members ensemble.
        We want all these jobs to share a common 'surfex_mpi' section in the
        configuration file.

        Naming the jobs 'surfex_mpi_mb1', 'surfex_mpi_mb2', ...,  'surfex_mpi_mbN'
        will lead the Vortex's job launcher to do this :
        - Get each job 'member' from the mbX extension : member=int(X)
        - put the associated value direcly in the job (variable RD_MEMBER in the
          template file, then interpreted as 'member' in the configuration
          dictionary)
        - rename all jobs 'surfex_mpi' so that they all use the same section of
          the configuration file

        # TODO : Mettre en place une solution plus Vortexienne en utilisant les Family :
        http://intra.cnrm.meteo.fr/algopy/trainings/vortex_dev2022_1/presentation/beamer/vortex_dev_jobs2_presentation.pdf

        """

        # Retrieve the number of FORCING files and launch 1 job per file
        # Each job is associated to 1 specific member so the *members_forcing*
        # attribute will be replaced by a *input_member* integer (different for each job)
        first, last, step = self.options.members_forcing.split('-')
        nforcings = int(last) - int(first) + 1
        self.njobs = nforcings

        mkjob_list = []
        options = vars(self.options)  # convert 'Namespace' object to 'dictionnary'
        if self.njobs == 1:
            options['members_forcing'] = int(first)
            self.set_job_conf(options)
            mkjob_list.append(self.mkjob_command(jobname=self.jobname))
        else:
            options.pop('members_forcing')
            for job_number in range(int(first), int(last), int(step)):
                options['members_forcing'] = str(job_number)
                jobname = f'{self.jobname}_mb{str(job_number)}'
                self.set_job_conf(options)
                mkjob_list.append(self.mkjob_command(jobname=jobname))

        self.write_conf_file()  # The configuration file is now complete, time to write it

        return mkjob_list

    def run(self):

        mkjob_list = self.mkjob_list_commands()

        os.chdir(self.jobdir)
        for mkjob in mkjob_list:
            print("Run command: " + mkjob + "\n")
            callSystemOrDie(mkjob)
