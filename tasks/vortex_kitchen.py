'''
Created on 23 févr. 2018

@author: lafaysse
'''

import os

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
        os.makedirs(self.workingdir)
        os.chdir(self.workingdir)

        os.symlink(os.environ["VORTEX"], "vortex")
        os.symlink(os.environ["SNOWTOOLS_CEN"] + "/tasks", "tasks")

        for directory in ["conf", "jobs"]:
            os.mkdir(directory)

        os.chdir("jobs")
        os.symlink(os.environ["SNOWTOOLS_CEN"] + "/jobs/" + self.jobtemplate, self.jobtemplate)

    def create_conf(self, options):
        ''' Prepare configuration file from s2m options'''
        confname = self.vapp + "_" + self.vconf + ".ini"
        conffile = vortex_conf_file(confname, 'w')

        conffile.new_class("DEFAULT")
        conffile.write_field('meteo', 'safran')
        conffile.write_field('geometry', self.vconf)
        conffile.write_field('forcingid', 'reanalyse@lafaysse')
        conffile.write_field('xpid', self.xpid + '@' + os.getlogin())
        conffile.write_field('ntasks', 40)
        conffile.write_field('nprocs', 40)
        conffile.write_field('nnodes', 1)
        conffile.write_field('openmp', 1)
        conffile.close()

    def mkjob_command(self, options):
        return "../vortex/bin/mkjob.py -j name=rea_s2m task=vortex_tasks profile=rd-beaufix-mt jobassistant=cen datebegin="\
            + options.datedeb.strftime("%Y%m%d%H%M") + " dateend=" + options.dateend.strftime("%Y%m%d%H%M") + "template=" + self.jobtemplate

    def run(self, options):
        os.system(self.mkjob_command(options))


class vortex_conf_file(file):

    def new_class(self, name):
        self.write("[" + name + "]\n")

    def write_field(self, fieldname, value):
        self.write(fieldname + " = " + str(value) + "\n")
