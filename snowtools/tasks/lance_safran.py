#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''
Created on Apr, 30. 2017

@author: Vernay
'''

import os
import sys
import argparse

from snowtools.tasks.vortex_kitchen import Vortex_conf_file
from snowtools.tasks.s2m_launcher import _S2M_command
from snowtools.utils.dates import check_and_convert_date
from snowtools.tools.execute import callSystemOrDie
from snowtools.utils.resources import InstallException

from snowtools.DATA import SNOWTOOLS_DIR


cutoff_map = dict(
    analysis   = 'assimilation',
    analyse    = 'assimilation',
    forecast   = 'prevision',
)


class Safran_command(_S2M_command):
    """class for SURFEX experiments launching commands"""

    def parse_options(self, arguments):
        description = "Generating Safran guess files."
        parser = argparse.ArgumentParser(description=description)
        parser.add_argument('-b', '--datedeb', help='Date to start the simulation (YYYYMMDD): MANDATORY OPTION', required=True)
        parser.add_argument('-e', '--datefin', help='Date to finish the simulation (YYYYMMDD): MANDATORY OPTION (unless --oper)', required=True)
        parser.add_argument('-r', '--region', help='Region over wich you want to launch your simulation', choices=['alp', 'pyr', 'cor'], required=True)
        parser.add_argument('-i', '--xpid', help='experiment ID: set the output directory', required=True)
        parser.add_argument('-c', '--cutoff', help='Cutoff of the simulation', choices=['analyse', 'prevision', 'assimilation', 'forecast', 'analysis'], default='assimilation')
        parser.add_argument('-o', '--observations', help='Path of the set of observations to be used')
#        parser.add_argument('-p', '--postes', help='Set the carpost file (list of simulation stations)')
        parser.add_argument('-x', '--executables', help='Path containing SAFRAN executables to be used')
        parser.add_argument('-n', '--namelist', help='Path containing the SAFRAN namelists to be used')
        parser.add_argument('-g', '--guess', help='Abspath of the directory containing guess to be used (either in the specified directory,'
                            'or in a subdirectory named after the season (format nnNN). To get archive files from hendrix,'
                            'the path must start with "hendrix:" or with "username@hendrix:"')
        parser.add_argument('-C', '--cpl_model', nargs=2, help='Couple of additional informations concerning the guess (model, configuration, grid,...)).'
                            'For example "-C era5 025". This information can be used to generate a proper Vortex Forcing name if the option -s is not used (recommended).' )
        parser.add_argument('-w', '--workdir', dest='dirwork', help="name of the output directory")
        parser.add_argument('-s', '--savedir', help='Specify an output directory to store generated files. To store files on hendrix, the path must start'
                            'with "hendrix:". It is recommended not to use this option, the SAFRAN forcing will then be stored on a proper Vortex cache.')
        parser.add_argument('-t', '--walltime', help="Specify your job walltime (format hh:mm:ss)", default='00:30:00')
        parser.add_argument('--oper', action='store_true', help='Operational chain')
        parser.add_argument('--safran', help='Launch safran task', action='store_true', default=True)
        parser.add_argument('--prepsaf', help='Launch prepsaf task', action='store_true', default=False)
        parser.add_argument('--ensemble', help='Explicitly specify an ensemble simulation', action='store_true')
        parser.add_argument('--ntasks', help='Explicitly specify the number of tasks by node (not used in general)', default=80)

        args = parser.parse_args()

        if args.cutoff in ['analysis', 'forecast', 'analyse']:
            args.cutoff = cutoff_map[args.cutoff]

        if args.xpid:
            args.outdir = args.xpid

        args.surfex = False

        return args

    def check_and_convert_options(self, vortex=False):
        self.options.datedeb = check_and_convert_date(self.options.datedeb)
        self.options.datedeb = self.options.datedeb
        self.options.datefin = check_and_convert_date(self.options.datefin)
        self.check_mandatory_arguments(**{'-i': 'xpid', '-c': 'cutoff'})

    def execute(self):
        # Check option values and convert them in types suited for defining a run configuration
        self.check_and_convert_options(vortex=True)

        if 'workdir' not in self.options:
            if 'WORKDIR' in list(os.environ.keys()):
                self.options.workdir = os.path.join(os.environ['WORKDIR'], self.options.xpid, 'safran',
                        self.options.region)
            else:
                self.options.workdir = os.path.join(self.options.xpid, 'safran', self.options.region)

        # vortex_kitchen(self.options)
        self.create_env()
        self.init_job_task_safran()
        self.make_conf_file()
        mkjob = self.mkjob_command()
        print("Run command: " + mkjob + "\n")
        callSystemOrDie(mkjob)

    def create_env(self):

        if not os.path.isdir(self.options.workdir):
            os.makedirs(self.options.workdir)
        os.chdir(self.options.workdir)

        if not os.path.islink("snowtools"):
            os.symlink(SNOWTOOLS_DIR, "snowtools")

        if not os.path.islink("tasks"):
            os.symlink(SNOWTOOLS_DIR + "/tasks/research/safran", "tasks")

        for directory in ["conf", "jobs"]:
            if not os.path.isdir(directory):
                os.mkdir(directory)

        if not os.path.islink("vortex"):
            if "VORTEX" not in list(os.environ.keys()):
                raise InstallException("VORTEX environment variable must be defined towards a valid vortex install.")
            else:
                os.symlink(os.environ["VORTEX"], "vortex")

    def init_job_task_safran(self):
        self.jobname = "ana_saf"
        self.reftask = "safran_analysis"
        self.period = " rundate=" + self.options.datedeb.strftime("%Y%m%d") + \
            " datebegin=" + self.options.datedeb.strftime("%Y%m%d") + \
            " dateend=" + self.options.datefin.strftime("%Y%m%d")
        self.options.nnodes = 1
        self.confcomplement = ""

    def make_conf_file(self):
        confdir = os.path.join(self.options.workdir, 'conf')
        self.options.vconf = self.options.region
        fullname = os.path.join(confdir, f'safran_{self.options.vconf}.ini')
        self.conf_file = Vortex_conf_file(self.options, None, fullname)
        self.conf_file.create_conf(jobname=self.jobname)
        self.conf_file.write_file()
        self.conf_file.close()

    def mkjob_command(self):

        return "vortex/bin/mkjob.py -j name=" + self.jobname + " task=" + self.reftask + \
               " profile=rd-belenos-mt jobassistant=cen " + self.period +\
               " time=" + self.options.walltime + " nnodes=" + str(self.options.nnodes) + self.confcomplement


if __name__ == "__main__":
    safran = Safran_command(sys.argv)
