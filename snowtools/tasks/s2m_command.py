#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Created on 30 Aug. 2017

@author: lafaysse
"""

# General python modules
import argparse
import os
import sys
import datetime
import textwrap

# Import snowtools modules
from snowtools.tools.initTG import clim
from snowtools.utils.dates import checkdateafter, check_and_convert_date
from snowtools.utils.resources import absolute_path, check_surfex_exe
from snowtools.utils.infomassifs import infomassifs
from snowtools.tasks import runs
from snowtools.tasks.vortex_kitchen import vortex_kitchen
from snowtools.tasks.crocO_vortex_kitchen import crocO_vortex_kitchen
from snowtools.tasks.s2m_launcher import _S2M_command
from snowtools.DATA import SNOWTOOLS_DIR
from snowtools.utils.FileException import UndefinedDirectoryException, UnsupportedOptionException


class Surfex_command(_S2M_command):
    """class for SURFEX experiments launching commands"""

    def execute(self):
        machine = os.uname()[1]
        if "taranis" in machine or "belenos" in machine:
            self.execute_through_vortex()
        else:
            self.execute_without_vortex()

    def check_and_convert_options(self, vortex=False):

        if self.options.command == 'oper':
            self.check_mandatory_arguments(**{'-r': 'region'})

            if self.options.datedeb:
                self.options.datedeb = check_and_convert_date(self.options.datedeb)
            else:
                # If rundate is not prescribed, get it from the current time.
                self.options.datedeb = self.set_default_date()

        else:
            self.check_mandatory_arguments(**{'-b': 'datedeb', '-e': 'datefin', '-f': 'forcing'})

            if not self.options.onlyextractforcing:
                try:
                    self.options.exesurfex = check_surfex_exe(self.options.exesurfex)
                except UndefinedDirectoryException as e:
                    if not vortex:
                        raise e
                    else:
                        print("Use SURFEX binaries from GENV\n")
                else:
                    print("USE SURFEX binaries from " + self.options.exesurfex)

            # Controls and type conversions of dates
            [self.options.datedeb, self.options.datefin, self.options.datespinup] = list(map(check_and_convert_date,
                                                                                             [self.options.datedeb,
                                                                                              self.options.datefin,
                                                                                              self.options.datespinup]))
            checkdateafter(self.options.datefin, self.options.datedeb)

            if vortex:
                self.check_mandatory_arguments(**{'-r': 'region', '-m': 'model'})

        # self.check_mandatory_arguments()
            self.set_path(vortex)
            self.set_geo(vortex)

    def set_geo(self, vortex):

        #
        if self.options.region:
            self.interpol = os.path.isfile(self.options.region)
        else:
            self.interpol = False
        if self.interpol:
            self.options.region = absolute_path(self.options.region)
            self.interpol = True

        # Check and conversion of geographical requirements
        elif self.options.region or self.options.slopes or self.options.aspects or self.options.minlevel \
                or self.options.maxlevel:
            INFOmassifs = infomassifs()

            if not vortex:
                self.options.region = INFOmassifs.region2massifs(self.options.region)

            if self.options.slopes:
                self.options.slopes = self.options.slopes.split(",")
            else:
                if 'allslopes' in self.options.region:
                    self.options.slopes = ["0", "20", "40"]
                else:
                    self.options.slopes = ["0"]

            self.options.aspects = INFOmassifs.get_list_aspect(self.options.aspects, self.options.slopes)
            self.options.minlevel, self.options.maxlevel \
                = INFOmassifs.check_and_convert_min_max_elevation(self.options.minlevel, self.options.maxlevel)

    def set_path(self, vortex):
        # Conversions of local paths in absolute paths
        [self.options.namelist, self.options.workdir, self.options.exesurfex] = \
            list(map(absolute_path, [self.options.namelist, self.options.workdir, self.options.exesurfex]))

        if self.options.conf:
            self.options.conf = absolute_path(self.options.conf)

        if not vortex:
            [self.options.forcing, self.options.diroutput] = \
                list(map(absolute_path, [self.options.forcing, self.options.diroutput]))

    def set_default_date(self):
        today = datetime.datetime.today()
        newhour = today.hour - today.hour % 3
        return today.replace(hour=newhour, minute=0, second=0, microsecond=0)

    def parse_options(self, arguments):

        s2m_description = """
             S2M command, that allow to launch SAFRAN-SURFEX-Crocus simulations
             (or any part of this model chain) as well as EDELWEISS simulations.
             """
        s2m_research_description = """
             The command to be used for research tasks to launch
             SAFRAN-SURFEX-Crocus or EDELWEISS simulations.

             s2m simulations need ground temperature to be initialized.
             If initial conditions are not known (PREP file not available for the starting
             date), then an init_TG.nc file is expected to be found in the prep directory.
             It must contain an initial ground temperature variable called TG with
             dimension (Number_of_points).
             The init_TG.nc file can be generated using -G or -g options (see their
             description). Otherwise, the command will crash.
             """
        parser = argparse.ArgumentParser(prog="s2m",
                                         description=s2m_description,
                                         epilog="For detailed documentation of general use case, "
                                         "run s2m research --help",
                                         )

        subparsers = parser.add_subparsers(dest='command')
        parser_research = subparsers.add_parser('research',
                                                description=s2m_research_description,
                                                formatter_class=argparse.ArgumentDefaultsHelpFormatter,
                                                )

        parser_research_main = parser_research.add_argument_group('Main arguments')

        parser_research_main.add_argument("-b", "--begin",
                                          type=str, dest="datedeb", required=True,
                                          help="Date of the beginning of the simulation.")

        parser_research_main.add_argument("-e", "--end",
                                          type=str, dest="datefin", required=True,
                                          help="Date of the end of the simulation")

        parser_research_main.add_argument("-o", "--output",
                                          type=str, dest="diroutput", default="output",
                                          help="Name of the output directory")

        parser_research_main.add_argument("-f", "--forcing",
                                          type=str, dest="forcing", default=None,
                                          help="Path of the forcing file or of the directory with the forcing files")

        parser_research_main.add_argument("-n", "--namelist",
                                          type=str, dest="namelist",
                                          default=SNOWTOOLS_DIR + '/DATA/OPTIONS_V8.1_NEW_OUTPUTS_NC.nam',
                                          help="Path of the mother namelist")

        parser_research_init = parser_research.add_argument_group('Initialization set up')
        parser_research_init.add_argument("-G", action="store_true", dest="groundonly", default=False,
                                          help=textwrap.dedent(
                                              """\
                                              Generate a ground initialization file by computing a climatological
                                              average of air temperature on the provided period.
                                              With this option, the simulation is not run, the user can use -g
                                              instead to combine the ground temperature initialization and running
                                              the simulation.
                                              """)
                                          )

        parser_research_init.add_argument("-g", action="store_true", dest="ground", default=False,
                                          help="Combines -G option and run the simulation aftterwards.")

        parser_research_init.add_argument("-x", "--spinupdate",
                                          type=str, dest="datespinup", default=None,
                                          help="Date of validity of the spinup file")

        parser_research_init.add_argument("-a", "--august_threshold",
                                          type=int, dest="threshold", default=-999,
                                          help="Limit the snow water equivalent each 1st August "
                                          "to the provided value in kg/m2")

        parser_research_forcing = parser_research.add_argument_group('Forcing pre-processing')

        parser_research_forcing.add_argument("-r", "--region", "--geometry",
                                             type=str, dest="region", default=None,
                                             help='''
                                             The geometry to use for SURFEX simulation.

                                             This option is also used for pre-processing of the FORCING file by
                                             extracting data or interpolating forcing files into a new geometry

                                             - If region is a single massif number or a list of massif numbers
                                             separated by comas or a generic name for a group of massifs,
                                             extract the corresponding massifs.

                                             - If region is a netcdf file describing the target geometry, interpolate
                                             SAFRAN fields on this geometry.
                                             The netcdf file describing the geometry must contain the massif_number and
                                             ZS variables. It can be 1d (Number_of_points) or 2d (y, x) in Lambert93
                                             or 2d (latitude, longitude). 1d files can optionnally provide aspect
                                             and slope. These variables will be computed by the software for 2d files.

                                             Note that you need to compile the fortran interpolator to use this
                                             extended option.
                                             ''')

        parser_research_forcing.add_argument("-L", "--lowest",
                                             type=int, dest="minlevel", default=None,
                                             help="Eliminate elevations lower than this level in FORCING "
                                             "files (in meters)")

        parser_research_forcing.add_argument("-U", "--upper",
                                             type=int, dest="maxlevel", default=None,
                                             help="Eliminate elevations above this level in FORCING files (in meters)")

        parser_research_forcing.add_argument("-c", "--classes_aspect",
                                             type=int, dest="aspects", default=None,
                                             help='''Specify the number of aspect classes to extract or generate.
                                             Default: 8

                                             -c 8 : N, NE, E, SE, S, SW, W, NW
                                             -c 4 : N, E, S, W
                                             -c 2 : N, S
                                             -c 135 : specify only 1 aspect in ° if multiple of 45°
                                             '''
                                             )

        parser_research_forcing.add_argument("-l", "--list_slopes",
                                             type=str, dest="slopes", default=None,
                                             help="Extract a reduced number of slope angles in FORCING files, "
                                             "or generate a larger number of slope angles for a flat FORCING file")

        parser_research_forcing.add_argument("-E", "--extractforcing",
                                             action="store_true", dest="onlyextractforcing", default=False,
                                             help="Only extract meteorological forcing")

        parser_research_forcing.add_argument("--addmask",
                                             action="store_true", dest="addmask", default=False,
                                             help="Apply shadows on solar radiation from surrounding masks")

        parser_research_path = parser_research.add_argument_group('Optional path definitions')

        parser_research_path.add_argument("-s", "--surfexexec",
                                          type=str, dest="exesurfex", default=None,
                                          help="Specify the directory where to find SURFEX binaries. "
                                          "(Must contains PGD, PREP, OFFLINE) "
                                          "If None, EXESURFEX environment variable is used.")

        parser_research_path.add_argument("-w", "--workdir",
                                          type=str, dest="workdir", default=None,
                                          help="Name of the running directory (workdir)")

        parser_research_others = parser_research.add_argument_group('Other arguments')

        parser_research_others.add_argument("--geotype",
                                            dest="geotype", default='unstructured',
                                            choices=['unstructured', 'grid'],
                                            help="Type of simulation geometry: grids must be defined in namelist "
                                            "while namelists are automatically updated for unstructured geometries "
                                            "according to the forcing file.")

        parser_research_others.add_argument("--veg",
                                            dest="veg", default=None,
                                            choices=['namelist', 'ecoclimap'],
                                            help="""
                                            When vegetation is extracted from ECOCLIMAP database, the corresponding
                                            files are downloaded in the working directory.

                                            Default is namelist when --geotype=unstructured.
                                            Default is eoclimap when --geotype=grid.
                                            """
                                            )

        parser_research_others.add_argument("--uenv",
                                            action="store", dest="uenv", default=None,
                                            help="In cases when additionnal input files are necessary, use this "
                                            "option to define the absolute path of the repository where those files "
                                            "are stored."
                                            "This option works for both Vortex/HPC and local executions."
                                            "In a Vortex execution, it is also possible (and advised) to provide a "
                                            "an existing uenv using the followong syntax : "
                                            "'--uenv=uenv:{uenv_name}@{username}'.")

        parser_research_vortex = parser_research.add_argument_group('Vortex options '
                                                                    '(available only on Meteo-France supercomputers)')

        parser_research_vortex.add_argument("-m", "--model",
                                            type=str, dest="model", default=None,
                                            help="Meteorological forcing format: s2m for ready-to-use s2m-generated FORCING "
                                                 " and safran for raw SAFRAN files.")

        parser_research_vortex.add_argument("-p", "--prep_xpid",
                                            action="store", type=str, dest="prep_xpid", default=None,
                                            help="xpid in wich are the PREP files to be used")

        parser_research_vortex.add_argument("--obsxpid",
                                            action="store", type=str, dest="obsxpid", default=None,
                                            help="xpid of the obs you want to assimilate")

        parser_research_vortex.add_argument("--nnodes",
                                            action="store", type=int, dest="nnodes", default=1,
                                            help="Total number of nodes requested by the s2m command"
                                                 " (can be split between several jobs or not)."
                                                 " Take care, this is not the number of nodes per job if several"
                                                 " jobs !!!")

        parser_research_vortex.add_argument("--ntasks",
                                            action="store", type=int, dest="ntasks", default=None,
                                            help="Number of tasks (and procs) per node.")

        parser_research_vortex.add_argument("--walltime",
                                            action="store", type=str, dest="walltime", default=None,
                                            help="specify your job walltime (format hh:mm:ss)")

        parser_research_vortex.add_argument("--writesx",
                                            action="store_true", dest="writesx", default=False,
                                            help="Optionnaly transfer the PRO files towards sxcen")

        parser_research_vortex.add_argument("--save-pro",
                                            dest="save_pro", default='multi',
                                            choices=['none', 'cache', 'archive', 'multi'],
                                            help="Wheteher and where to save PRO_ output files. "
                                                 "multi is the default and store both in cache and in archive. "
                                                 "Use none not to save the PRO_ files at all.")

        parser_research_vortex.add_argument("--postprocess",
                                            dest="postprocess_exe", default=None,
                                            help="Postprocessing of PRO file. Provide executable path (absolute) "
                                                 "and its options as a single string. Will be executed after a"
                                                 "SURFEX execution in the working directory. If you want anything"
                                                 "to be saved, please write in commonly used files (PRO, DIAG, CUMUL)."
                                                 "Please ensure yourself that your algorithm is correctly parallelized."
                                            )

        parser_research_vortex.add_argument("--drhook",
                                            action="store_true", dest="drhook", default=False,
                                            help="Profiling MPI task with DRHOOK")

        parser_research_vortex.add_argument("--task",
                                            dest="task", type=str, default='surfex',
                                            choices=["surfex", "surfex_dailyprep", "escroc", "escroc_scores", "croco",
                                                     "croco_perturb", "reforecast", "debug", "refill", "interpol"],
                                            help="The task, if not the default one.")

        parser_research_vortex.add_argument("--escroc",
                                            type=str, dest="escroc", default='E2',
                                            help="ESCROC subensemble in case --task=escroc")

        parser_research_vortex.add_argument("--croco",
                                            type=str, dest="croco", default='openloop',
                                            choices=["openloop", "synth", "real"],
                                            help="assimilation mode in case --task=croco")

        parser_research_vortex.add_argument("--conf",
                                            type=str, dest="conf", default=None,
                                            help="configuration file in case --task=croco")

        parser_research_vortex.add_argument("--synthmember",
                                            type=str, dest="synthmember", default=1,
                                            help="synthetic member if --croco=synth")

        parser_research_vortex.add_argument("--nforcing",
                                            action="store", type=int, dest="nforcing", default=1,
                                            help="Number of members of forcing files in Croco task only")

        parser_research_vortex.add_argument("--nmembers",
                                            action="store", type=int, dest="nmembers", default=None,
                                            help="Total number of executions of the binary or script associated"
                                                 "with the main algocomponent of the task.")

        parser_research_vortex.add_argument("--startmember",
                                            action="store", type=int, dest="startmember", default=None,
                                            help="Number of first member")

        parser_research_vortex.add_argument("--sensor",
                                            action="store", type=str, dest="sensor", default="MODIS",
                                            help="specify the sensor name of the obs you want to assimilate")

        parser_research_vortex.add_argument("--interpol_blocks",
                                            type=str, dest="interpol_blocks", default='',
                                            choices=['meteo', 'pro', 'meteo,pro', 'pro,meteo'],
                                            help="List of blocks to interpol --task=interpol")

        parser_oper = subparsers.add_parser('oper', description="Do not use unless you know what it does. "
                                            "Use s2m oper --help for details")

        parser_oper.add_argument("-b", "--begin",
                                 type=str, dest="datedeb", required=True,
                                 help="Rundate (réseau).")

        parser_oper.add_argument("-r", "--region",
                                 type=str, dest="region", required=True,
                                 help='vconf of the operational run')

        parser_oper.add_argument("--dev",
                                 action="store_true", dest="dev", default=False,
                                 help="Operational chain in development")

        parser_oper.add_argument("--task",
                                 dest="task", type=str, default='analysis',
                                 choices=["analysis", "forecast", "monthlyreanalysis", "monthlyreanalysissytron"],
                                 help="")

        parser_oper.add_argument("--walltime",
                                 action="store", type=str, dest="walltime", default=None,
                                 help="specify your job walltime (format hh:mm:ss)")

        options  = parser.parse_args(arguments)

        options.surfex = True
        options.safran = False

        return options

    def execute_without_vortex(self):

        # Check option values and convert them in types suited for defining a run configuration
        self.check_and_convert_options(vortex=False)
        self.check_mandatory_arguments(**{'-o': 'diroutput'})

        if self.options.ground or self.options.groundonly:
            clim(self.options)

        if not self.options.groundonly:

            # Define a run object
            if type(self.options.forcing) is list or self.options.addmask:
                run = runs.postesrun(self.options.datedeb, self.options.datefin, self.options.forcing,
                                     self.options.diroutput, threshold=self.options.threshold,
                                     workdir=self.options.workdir, datespinup=self.options.datespinup,
                                     execdir=self.options.exesurfex,
                                     namelist=self.options.namelist,
                                     addmask=True, onlyextractforcing=self.options.onlyextractforcing,
                                     s2mcommand=self.command)
            elif self.interpol:
                if 'pro' in self.options.forcing or 'PRO' in self.options.forcing:
                    myclass = runs.interpolpro
                elif self.options.geotype == 'grid':
                    myclass = runs.interpolgriddedrun
                else:
                    myclass = runs.interpolrun

                run = myclass(self.options.datedeb, self.options.datefin,
                              self.options.forcing, self.options.diroutput,
                              threshold=self.options.threshold, workdir=self.options.workdir,
                              datespinup=self.options.datespinup,
                              geolist=[self.options.region], execdir=self.options.exesurfex,
                              namelist=self.options.namelist,
                              onlyextractforcing=self.options.onlyextractforcing,
                              s2mcommand=self.command)

            elif self.options.region or self.options.slopes or self.options.aspects or self.options.minlevel \
                    or self.options.maxlevel:

                if self.options.onlyextractforcing:
                    if 'pro' in self.options.forcing or 'PRO' in self.options.forcing:
                        run = runs.massifextractpro(self.options.datedeb, self.options.datefin,
                                                    self.options.forcing, self.options.diroutput,
                                                    workdir=self.options.workdir,
                                                    geolist=[self.options.region, self.options.minlevel,
                                                             self.options.maxlevel, self.options.slopes,
                                                             self.options.aspects],
                                                    s2mcommand=self.command)
                    else:
                        run = runs.massifextractforcing(self.options.datedeb, self.options.datefin,
                                                        self.options.forcing, self.options.diroutput,
                                                        workdir=self.options.workdir,
                                                        geolist=[self.options.region, self.options.minlevel,
                                                                 self.options.maxlevel, self.options.slopes,
                                                                 self.options.aspects],
                                                        s2mcommand=self.command)
                else:

                    run = runs.massifrun(self.options.datedeb, self.options.datefin, self.options.forcing,
                                         self.options.diroutput, threshold=self.options.threshold,
                                         workdir=self.options.workdir, datespinup=self.options.datespinup,
                                         execdir=self.options.exesurfex,
                                         namelist=self.options.namelist,
                                         geolist=[self.options.region, self.options.minlevel,
                                                  self.options.maxlevel, self.options.slopes,
                                                  self.options.aspects],
                                         s2mcommand=self.command)
            else:
                if self.options.geotype == 'grid':
                    if self.options.veg in ['ecoclimap', None]:
                        myclass = runs.griddedrun
                    else:
                        raise UnsupportedOptionException("Gridded simulations (--geotype=grid) with homogeneous "
                                                         "vegetation (--veg=namelist) are not implemented")
                else:  # geotype=unstructured
                    if self.options.veg in ['ecoclimap', None]:
                        myclass = runs.ecoclimaprun
                    else:
                        myclass = runs.surfexrun

                run = myclass(self.options.datedeb, self.options.datefin, self.options.forcing,
                              self.options.diroutput, threshold=self.options.threshold,
                              workdir=self.options.workdir, datespinup=self.options.datespinup,
                              execdir=self.options.exesurfex,
                              namelist=self.options.namelist,
                              s2mcommand=self.command)

            if self.options.uenv:
                run.get_extra_files(self.options.uenv)

            # Execute the run
            run.run()

    def execute_through_vortex(self):

        # Check option values and convert them in types suited for defining a run configuration
        self.check_and_convert_options(vortex=True)

        if hasattr(self.options, 'workdir'):
            defaultworkdir = not self.options.workdir
        else:
            defaultworkdir = True  # s2m oper command

        if defaultworkdir:
            if 'WORKDIR' in list(os.environ.keys()):
                self.options.workdir = os.environ['WORKDIR']
            else:
                self.options.workdir = "."

        # Cook vortex task
        if self.options.task == 'croco':
            crocO_vortex_kitchen(self.options, self.command)
        else:
            vortex_kitchen(self.options, self.command)


def main():
    Surfex_command(sys.argv)


if __name__ == "__main__":
    main()
