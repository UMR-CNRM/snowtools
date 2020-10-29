#!/usr/bin/env python
# -*- coding: utf-8 -*-

'''
Created on 30 Aug. 2017

@author: lafaysse
'''

# General python modules
from optparse import OptionParser
import os
import sys
import datetime

# Import snowtools modules
from tools.initTG import clim
from utils.dates import checkdateafter, check_and_convert_date
from utils.resources import absolute_path, check_surfex_exe
from utils.infomassifs import infomassifs
import tasks.runs
from tasks.vortex_kitchen import vortex_kitchen
from tasks.crampon_vortex_kitchen import crampon_vortex_kitchen
from tasks.s2m_launcher import _S2M_command

usage = "usage: s2m -b begin_date -e end_date -f forcing [-m forcingmodel] [-o path_output] [-w workdir] [-n namelist] [-x date_end_spinup] [-a threshold_1aout] [-r region] [-l list_slopes] [-c nb_classes_aspects] [-L Lower-altitude] [-U Upper-altitude] [-s surfex_exe_directory]"


class Surfex_command(_S2M_command):
    """class for SURFEX experiments launching commands"""

    def execute(self):
        machine = os.uname()[1]
        if "beaufix" in machine or "prolix" in machine or "epona" in machine or "belenos" in machine:
            self.execute_through_vortex()
        else:
            self.execute_without_vortex()

    def check_and_convert_options(self, vortex=False):

        if self.options.oper:
            self.check_mandatory_arguments(**{'-r': 'region'})

            if self.options.datedeb:
                self.options.datedeb = check_and_convert_date(self.options.datedeb)
            else:
                # If rundate is not prescribed, get it from the current time.
                self.options.datedeb = self.set_default_date()

        else:
            self.check_mandatory_arguments(**{'-b': 'datedeb', '-e': 'datefin', '-f': 'forcing'})

            if not self.options.onlyextractforcing:
                self.options.exesurfex = check_surfex_exe(self.options.exesurfex)
                print(self.options.exesurfex)

            # Controls and type conversions of dates
            [self.options.datedeb, self.options.datefin, self.options.datespinup] = list(map(check_and_convert_date, [self.options.datedeb, self.options.datefin, self.options.datespinup]))
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
        elif self.options.region or self.options.slopes or self.options.aspects or self.options.minlevel or self.options.maxlevel:
            INFOmassifs = infomassifs()

            if not vortex:
                self.options.region = INFOmassifs.region2massifs(self.options.region)

            if self.options.slopes:
                self.options.slopes = self.options.slopes.split(",")
            else:
                self.options.slopes = ["0", "20", "40"]

            self.options.aspects = INFOmassifs.get_list_aspect(self.options.aspects, self.options.slopes)
            self.options.minlevel, self.options.maxlevel = INFOmassifs.check_and_convert_min_max_elevation(self.options.minlevel, self.options.maxlevel)

    def set_path(self, vortex):
        # Conversions of local paths in absolute paths
        [self.options.namelist, self.options.workdir, self.options.exesurfex] = \
            list(map(absolute_path, [self.options.namelist, self.options.workdir, self.options.exesurfex]))

        if not vortex:
            [self.options.forcing, self.options.diroutput] = \
                list(map(absolute_path, [self.options.forcing, self.options.diroutput]))

    def set_default_date(self):
        today = datetime.datetime.today()
        newhour = today.hour - today.hour % 3
        return today.replace(hour=newhour, minute=0, second=0, microsecond=0)

    def parse_options(self, arguments):

        parser = OptionParser(usage)

        parser.add_option("-g", action="store_true", dest="ground", default=False)
        parser.add_option("-G", action="store_true", dest="groundonly", default=False)

        parser.add_option("-b", "--begin",
                          action="store", type="string", dest="datedeb", default=None,
                          help="Date to start the simulation (YYYYMMDD): MANDATORY OPTION")

        parser.add_option("-e", "--end",
                          action="store", type="string", dest="datefin", default=None,
                          help="Date to finish the simulation (YYYYMMDD): MANDATORY OPTION (unless --oper)")

        parser.add_option("-o", "--output",
                          action="store", type="string", dest="diroutput", default="output",
                          help="name of the output directory - default: output")

        parser.add_option("-f", "--forcing",
                          action="store", type="string", dest="forcing", default=None,
                          help="path of the forcing file or of the directory with the forcing files - default: None")

        parser.add_option("-m", "--model",
                          action="store", type="string", dest="model", default=None,
                          help="meteorological model used as forcing")

        parser.add_option("-x", "--spinupdate",
                          action="store", type="string", dest="datespinup", default=None,
                          help="path of the directory with the spinup file - default: None")

        parser.add_option("-l", "--list_slopes",
                          action="store", type="string", dest="slopes", default=None,
                          help="path of the file with the list of physical options - default: None")

        parser.add_option("-r", "--region",
                          action="store", type="string", dest="region", default=None,
                          help="path of the file with the list of physical options - default: alpes")

        parser.add_option("-n", "--namelist",
                          action="store", type="string", dest="namelist", default=os.environ['SNOWTOOLS_CEN'] + '/DATA/OPTIONS_V8.1_NEW_OUTPUTS_NC.nam',
                          help="path of the mother namelist - default: " + os.environ['SNOWTOOLS_CEN'] + '/DATA/OPTIONS_V8.1_NEW_OUTPUTS_NC.nam')

        parser.add_option("-s", "--surfexexec",
                          action="store", type="string", dest="exesurfex", default=None,
                          help="path of the mother namelist - default: $EXESURFEX")

        parser.add_option("-w", "--workdir",
                          action="store", type="string", dest="workdir", default=None,
                          help="name of the output directory - default: output")

        parser.add_option("-a", "--august_threshold",
                          action="store", type="int", dest="threshold", default=-999,
                          help="name of the output directory - default: -999")

        parser.add_option("-L", "--lowest",
                          action="store", type="int", dest="minlevel", default=None,
                          help="name of the output directory - default: None")

        parser.add_option("-U", "--upper",
                          action="store", type="int", dest="maxlevel", default=None,
                          help="name of the output directory - default: None")

        parser.add_option("-c", "--classes_aspect",
                          action="store", type="int", dest="aspects", default=None,
                          help="name of the output directory - default: None")

        parser.add_option("-E", "--extractforcing",
                          action="store_true", dest="onlyextractforcing", default=False,
                          help="only extract meteorological forcing - default: False")

        parser.add_option("--addmask",
                          action="store_true", dest="addmask", default=False,
                          help="apply shadows on solar radiation from surrounding masks")

        parser.add_option("--oper",
                          action="store_true", dest="oper", default=False,
                          help="Operational chain")

        parser.add_option("--forecast",
                          action="store_true", dest="forecast", default=False,
                          help="To separate analysis and forecast modes")

        parser.add_option("--reinit",
                          action="store_true", dest="reinit", default=False,
                          help="To reinitialize operational analyses with a reanalysis")

        parser.add_option("--monthlyreanalysis",
                          action="store_true", dest="monthlyreanalysis", default=False,
                          help="Run monthly reanalysis")

        parser.add_option("--dailyprep",
                          action="store_true", dest="dailyprep", default=False,
                          help="Split reanalysis day by day to prepare initial conditions for reforecast")

        parser.add_option("--crampon",
                          action="store", type='string', dest="crampon", default=None,
                          help="CRAMPON assimilation sequence activation and ABSOLUTE path to conf (assimdates (file)")

        parser.add_option("--nforcing",
                          action="store", type="int", dest="nforcing", default=1,
                          help="Number of members of forcing files")

        parser.add_option("--cramponmonthly",
                          action="store_true", dest="cramponmonthly", default=False,
                          help="activation of CRAMPON with monthly forcing files (not possible yet)." )

        parser.add_option("--grid",
                          action="store_true", dest="gridsimul", default=False,
                          help="This is a gridded simulation as defined in the namelist - default: False")

        parser.add_option("--escroc",
                          action="store", type="string", dest="escroc", default=None,
                          help="ESCROC subensemble")

        parser.add_option("--scores",
                          action="store_true", dest="scores", default=False,
                          help="ESCROC scores")

        parser.add_option("--nmembers",
                          action="store", type="int", dest="nmembers", default=None,
                          help="Number of members")

        parser.add_option("--startmember",
                          action="store", type="int", dest="startmember", default=None,
                          help="Number of first member")

        parser.add_option("--nnodes",
                          action="store", type="int", dest="nnodes", default=1,
                          help="Number of nodes")

        parser.add_option("--soda",
                          action="store", type='string', dest="soda", default=None,
                          help="ESCROC-SODA assimilation sequence activation and ABSOLUTE path to conf (assimdates (file)")

        parser.add_option("--sodamonthly",
                          action="store_true", dest="sodamonthly", default=False,
                          help="activation of SODA with monthly forcing files" )

        parser.add_option("--openloop",
                          action="store_true", dest="openloop", default=False,
                          help="OFFLINE inout at assimdates without assim")

        parser.add_option("--walltime",
                          action="store", type = "string", dest="walltime", default = None,
                          help="specify your job walltime (format hh:mm:ss)")

        parser.add_option("--writesx",
                          action="store", type = "str", dest="writesx", default = None,
                          help="specify the root path (.../vortex) where you'd like to store PREP files on sxcen")

        parser.add_option("--sensor",
                          action="store", type = "str", dest="sensor", default = "MODIS",
                          help="specify the sensor name of the obs you want to assimilate")

        parser.add_option("--ntasks",
                          action="store", type="int", dest="ntasks", default=None,
                          help="Number of tasks (and procs) per node.")

        parser.add_option("--debug",
                          action="store_true", dest="debug", default=False,
                          help="Debug task with files available on server")

        (options, args) = parser.parse_args(arguments)

        del args

        options.surfex = True
        options.safran = False

        return options

    def exit_usage(self):
        sys.exit(usage)

    def execute_without_vortex(self):

        # Check option values and convert them in types suited for defining a run configuration
        self.check_and_convert_options(vortex=False)
        self.check_mandatory_arguments(**{'-o': 'diroutput'})

        if self.options.ground or self.options.groundonly:
            clim(self.options)

        if not self.options.groundonly:

            # Define a run object
            if type(self.options.forcing) is list or self.options.addmask:
                run = tasks.runs.postesrun(self.options.datedeb, self.options.datefin, self.options.forcing, self.options.diroutput, threshold=self.options.threshold,
                                           workdir=self.options.workdir, datespinup=self.options.datespinup,
                                           execdir=self.options.exesurfex,
                                           namelist=self.options.namelist,
                                           addmask=True)
            elif self.interpol:
                run = tasks.runs.interpolrun(self.options.datedeb, self.options.datefin, self.options.forcing, self.options.diroutput, threshold=self.options.threshold,
                                             workdir=self.options.workdir, datespinup=self.options.datespinup, geolist=[self.options.region],
                                             execdir=self.options.exesurfex,
                                             namelist=self.options.namelist,
                                             addmask=True,
                                             onlyextractforcing=self.options.onlyextractforcing)
            elif self.options.region or self.options.slopes or self.options.aspects or self.options.minlevel or self.options.maxlevel:

                if self.options.onlyextractforcing:
                    if 'pro' in self.options.forcing or 'PRO' in self.options.forcing:
                        run = tasks.runs.massifextractpro(self.options.datedeb, self.options.datefin, self.options.forcing, self.options.diroutput,
                                                          workdir=self.options.workdir,
                                                          geolist=[self.options.region, self.options.minlevel, self.options.maxlevel, self.options.slopes, self.options.aspects])
                    else:
                        run = tasks.runs.massifextractforcing(self.options.datedeb, self.options.datefin, self.options.forcing, self.options.diroutput,
                                                              workdir=self.options.workdir,
                                                              geolist=[self.options.region, self.options.minlevel, self.options.maxlevel, self.options.slopes, self.options.aspects])
                else:

                    run = tasks.runs.massifrun(self.options.datedeb, self.options.datefin, self.options.forcing, self.options.diroutput, threshold=self.options.threshold,
                                               workdir=self.options.workdir, datespinup=self.options.datespinup,
                                               execdir=self.options.exesurfex,
                                               namelist=self.options.namelist,
                                               geolist=[self.options.region, self.options.minlevel, self.options.maxlevel, self.options.slopes, self.options.aspects])
            else:
                if self.options.gridsimul:
                    run = tasks.runs.griddedrun(self.options.datedeb, self.options.datefin, self.options.forcing, self.options.diroutput, threshold=self.options.threshold,
                                                workdir=self.options.workdir, datespinup=self.options.datespinup,
                                                execdir=self.options.exesurfex,
                                                namelist=self.options.namelist)
                else:
                    run = tasks.runs.surfexrun(self.options.datedeb, self.options.datefin, self.options.forcing, self.options.diroutput, threshold=self.options.threshold,
                                               workdir=self.options.workdir, datespinup=self.options.datespinup,
                                               execdir=self.options.exesurfex,
                                               namelist=self.options.namelist)

            # Execute the run
            run.run()

    def execute_through_vortex(self):

        # Check option values and convert them in types suited for defining a run configuration
        self.check_and_convert_options(vortex=True)

        if not self.options.workdir:
            if 'WORKDIR' in list(os.environ.keys()):
                self.options.workdir = os.environ['WORKDIR']
            else:
                self.options.workdir = "."

        # Cook vortex task
        if not self.options.soda:
            vortex_kitchen(self.options)
#        elif self.options.escroc:
#            run = vortex_kitchen_soda(self.options)
#            run.run(self.options)
        else:
            # Cook vortex task
            if not self.options.crampon:
                run = vortex_kitchen(self.options)
                run.run(self.options)
            elif self.options.escroc:
                run = crampon_vortex_kitchen(self.options)
                run.run(self.options)
            else:
                print ("soda should run with escroc option")


if __name__ == "__main__":

    Surfex_command(sys.argv)
