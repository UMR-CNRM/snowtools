#! /usr/bin/python
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

try:
    from utils.resources import check_snowtools_install
    from utils.resources import InstallException
    print (os.environ["SNOWTOOLS_CEN"])
    check_snowtools_install()
    print('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
    print('Snowtools installation has been successfully checked.')
    print('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
except ImportError or InstallException:
    print('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
    print('Incorrect snowtools installation. Check the documentation.')
    print('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
    raise

# Import snowtools modules
from tools.initTG import clim
from utils.dates import checkdateafter, check_and_convert_date
from utils.resources import absolute_path, check_surfex_exe
from utils.infomassifs import infomassifs
import tasks.runs
from tasks.vortex_kitchen import vortex_kitchen
from tasks.vortex_kitchen_soda import vortex_kitchen_soda

usage = "usage: s2m -b begin_date -e end_date -f forcing [-m forcingmodel] [-o path_output] [-w workdir] [-n namelist] [-x date_end_spinup] [-a threshold_1aout] [-r region] [-l list_slopes] [-c nb_classes_aspects] [-L Lower-altitude] [-U Upper-altitude] [-s surfex_exe_directory]"


def exit_usage():
    sys.exit(usage)


def check_and_convert_options(options, vortex=False):

    if options.oper:
        list_mandatory = [options.region]
        list_print = "-r"
    else:
        list_mandatory = [options.datedeb, options.datefin, options.forcing]
        list_print = "-b -e -f"

        if vortex:
            list_mandatory.extend([options.model, options.region])
            list_print += " -m -r"

    for mandatory in list_mandatory:
        if not mandatory:
            print("Missing mandatory option: " + list_print)
            exit_usage()

    if options.oper:
        if options.datedeb:
            options.datedeb = check_and_convert_date(options.datedeb)
        else:
            # If rundate is not prescribed, get it from the current time.
            options.datedeb = datetime.datetime.today().replace(minute=0, second=0, microsecond=0)
    else:
        # Controls and type conversions of dates
        [options.datedeb, options.datefin, options.datespinup] = list(map(check_and_convert_date, [options.datedeb, options.datefin, options.datespinup]))
        checkdateafter(options.datefin, options.datedeb)

    # Conversions of local paths in absolute paths
    [options.namelist, options.dirwork, options.exesurfex] = \
        list(map(absolute_path, [options.namelist, options.dirwork, options.exesurfex]))

    if not vortex:
        [options.forcing, options.diroutput] = \
            list(map(absolute_path, [options.forcing, options.diroutput]))

    if not options.oper:
        options.exesurfex = check_surfex_exe(options.exesurfex)
        print(options.exesurfex)

    # Check and conversion of geographical requirements
    if options.region or options.slopes or options.aspects or options.minlevel or options.maxlevel:
        INFOmassifs = infomassifs()

        if not vortex:
            options.region = INFOmassifs.region2massifs(options.region)

        if options.slopes:
            options.slopes = options.slopes.split(",")
        else:
            options.slopes = ["0", "20", "40"]

        options.aspects = INFOmassifs.get_list_aspect(options.aspects, options.slopes)

        options.minlevel, options.maxlevel = INFOmassifs.check_and_convert_min_max_elevation(options.minlevel, options.maxlevel)

    return options


def parse_options(arguments):

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
                      action="store", type="string", dest="dirwork", default=None,
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
    parser.add_option("--nforcing",
                      action="store", type="int", dest="nforcing", default=1,
                      help="Number of members of forcing files")
    parser.add_option("--sodamonthly",
                      action="store_true", dest="sodamonthly", default=False,
                      help="activation of SODA with monthly forcing files" )

    parser.add_option("--openloop",
                      action="store_true", dest="openloop", default=False,
                      help="OFFLINE inout at assimdates without assim")

    parser.add_option("--walltime",
                      action="store", type = "int", dest="walltime", default = None,
                      help="specify your job walltime (useful for debugging on sev. nodes)")

    parser.add_option("--writesx",
                      action="store", type = "str", dest="writesx", default = None,
                      help="specify the root path (.../vortex) where you'd like to store PREP files on sxcen")

    parser.add_option("--sensor",
                      action="store", type = "str", dest="sensor", default = "MODIS",
                      help="specify the sensor name of the obs you want to assimilate")

    (options, args) = parser.parse_args(arguments)

    del args

    return options


def execute(args):

    # Read the options provided by the user
    options = parse_options(args)

    # Check option values and convert them in types suited for defining a run configuration
    options = check_and_convert_options(options)

    if options.ground or options.groundonly:
        clim(options)

    if not options.groundonly:

        # Define a run object
        if type(options.forcing) is list or options.addmask:
            run = tasks.runs.postesrun(options.datedeb, options.datefin, options.forcing, options.diroutput, threshold=options.threshold,
                                       dirwork=options.dirwork, datespinup=options.datespinup,
                                       execdir=options.exesurfex,
                                       namelist=options.namelist,
                                       addmask=True)

        elif options.region or options.slopes or options.aspects or options.minlevel or options.maxlevel:

            if options.onlyextractforcing:
                run = tasks.runs.massifextractforcing(options.datedeb, options.datefin, options.forcing, options.diroutput,
                                                      dirwork=options.dirwork,
                                                      geolist=[options.region, options.minlevel, options.maxlevel, options.slopes, options.aspects])
            else:

                run = tasks.runs.massifrun(options.datedeb, options.datefin, options.forcing, options.diroutput, threshold=options.threshold,
                                           dirwork=options.dirwork, datespinup=options.datespinup,
                                           execdir=options.exesurfex,
                                           namelist=options.namelist,
                                           geolist=[options.region, options.minlevel, options.maxlevel, options.slopes, options.aspects])
        else:
            if options.gridsimul:
                run = tasks.runs.griddedrun(options.datedeb, options.datefin, options.forcing, options.diroutput, threshold=options.threshold,
                                            dirwork=options.dirwork, datespinup=options.datespinup,
                                            execdir=options.exesurfex,
                                            namelist=options.namelist)
            else:
                run = tasks.runs.surfexrun(options.datedeb, options.datefin, options.forcing, options.diroutput, threshold=options.threshold,
                                           dirwork=options.dirwork, datespinup=options.datespinup,
                                           execdir=options.exesurfex,
                                           namelist=options.namelist)

        # Execute the run
        run.run()


def execute_through_vortex(args):

    # Read the options provided by the user
    options = parse_options(args)

    # Check option values and convert them in types suited for defining a run configuration
    options = check_and_convert_options(options, vortex=True)

    if not options.dirwork:
        if 'WORKDIR' in list(os.environ.keys()):
            options.dirwork = os.environ['WORKDIR']
        else:
            options.dirwork = "."

    # Cook vortex task
    if not options.soda:
        run = vortex_kitchen(options)
        run.run(options)
    elif options.escroc:
        run = vortex_kitchen_soda(options)
        run.run(options)
    else:
        print ("soda should run with escroc option")


if __name__ == "__main__":

    machine = os.uname()[1]

    if "beaufix" in machine or "prolix" in machine:
        execute_through_vortex(sys.argv)
    else:
        execute(sys.argv)
