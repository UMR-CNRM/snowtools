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
from tools.initTG import clim

try:
    from utils.resources import check_snowtools_install
except:
    raise 'Incorrect snowtools installation. Check the documentation.'
check_snowtools_install()
 
# Import snowtools modules
from utils.dates import checkdateafter,check_and_convert_date
from utils.resources import absolute_path,check_surfex_exe
from utils.infomassifs import infomassifs
import tasks.runs

usage = "usage: s2m -b begin_date -e end_date -f forcing [-o path_output] [-w workdir] [-n namelist] [-x date_end_spinup] [-a threshold_1aout] [-r region] [-l list_slopes] [-c nb_classes_aspects] [-L Lower-altitude] [-U Upper-altitude] [-s surfex_exe_directory]"
def exit_usage():
    sys.exit(usage)

def check_and_convert_options(options):
    
    for mandatory in [options.datedeb,options.datefin,options.forcing]:
        if not mandatory:
            print "Missing mandatory option (-b -e -f)"
            exit_usage()
    
    # Controls and type conversions of dates
    [options.datedeb,options.datefin,options.datespinup]=map(check_and_convert_date,[options.datedeb,options.datefin,options.datespinup])        
    checkdateafter(options.datefin,options.datedeb)
    
    # Conversions of local paths in absolute paths    
    [options.forcing,options.namelist,options.diroutput,options.dirwork,options.exesurfex]=\
        map(absolute_path,[options.forcing,options.namelist,options.diroutput,options.dirwork,options.exesurfex])
    
    options.exesurfex=check_surfex_exe(options.exesurfex)
    print options.exesurfex
    
    # Check and conversion of geographical requirements
    if options.region or options.slopes or options.aspects or options.minlevel or options.maxlevel:
        INFOmassifs=infomassifs() 
            
        options.region=INFOmassifs.region2massifs(options.region)
            
        if options.slopes:
            options.slopes=options.slopes.split(",")        
        else:
            options.slopes=["0","20","40"]

        options.aspects=INFOmassifs.get_list_aspect(options.aspects, options.slopes)
         
        options.minlevel,options.maxlevel=INFOmassifs.check_and_convert_min_max_elevation(options.minlevel,options.maxlevel)

    return options

def parse_options(arguments):

    parser = OptionParser(usage)
    
    parser.add_option("-g", action="store_true", dest="ground",default=False)
    
    parser.add_option("-b", "--begin",
                      action="store", type="string", dest="datedeb", default=None,
                      help="Date to start the simulation (YYYYMMDD): MANDATORY OPTION")
    
    parser.add_option("-e", "--end",
                      action="store", type="string", dest="datefin", default=None,
                      help="Date to finish the simulation (YYYYMMDD): MANDATORY OPTION")
    
    parser.add_option("-o", "--output",
                      action="store", type="string", dest="diroutput", default="output",
                      help="name of the output directory - default: output")
    
    parser.add_option("-f", "--forcing",
                      action="store", type="string", dest="forcing", default=None,
                      help="path of the forcing file or of the directory with the forcing files - default: None")
    
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
                      action="store", type="string", dest="namelist", default=os.environ['SNOWTOOLS_CEN']+'/DATA/OPTIONS_V8_NEW_OUTPUTS_NC.nam',
                      help="path of the mother namelist - default: "+os.environ['SNOWTOOLS_CEN']+'/DATA/OPTIONS_V8_NEW_OUTPUTS_NC.nam')
    
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
    
    (options, args) = parser.parse_args(arguments)    
    
    del args
    
    return options

if __name__ == "__main__":

    # Read the options provided by the user
    options=parse_options(sys.argv)

    # Check option values and convert them in types suited for defining a run configuration
    options=check_and_convert_options(options)
    
    if options.ground:
        clim(options.forcing,options.datedeb,options.datefin)
    
    # Define a run object
    if options.region or options.slopes or options.aspects or options.minlevel or options.maxlevel:
        run=tasks.runs.massifrun(options.datedeb,options.datefin,options.forcing,options.diroutput,threshold=options.threshold,
                             dirwork=options.dirwork,datespinup=options.datespinup,
                             execdir=options.exesurfex,
                             geolist=[options.region,options.minlevel,options.maxlevel,options.slopes,options.aspects])
    else:
        run=tasks.runs.surfexrun(options.datedeb,options.datefin,options.forcing,options.diroutput,threshold=options.threshold,
                             dirwork=options.dirwork,datespinup=options.datespinup,
                             execdir=options.exesurfex)

    # Execute the run
    run.run()

