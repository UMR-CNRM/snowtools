'''
Created on 20 mars 2018

@author: lafaysse
'''
from optparse import OptionParser
import os
import sys

list_sites = ["cdp", "oas", "obs", "ojp", "rme", "sap", "snb", "sod", "swa", "wfj"]

bdate = dict(cdp="1994100101",
             oas="1997100107",
             obs="1997100107",
             ojp="1997100107",
             rme="1988100108",
             sap="2005093016",
             snb="2005100107",
             sod="2007100101",
             swa="2005100107",
             wfj="1996090101"
             )

edate = dict(cdp="2014100100",
             oas="2010100106",
             obs="2010100106",
             ojp="2010100106",
             rme="2008100107",
             sap="2015093015",
             snb="2015100106",
             sod="2014100100",
             swa="2015100106",
             wfj="2016090100"
             )

usage = "usage: python ESM_snowmip.py --site=xxx"


def parse_options(arguments):
    parser = OptionParser(usage)

    parser.add_option("--site",
                      action="store", type="string", dest="site", default=None,
                      help="3 letters of ESM-SnowMIP site")

    parser.add_option("--action",
                      action="store", type="string", dest="action", default=None,
                      help="spinup, run, scores")

    parser.add_option("--escroc",
                      action="store", type="string", dest="escroc", default=None,
                      help="E1, E2")

    (options, args) = parser.parse_args(arguments)
    del args
    return options


if __name__ == '__main__':

    options = parse_options(sys.argv)

    if options.action == "spinup":
        commande = "python $SNOWTOOLS_CEN/tasks/s2m_command.py -g -o " + "SPINUP_" + options.site + " -f /home/lafaysse/data/ESM-SnowMIP/" + options.site + "/meteo -b " + bdate[options.site] + " -e " + edate[options.site] + " -n /home/lafaysse/data/ESM-SnowMIP/OPTIONS_" + options.site + ".nam -s /home/lafaysse/SURFEX/lafaysse_fromV8trunk_withmeb/exe"

    elif options.action == "run":
        if options.escroc == "E1":
            nmembers = 7776
            nnodes = 15
            namelist = "ESM-SnowMIP/OPTIONS_" + options.site + ".nam"
        elif options.escroc == "E2":
            nmembers = 35
            nnodes = 1
            namelist = "ESM-SnowMIP/OPTIONS_" + options.site + ".nam"
        elif options.escroc == "E2CLEAR":
            nmembers = 35
            nnodes = 1
            namelist = "ESM-SnowMIP/OPTIONS_CLEAR" + options.site + ".nam"
        elif options.escroc == "Crocus":
            nmembers = 1
            nnodes = 1
            
        commande = "python $SNOWTOOLS_CEN/tasks/s2m_command.py --escroc=" + options.escroc + " --nnodes=" + str(nnodes) + " -o " + options.escroc + " --nmembers=" + str(nmembers) + " -m ESM-SnowMIP -f meteo -r " + options.site + " -b " + bdate[options.site] + " -e " + edate[options.site] + " -x " + edate[options.site] + " -n ESM-SnowMIP/OPTIONS_" + options.site + ".nam -s /home/cnrm_other/cen/mrns/lafaysse/SURFEX/lafaysse_fromV8trunk_withmeb/exe_hybride"

    elif options.action == "scores":
        if options.escroc == "E1":
            nmembers = 7776
        elif options.escroc == "E2":
            nmembers = 35            
        elif options.escroc == "Crocus":
            nmembers = 1        
        commande = "python $SNOWTOOLS_CEN/tasks/s2m_command.py --scores --escroc=" + options.escroc + " --nnodes=1 -o " + options.escroc + " --nmembers=" + str(nmembers) + " -m ESM-SnowMIP -f meteo -r " + options.site + " -b " + bdate[options.site] + " -e " + edate[options.site] + " -s /home/cnrm_other/cen/mrns/lafaysse/SURFEX/lafaysse_fromV8trunk_withmeb/exe_hybride"

    else:
        print "No action to do."
        sys.exit(0)
    print commande
    os.system(commande)
