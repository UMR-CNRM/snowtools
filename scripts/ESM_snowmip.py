'''
Created on 20 mars 2018

@author: lafaysse
'''
from optparse import OptionParser
from tools.change_forcing import forcinput_ESMSnowMIP
from .post_snowmip import ESMSnowMIP_output
import os
import sys
import shutil

dirsnowmip = os.environ['HOME'] + "/data/ESM-SnowMIP/"
dirnamelist = dirsnowmip + "namelists_8.0/"
direxe = os.environ['HOME'] + "/SURFEX/8_0/exe"
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

usage = "usage: python ESM_snowmip.py --site=xxx --source=xxx"


def parse_options(arguments):
    parser = OptionParser(usage)

    parser.add_option("--site",
                      action="store", type="string", dest="site", default=None,
                      help="3 letters of ESM-SnowMIP site")

    parser.add_option("--source",
                      action="store", type="string", dest="source", default=None,
                      help="insitu or gswp3c")

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

    list_sites = options.site.split(",")

    for site in list_sites:

        if options.source == "insitu":
            begindate = bdate[site]
            enddate = edate[site]
        elif options.source == "gswp3c":
            begindate = "1980100100"
            enddate = "2010093023"

        dirforcing = dirsnowmip + "/" + site + "/" + options.source
        if not os.path.isdir(dirforcing):
            os.mkdir(dirforcing)
        forcing = dirforcing + "/FORCING_" + begindate + "_" + enddate + ".nc"

        if "forcing" in options.action:
            met = dirsnowmip + "met_" + options.source + "_" + site + "_" + begindate[0:4] + "_" + enddate[0:4] + ".nc"
            forcinput_ESMSnowMIP(met, forcing)

        if "spinup" in options.action:
            commande = "python $SNOWTOOLS_CEN/tasks/s2m_command.py -g -o " + dirsnowmip + "SPINUP_" + site + "_" + options.source + " -f " + dirforcing + " -b " + begindate + " -e " + enddate + " -n " + dirnamelist + "/OPTIONS_" + site + ".nam -s " + direxe
            print(commande)
            os.system(commande)

        if "runpc" in options.action:
            dirprep = dirsnowmip + "Crocus_" + site + "_" + options.source + "/prep/"
            dirprepspinup = dirsnowmip + "SPINUP_" + site + "_" + options.source + "/prep/"
            if not os.path.isdir(dirprep):
                os.makedirs(dirprep)
            for fichier in ["PGD.nc", "PREP_" + enddate + ".nc"]:
                shutil.copy(dirprepspinup + fichier, dirprep + fichier)
            commande = "python $SNOWTOOLS_CEN/tasks/s2m_command.py -o  " + dirsnowmip + "Crocus_" + site + "_" + options.source  + " -f " + dirforcing + " -x " + enddate + " -b " + begindate + " -e " + enddate + " -n " + dirnamelist + "/OPTIONS_" + site + ".nam -s " + direxe
            print(commande)
            os.system(commande)
        elif options.action == "runbeaufix":
            if options.escroc == "E1":
                nmembers = 7776
                nnodes = 15
                namelist = "ESM-SnowMIP/OPTIONS_" + site + ".nam"
            elif options.escroc == "E2":
                nmembers = 35
                nnodes = 1
                namelist = "ESM-SnowMIP/OPTIONS_" + site + ".nam"
            elif options.escroc == "E2CLEAR":
                nmembers = 35
                nnodes = 1
                namelist = "ESM-SnowMIP/OPTIONS_CLEAR" + site + ".nam"
            elif options.escroc == "Crocus":
                nmembers = 1
                nnodes = 1

            commande = "python $SNOWTOOLS_CEN/tasks/s2m_command.py --escroc=" + options.escroc + " --nnodes=" + str(nnodes) + " -o " + options.escroc + " --nmembers=" + str(nmembers) + " -m ESM-SnowMIP -f meteo -r " + site + " -b " + begindate + " -e " + enddate + " -x " + enddate + " -n ESM-SnowMIP/OPTIONS_" + site + ".nam -s /home/cnrm_other/cen/mrns/lafaysse/SURFEX/lafaysse_fromV8trunk_withmeb/exe_hybride"
            print(commande)
            os.system(commande)
        elif options.action == "scores":
            if options.escroc == "E1":
                nmembers = 7776
            elif options.escroc == "E2":
                nmembers = 35
            elif options.escroc == "Crocus":
                nmembers = 1
            commande = "python $SNOWTOOLS_CEN/tasks/s2m_command.py --scores --escroc=" + options.escroc + " --nnodes=1 -o " + options.escroc + " --nmembers=" + str(nmembers) + " -m ESM-SnowMIP -f meteo -r " + site + " -b " + begindate + " -e " + enddate + " -s /home/cnrm_other/cen/mrns/lafaysse/SURFEX/lafaysse_fromV8trunk_withmeb/exe_hybride"
            print(commande)
            os.system(commande)

        else:
            print("No run to do.")
            sys.exit(0)
            print(commande)
            os.system(commande)

        if "post" in options.action:
            pro = dirsnowmip + "Crocus_" + site + "_" + options.source + "/pro/PRO_" + begindate + "_" + enddate + ".nc"
            final = dirsnowmip + "Crocus_REF_" + options.source + "_" + site + "_" + begindate[0:4] + "_" + enddate[0:4] + ".nc"
            ESMSnowMIP_output(pro, final)
