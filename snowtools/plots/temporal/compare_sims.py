#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''
Created on 25 mar 2020

@author: lafaysse
'''

import os
import sys

import matplotlib

from optparse import OptionParser

from snowtools.utils.prosimu import prosimu
from snowtools.plots.temporal.chrono import temporalplotSim
from snowtools.utils.dates import checkdateafter, check_and_convert_date
from snowtools.utils.resources import absolute_path

matplotlib.use('Agg')

usage = "compare_sims.py [-b YYYYMMDD] [-e YYYYMMDD] --dirsim=dirsim1,dirsim2 --labels=label1,labe2 --dirplot=dirplot --format=pdf,png,eps --yearly"


def parse_options(arguments):
    parser = OptionParser(usage)


    parser.add_option("-b", "--begin",
                      action="store", type="string", dest="datebegin", default=None,
                      help="First year of extraction")

    parser.add_option("-e", "--end",
                      action="store", type="string", dest="dateend", default=None,
                      help="Last year of extraction")

    parser.add_option("--pro",
                      action="store", dest="pro", default=None,
                      help="List of pro files to compare")

    parser.add_option("--labels",
                      action="store", dest="labels", default=None,
                      help="Labels of simulations in the plot")

    parser.add_option("--var",
                      action="store", dest="var", default=None,
                      help="Name of variable to plot")

    parser.add_option("--dirplot",
                      action="store", dest="dirplot", default=os.getcwd() + "/plot",
                      help="Directory where the figures are saved")

    parser.add_option("--format",
                      action="store", dest="format", default="png",
                      help="Format of plots")

    parser.add_option("--yearly",
                      action="store_true", dest="yearly", default=False,
                      help="Yearly plots")

    (options, args) = parser.parse_args(arguments)
    del args
    return options


def check_and_convert_options(options):

    # Conversions of local paths in absolute paths
    [options.pro, options.dirplot] = \
        list(map(absolute_path, [options.pro, options.dirplot]))

    if type(options.pro) is not list:
        options.pro = [options.pro]

    options.labels = options.labels.split(',')

    # Create plot directory
    if not os.path.isdir(options.dirplot):
        os.makedirs(options.dirplot)

    if options.datebegin and options.dateend:
    # Conversions of dates in datetime objects
        [options.datebegin, options.dateend] = list(map(check_and_convert_date, [options.datebegin, options.dateend]))
        checkdateafter(options.dateend, options.datebegin)

    options.colors = ['green', 'cyan', 'blue', 'red', 'orange', 'black', 'gray']

    return options


if __name__ == "__main__":
    options = parse_options(sys.argv)
    options = check_and_convert_options(options)

    plot = temporalplotSim()

    for i, pro in enumerate(options.pro):
        p=prosimu(pro)
        timeSim = p.readtime()
        varSim = p.read(varname=options.var, selectpoint=0)
        plot.add_line(timeSim, varSim, label=options.labels[i], color=options.colors[i])
        p.close()

    plot.finalize(timeSim)
    plotfilename = options.dirplot + "/" + options.var + "." + options.format
    plot.save(plotfilename, formatout=options.format)




