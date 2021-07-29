#!/usr/bin/env python3
# -*- coding: utf-8 -*-
'''
Created on 5 feb. 2019

@author: cluzetb

perform local tests/dev of SODA based on :
    - set of prep files from an openloop
    - opt : set of REAL observations OR generate synthetical observations.

'''
import sys
import time
import os
from optparse import OptionParser

from snowtools.assim.utilcrocO import read_conf
from snowtools.assim.CrocOrun import CrocOrun
usage = 'crocO --opts'


def parse_options(arguments):

    parser = OptionParser(usage)

    parser.add_option("--xpid",
                      type="string", action="store", dest="xpid", default=None,
                      help="xpid of the soda-vortex task ")
    parser.add_option("-d",
                      action = 'store', type = 'string', dest='dates', default = None,
                      help = "specify assimilation dates to try")
    # parser.add_option("--evid",
    #                   action="store", type="string", dest="evid", default="",
    #                   help="xpev (useful for pickle save")
    parser.add_option("--vapp",
                      action="store", type="string", dest="vapp", default='s2m',
                      help="vapp of the soda-vortex task")
    parser.add_option("--vconf",
                      action="store", type="string", dest="vconf", default='12',
                      help="vconf of the soda-vortex task")
    # parser.add_option("--user",
    #                   action='store', type='string', dest='user', default='cluzetb',
    #                   help= 'user where to find the files')
    parser.add_option("--todo",
                      action = 'store', type = 'string', dest='todo', default = None,
                      help= 'type of evaluation')
    parser.add_option("--nmembers",
                      action = 'store', type = int, dest='nmembers', default = 35,
                      help= 'nb members for post proc.')
    parser.add_option("--distr",
                      action = 'store_true', dest = 'distr', default = False,
                      help = 'specify if your simulation is distributed or not')
    parser.add_option("--vars", type = 'string', action="callback", callback=callvars, default = 'all',
                      help="specify analysis variables separated by commas : b1,b.., b7 for MODIS bands, scf, DEP for snowdepth")
    parser.add_option("--fact", type = float, action = 'callback', callback = callvars, default = 1.,
                      help = ' set multiplicative factors for the observation errors')
    parser.add_option("--classesS", type = 'string', action="callback", callback=callvars, default = '0,20,40',
                      help="specify analyzed slope classes ex : 0,20,40")
    parser.add_option("--classesA", type = 'string', action="callback", callback=callvars, default = 'all',
                      help="specify analyzed aspect classes ex : N,NE,E,SE,S,SO")
    parser.add_option("--classesE", type = 'string', action="callback", callback=callvars, default = 'all',
                      help="specify analyzed elev classes ex : 1800,2100,2400,2700")
    parser.add_option("-o",
                      action = 'store', type = 'string', dest='saverep', default = "",
                      help= 'name of the postproc/type_of_anal/ without /')
    parser.add_option("--mpi",
                      action = 'store_true', dest = 'mpi', default = False,
                      help='activate MPI for soda')
    parser.add_option("--sodadistr",
                      action = 'store_true', dest = 'sodadistr', default = False,
                      help='activate distributed assim instead of semi-distr.')
    parser.add_option('--synth',
                      action = 'store', dest = 'synth', type = int, default = None,
                      help ='choose member as a synthetic observation. TODO : 0 for random.')
    parser.add_option('--sensor',
                      action = 'store', dest = 'sensor', type = 'string', default = None,
                      help ='provide sensor name for OBS reading (MODIS, pleiades...).')
    (options, args) = parser.parse_args(arguments)

    del args

    return options


def callvars(option, opt, value, parser):
    setattr(parser.values, option.dest, value.split(','))


def execute(args):
    options = parse_options(args)
    if 'VORTEXPATH' not in os.environ.keys():
        raise Exception('you must export VORTEXPATH to the root of your local vortex xps.')
    else:
        options.vortexpath = os.environ['VORTEXPATH']
    rootdir = options.vortexpath + '/' + options.vapp + '/' + options.vconf + '/'
    conf = read_conf(rootdir + options.xpid + '/conf/' + options.vapp + '_' + options.vconf + '.ini')
    run = CrocOrun(options, conf)
    if options.todo =='corr':
        pass
    else:
        run.run()
    if options.todo is not None:
        run.post_proc(options)

if __name__ == '__main__':
    start_time = time.time()
    execute(sys.argv)
    elapsed_time = time.time() - start_time
    print(elapsed_time)
