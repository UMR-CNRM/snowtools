#!/usr/bin/env/python3
# -*- coding: utf-8 -*-
'''
Created on 12 févr. 2020

@author: cluzetb & deschampsbc

Generate perturbed forcings on beaufix and transfer them in order to save time by parallelizing.
Two options:
1. (preferred): launch on beaufix via submit_jobs_gener_opert_forcings_beaufix.py. files are automatically tranferred to hendrix.
2. (optional): launch this script locally and manually transfer the files to hendrix via ftp.

/!\ do not launch this script directly on beaufix ! this job must be submitted to sbatch via submit_job_gener_pert_forcings.py
 from :
  - initial forcing file
  - .txt/.csv file with statistical values of perturbations for each variable (param.txt as an example)
   * sigma is without units for multiplicative noises
   * set sigma to 0 to deactivate perturbations (e.g. for impurities)
   * fsys column is optional, by default fsys is set to 0 and 1 for additive and multiplicative noises respectively.

 Adapted from Charrois/Revuelto's


'''
import csv
import multiprocessing
import os
import shutil
import argparse
import netCDF4  # @UnresolvedImport
import numpy as np
from snowtools.tools.makeForcingEnsemble import addNoiseMultiplicative, addNoiseAdditive, convertPrecipPhase, addNoise2Impur


class PertParser(object):
    def __init__(self):
        self.argparser = argparse.ArgumentParser(description = "Forcing perturbation on beaufix. Copy output in cache and on hendrix")
        self.argparser.add_argument("-param", dest = "param", default = os.environ['SNOWTOOLS_CEN'] + '/tools/param.txt')
        self.argparser.add_argument("-inFOR", dest = "inFOR", help = " path to reference forcing")
        self.argparser.add_argument("-Exp", dest = "Exp", help = "xpid out")
        self.argparser.add_argument("-nmembers", dest = "nmembers", type=int)
        self.argparser.add_argument("-cacheBeauf", dest = "cacheBeauf", help = 'cache on beaufix (/scratch/work/<username>/cache/vortex/safran/geom/')
        self.argparser.add_argument("-rootHend", dest = "rootHend")
        self.argparser.add_argument("-brutalImp", dest = 'brutalImp', type = bool, default = False,
                                    help = " activate brutal perturbation of the impurities")
        self.argparser.add_argument("-walltime_transfer", dest = 'walltime_transfer', help= 'HH:MM:SS format', default = '01:00:00')

    def parse(self):
        return self.argparser.parse_args()


def workerPerturb(largs):
    '''
    each worker is perturbing 1 forcing member
    '''
    # unpack args
    np.random.seed()
    f = largs[0]
    po = largs[1]
    numMember = largs[2]
    o = largs[3]
    brutal = largs[4]
    print('Start generating forcing ensemble')
    print(' Reference forcing : ' + f)

    if not os.path.exists(o):  # creates o
        os.mkdir(o)
    if o.endswith('/'):  # format o
        o = o[: - 1]
    nL = 4  # length of the digit appended to the folder name

    # read parameters: sigma and tau for each disturbed variable from a csv file
    param = {}
    with open(po, mode = 'r') as csv_file:
        csv_reader = csv.DictReader(csv_file)
        print('Param value')
        for row in csv_reader:
            # fsys is an optional argument. if not prescribed, set to 0 for additive perts, and 1 for multiplicative
            if 'fsys' in row.keys():
                fsysparam = float(row['fsys'])
            else:
                if row['varName'] in ['Tair', 'Lwdown']:
                    fsysparam = 0
                else:
                    fsysparam = 1
            param[row['varName']] = [float(row['std']), float(row['tau']), fsysparam ]
            print(str(row['varName']) + ' | std : ' + str(param[row['varName']][0]) + ' | tau : ' + str(param[row['varName']][1]) + ' | bias factor : ' + str(param[row['varName']][2]))

    bn = os.path.basename(f)

    # generate 1 forcing
    outN = str(numMember).zfill(nL)
    print('Generating forcing number : ' + outN)

    oMb = o + '/mb' + str(outN)
    if not os.path.exists(oMb):
        os.mkdir(oMb)
        if not os.path.exists(oMb + '/meteo'):
            os.mkdir(oMb + '/meteo')

    outFOR = oMb + '/meteo/' + bn
    print(outFOR)
    if os.path.exists(outFOR):
        os.remove(outFOR)
    shutil.copyfile( f, outFOR )

    # generate perturbed forcing
    FORCING = netCDF4.Dataset(outFOR, 'a')
    t = FORCING.variables['time']
    dt = float(t[1] - t[0])
    semiDistrib = len(np.shape(FORCING.variables['Tair'])) == 2  # test the number of dims
    # Disturb Tair
    if param['Tair'][0] != 0:
        var = FORCING.variables['Tair'][:]
        FORCING.variables['Tair'][:] = addNoiseAdditive( var, param['Tair'][0], param['Tair'][1], dt, fsys = param['Tair'][2], semiDistrib = semiDistrib)

    # Disturb Snowf and Rainf ! must come after Tair perturbation !
    if param['Precip'][0] != 0:
        Precip = FORCING.variables['Snowf'][:] + FORCING.variables['Rainf'][:]
        Precip = addNoiseMultiplicative( Precip, param['Precip'][0], param['Precip'][1], dt, fsys = param['Precip'][2], semiDistrib = semiDistrib)
        Tair = FORCING.variables['Tair'][:]
        FORCING.variables['Rainf'][:], FORCING.variables['Snowf'][:] = convertPrecipPhase( Tair, Precip, Tmin = 272.65, Tmax = 274.05)

    # Disturb SWdown
    if param['DIR_SWdown'][0] != 0:
        var = FORCING.variables['DIR_SWdown'][:]
        FORCING.variables['DIR_SWdown'][:] = addNoiseMultiplicative( var, param['DIR_SWdown'][0], param['DIR_SWdown'][1], dt, fsys = param['DIR_SWdown'][2], semiDistrib = semiDistrib)

    # Disturb Wind
    if param['Wind'][0] != 0:
        var = FORCING.variables['Wind'][:]
        FORCING.variables['Wind'][:] = addNoiseMultiplicative( var, param['Wind'][0], param['Wind'][1], dt, fsys = param['Wind'][2], semiDistrib = semiDistrib)

    # Disturb LWdown
    if param['LWdown'][0] != 0:
        var = FORCING.variables['LWdown'][:]
        FORCING.variables['LWdown'][:] = addNoiseAdditive( var, param['LWdown'][0], param['LWdown'][1], dt, fsys = param['LWdown'][2], semiDistrib = semiDistrib)

    # Disturb IMPWET1
    if param['IMPWET1'][0] != 0:
        var = FORCING.variables['IMPWET1'][:]
        FORCING.variables['IMPWET1'][:] = addNoise2Impur(var, 'IMPWET1', param['IMPWET1'][0], param['IMPWET1'][1], dt, semiDistrib = semiDistrib, brutal=brutal)

    # Disturb IMPWET2
    if param['IMPWET2'][0] != 0:
        var = FORCING.variables['IMPWET2'][:]
        FORCING.variables['IMPWET2'][:] = addNoise2Impur(var, 'IMPWET2', param['IMPWET2'][0], param['IMPWET2'][1], dt, semiDistrib = semiDistrib, brutal=brutal)

    # Disturb IMPDRY1
    if param['IMPDRY1'][0] != 0:
        var = FORCING.variables['IMPDRY1'][:]
        FORCING.variables['IMPDRY1'][:] = addNoise2Impur(var, 'IMPDRY1', param['IMPDRY1'][0], param['IMPDRY1'][1], dt, semiDistrib = semiDistrib, brutal=brutal)

    # Disturb IMPDRY2
    if param['IMPDRY2'][0] != 0:
        var = FORCING.variables['IMPDRY2'][:]
        FORCING.variables['IMPDRY2'][:] = addNoise2Impur(var, 'IMPDRY2', param['IMPDRY2'][0], param['IMPDRY2'][1], dt, semiDistrib = semiDistrib, brutal=brutal)

    FORCING.close()


def multiprocess(forcName, paramPath, nmembers, yearDir, brutalImp):
    p = multiprocessing.Pool(min(multiprocessing.cpu_count(), nmembers))
    p.map(workerPerturb, [[forcName, paramPath, i, yearDir, brutalImp] for i in range(1, nmembers + 1)])
    p.close()
    p.join()


if __name__ == '__main__':
    if 'beaufixlogin' in os.uname()[1]:
        raise Exception('do not launch this script directly on beaufix login!',
                        'use submit_jobs_gener_opert_forcings_beaufix.py ',
                        ' to properly allocate a computation node')

    tmp = os.path.expanduser("~")
    username = tmp.split('/')[-1]
    # ou plus direct mais avec import getpass
    # username = getpass.getuser()
    parser = PertParser()
    args = parser.parse()
    if args.cacheBeauf[-1] != '/':
        args.cacheBeauf += '/'
    if args.rootHend[-1] != '/':
        args.rootHend += '/'

    FORbname = args.inFOR.split('/')[-1]
    yearDir = args.cacheBeauf + args.Exp

    if not os.path.exists(yearDir):
        os.makedirs(yearDir)
    shutil.copyfile(args.param, yearDir + '/param.txt')

    # run the perturbation for a FORCING file
    multi = multiprocess(args.inFOR, args.param, args.nmembers, yearDir, args.brutalImp)

    # if on beaufix, transfer the forcings via a transfer job.
    # Else, you' ll have to do it yourself.
    if 'beaufix' in os.uname()[1]:
        # prepare the transfer script

        job_transfert_name = "job_transfert_pert_forcings_out.bash"
        job_transfert_path = "/home/cnrm_other/cen/mrns/deschampsbc/script/"
        if os.path.exists(job_transfert_path + job_transfert_name):
            os.remove(job_transfert_path + job_transfert_name)
        job_transfert_local = open(job_transfert_path + job_transfert_name, 'a')

        job_transfert_local.write("#!/bin/bash\n#SBATCH --verbose\n#SBATCH --job-name=pertforc_transfert_" + "\n" +
                                  "#SBATCH --nodes=1\n#SBATCH --ntasks=1\n#SBATCH --ntasks-per-core=1\n" +
                                  "#SBATCH --time=" + args.walltime_transfer + "\n#SBATCH --mem=1gb\n#SBATCH --partition=transfert\n")

        for mb in range(1, args.nmembers + 1):
            vpathbeauf = yearDir + '/mb{0:04d}/meteo/'.format(mb) + FORbname
            vpathhend = args.rootHend + args.Exp + '/mb{0:04d}/meteo/'.format(mb) + FORbname
            job_transfert_local.write('ftput -o mkdir -u ' + username + ' -h hendrix.meteo.fr ' + vpathbeauf + ' ' + vpathhend  + ' || exit\n')
        job_transfert_local.close()

        # transfer all the files to hendrix
        os.system('sbatch ' + job_transfert_path + job_transfert_name)
    else:
        print("A visualisation task is launched to check the outputs. You will find it in viz/ dir")
        print("now it is up to you to archive the forcings stored at:")
        print(yearDir)
        if not os.path.exists(yearDir + '/viz'):
            os.mkdir(yearDir + '/viz')
        cmd2 = os.environ['SNOWTOOLS_CEN'] + '/tools/VizForEnsemble.py -r ' + yearDir + '/' + ' -o ' + yearDir  + '/' + 'viz' +\
            ' -ref ' + args.inFOR
        os.system(cmd2)
