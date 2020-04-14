#!/usr/bin/env/python3
# -*- coding: utf-8 -*-
'''
Created on 12 févr. 2020

@author: cluzetb

Generate perturbed forcings on hendrix in order to save time by parallelizing.
inspired on Code/Code_stage_CEN/run_noeud.py

'''
import csv
import multiprocessing
import os
from shutil import copyfile
import shutil

import netCDF4

import numpy as np
from tools.makeForcingEnsemble import addNoise2DIR_SWdown, addNoise2Impur, addNoise2LWdown,\
    addNoise2Rainf, addNoise2Snowf, addNoise2Tair, addNoise2Wind, convertPrecipPhase


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

    # read paramaters: sigma and tau for each disturbed variable from a csv file
    param = {}
    with open(po, mode = 'r') as csv_file:
        csv_reader = csv.DictReader(csv_file)
        print('Param value')
        for row in csv_reader:
            param[row['varName']] = [float(row['std']), float(row['tau'])]
            print(str(row['varName']) + ' | std : ' + str(param[row['varName']][0]) + ' | tau : ' + str(param[row['varName']][1]))

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
    shutil.copy( f, outFOR )

    # generate perturbed forcing
    FORCING = netCDF4.Dataset(outFOR, 'a')
    t = FORCING.variables['time']
    dt = float(t[1] - t[0])
    semiDistrib = len(np.shape(FORCING.variables['Tair'])) == 2  # test the number of dims
    # Disturb Tair
    if param['Tair'][0] != 0:
        FORCING = addNoise2Tair( FORCING, param['Tair'][0], param['Tair'][1], dt, semiDistrib = semiDistrib)

    # Disturb Snowf
    if param['Snowf'][0] != 0:
        FORCING = addNoise2Snowf( FORCING, param['Snowf'][0], param['Snowf'][1], dt, semiDistrib = semiDistrib)

    # Disturb Rainf
    if param['Rainf'][0] != 0:
        FORCING = addNoise2Rainf( FORCING, param['Rainf'][0], param['Rainf'][1], dt, semiDistrib = semiDistrib)

    # Disturb SWdown
    if param['DIR_SWdown'][0] != 0:
        FORCING = addNoise2DIR_SWdown( FORCING, param['DIR_SWdown'][0], param['DIR_SWdown'][1], dt, semiDistrib = semiDistrib)

    # Disturb Wind
    if param['Wind'][0] != 0:
        FORCING = addNoise2Wind( FORCING, param['Wind'][0], param['Wind'][1], dt, semiDistrib = semiDistrib)

# Disturb LWdown
    if param['LWdown'][0] != 0:
        FORCING = addNoise2LWdown( FORCING, param['LWdown'][0], param['LWdown'][1], dt, semiDistrib = semiDistrib)

    # Disturb IMPWET1
    if param['IMPWET1'][0] != 0:
        FORCING = addNoise2Impur( FORCING, 'IMPWET1', param['IMPWET1'][0], param['IMPWET1'][1], dt, semiDistrib = semiDistrib, brutal=brutal)

    # Disturb IMPWET2
    if param['IMPWET2'][0] != 0:
        FORCING = addNoise2Impur( FORCING, 'IMPWET2', param['IMPWET2'][0], param['IMPWET2'][1], dt, semiDistrib = semiDistrib, brutal=brutal)

    # Disturb IMPDRY1
    if param['IMPDRY1'][0] != 0:
        FORCING = addNoise2Impur( FORCING, 'IMPDRY1', param['IMPDRY1'][0], param['IMPDRY1'][1], dt, semiDistrib = semiDistrib, brutal=brutal)

    # Disturb IMPDRY2
    if param['IMPDRY2'][0] != 0:
        FORCING = addNoise2Impur( FORCING, 'IMPDRY2', param['IMPDRY2'][0], param['IMPDRY2'][1], dt, semiDistrib = semiDistrib, brutal=brutal)

    # Convert phases to solid or liquid according to threshold temperature ! MUST ALWAYS COME AFTER TEMPERATURE WERE DISTURBED
    FORCING = convertPrecipPhase( FORCING, semiDistrib = semiDistrib)

    FORCING.close()


def multiprocess(forcName, paramPath, nmembers, yearDir, brutalImp):

    p = multiprocessing.Pool(min(multiprocessing.cpu_count(), nmembers))
    p.map(workerPerturb, [[forcName, paramPath, i, yearDir, brutalImp] for i in range(1, nmembers + 1)])
    p.close()
    p.join()
    # for p in procs:
    #    p.join()


if __name__ == '__main__':
    # ### PARAMETERS ###
    years = [2013, 2014, 2015, 2016]
    nmembers = 160
    brutalImp = '--brutalImp'
    vconf = 'postes_8_9_12_13_15_16_csv/'
    timelimitTransfer = '00:30:00'  # HH:mm:ss
    ####################
    # where to find the reference forcing file and the parameters file
    refPath = '/home/cluzetb/Data/safran/' + vconf + '/ref/mb0000/meteo/'
    paramPath = '/home/cluzetb/snowtools_git/tools/param.txt'

    # where to put the perturbed forcings on beaufix cache
    rootBeauf = '/scratch/mtool/cluzetb/cache/vortex/safran/' + vconf

    # where to put the archived forcings on hendrix
    rootHend = '/home/cluzetb/vortex/safran/' + vconf

    for year in years:
        forcName = refPath + 'FORCING_{0}080106_{1}080106.nc'.format(year, year + 1)

        yearDir = rootBeauf + 'forcing_{0}{1}B_31D_11_t1500_160/'.format(year, year + 1)

        if not os.path.exists(yearDir):
            os.makedirs(yearDir)
        copyfile(paramPath, yearDir + 'param.txt')
        # run the perturbation for the considered year
        multi = multiprocess(forcName, paramPath, nmembers, yearDir, brutalImp)

    # prepare the transfer script
    job_transfert_name = "job_transfert_pert_forcings_out.bash"
    job_transfert_path = "/home/cnrm_other/cen/mrns/cluzetb/assim/jobs/"
    if os.path.exists(job_transfert_path + job_transfert_name):
        os.remove(job_transfert_path + job_transfert_name)
    job_transfert_local = open(job_transfert_path + job_transfert_name, 'a')

    job_transfert_local.write("#!/bin/bash\n#SBATCH --verbose\n#SBATCH --job-name=pertforc_transfert_" + "\n" +
                              "#SBATCH --nodes=1\n#SBATCH --ntasks=1\n#SBATCH --ntasks-per-core=1\n" +
                              "#SBATCH --time=" + timelimitTransfer + "\n#SBATCH --mem=1gb\n#SBATCH --partition=transfert\n")
    for year in years:
        for mb in range(1, nmembers + 1):
            vpathbeauf = rootBeauf + 'forcing_{0}{1}B_31D_11_t1500_160/'.format(year, year + 1)\
                + 'mb{0:04d}/meteo/'.format(mb) + 'FORCING_{0}080106_{1}080106.nc'.format(year, year + 1)
            vpathhend = rootHend + 'forcing_{0}{1}B_31D_11_t1500_160/'.format(year, year + 1)\
                + 'mb{0:04d}/meteo/'.format(mb) + 'FORCING_{0}080106_{1}080106.nc'.format(year, year + 1)
            job_transfert_local.write('ftput -o mkdir -u cluzetb -h hendrix.meteo.fr ' + vpathbeauf + ' ' + vpathhend  + ' || exit\n')
    job_transfert_local.close()

    # transfer all the files to hendrix
    os.system('sbatch ' + job_transfert_path + job_transfert_name)
