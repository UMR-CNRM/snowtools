#!/usr/bin/python
# -*- coding: utf-8 -*-

"""
Created on Wed Oct 24 17:53:27 2018

@author: deschampsbc, cluzetb
Command to generate an ensemble of perturbed forcings from :
  - initial forcing file (param.txt as an example)
  - .csv file with statistical values of perturbations for each variable


 Adapted from Charrois/Revuelto's

IN:
    -f : reference forcing file
    -p : csv file with the value of std and tau for each disturbed variable
    -nmembers  : size of the ensemble
    -o : writing output in o
    -startmember : number of the first disturbed file
Reference : Charrois et al., 2016
"""
import netCDF4
import os
import shutil
import numpy as np
import argparse
import csv


def addNoise2Tair( f, sigma, tau, dt, semiDistrib = False):
    """
    Add noise with temporal correlation.
    """
    print('Treating variable : Tair')
    var = f.variables['Tair']
    nT = np.shape(var)[0]  # time length
    xx = np.zeros(nT, float)

    v2 = sigma * sigma  # variance
    phi = np.exp( - dt / tau)
    sig2 = v2 * (1 - phi * phi)
    sig = np.sqrt(sig2)
    epsilon = np.random.normal( 0., sig, nT)

    for ii in range(nT):
        xx[ii] = phi * xx[ii - 1] + epsilon[ii]
    if semiDistrib:
        xx = np.reshape(xx, (nT, 1))
    else:
        xx = np.reshape(xx, (nT, 1, 1))
    var[:] = var[:] + xx

    return f


def addNoise2Snowf( f, sigma, tau, dt, semiDistrib = False):
    """
    Add noise with temporal correlation.
    """
    print('Treating variable : Snowf')
    var = f.variables['Snowf']
    nT = np.shape(var)[0]  # time length

    xx = np.zeros(nT, float)
    XX = np.zeros(nT, float)
    YY = np.zeros(nT, float)

    phi = np.exp( - dt / tau)
    sig2 = (1 - phi * phi)
    sig = np.sqrt(sig2)
    epsilon = np.random.normal(0., sig, nT)

    for ii in range(nT):
        xx[ii] = phi * xx[ii - 1] + epsilon[ii]
        XX[ii] = xx[ii] * 0.7
        YY[ii] = 1 + XX[ii]
        while YY[ii] < 0 or YY[ii] > 2:
            epsilon[ii] = np.random.normal(0., sig)
            xx[ii] = phi * xx[ii - 1] + epsilon[ii]
            XX[ii] = xx[ii] * 0.7
            YY[ii] = 1 + XX[ii]
    if semiDistrib:
        YY = np.reshape(YY, (nT, 1))
    else:
        YY = np.reshape(YY, (nT, 1, 1))
    var[:] = var[:] * YY

    return f


def addNoise2DIR_SWdown( f, sigma, tau, dt, semiDistrib = False):
    """
    Add noise with temporal correlation.
    """
    print('Treating variable : DIR_SWdown')
    var = f.variables['DIR_SWdown']
    nT = np.shape(var)[0]  # time length

    xx = np.zeros(nT, float)
    XX = np.zeros(nT, float)
    YY = np.zeros(nT, float)

    phi = np.exp( - dt / tau)
    sig2 = (1 - phi * phi)
    sig = np.sqrt(sig2)
    epsilon = np.random.normal(0., sig, nT)
    Snowf = f.variables['Snowf']
    for ii in range(nT):
        # Common to all functions
        if semiDistrib:  # semi-distributed
            varOK = var[ii, :]
            SnowfOK = Snowf[ii, :]

        else:  # distributed
            varOK = var[ii, :, :]
            SnowfOK = Snowf[ii, :, :]

        # Possibly specific to this var
        xx[ii] = phi * xx[ii - 1] + epsilon[ii]
        XX[ii] = xx[ii] * 0.7
        YY[ii] = 1 + XX[ii]
        while YY[ii] < 0.5 or YY[ii] > 1.5:
            epsilon[ii] = np.random.normal(0., sig)
            xx[ii] = phi * xx[ii - 1] + epsilon[ii]
            XX[ii] = xx[ii] * 0.7
            YY[ii] = 1 + XX[ii]
        var_p = varOK > 200.
        Snowf_p = SnowfOK > 0.
        temp_var_p = var_p * Snowf_p
        index_temp_var_p = np.where(temp_var_p)
        index_temp_var_p_else = np.where(~temp_var_p)
        varOK[index_temp_var_p] = 200.
        varOK[index_temp_var_p_else] = varOK[index_temp_var_p_else] * YY[ii]

        if semiDistrib:
            var[ii, :] = varOK[:]
        else:
            var[ii, :, :] = varOK[:, :]
    return f


def addNoise2Wind( f, sigma, tau, dt, semiDistrib = False):
    """
    Add noise with temporal correlation.
    """
    print('Treating variable : Wind')
    var = f.variables['Wind']
    nT = np.shape(var)[0]  # time length

    xx = np.zeros(nT, float)
    XX = np.zeros(nT, float)
    YY = np.zeros(nT, float)
    phi = np.exp( - dt / tau)
    sig2 = (1 - phi * phi)
    sig = np.sqrt(sig2)
    epsilon = np.random.normal(0., sig, nT)

    for ii in range(nT):
        xx[ii] = phi * xx[ii - 1] + epsilon[ii]
        XX[ii] = xx[ii] * 0.7
        YY[ii] = 1 + XX[ii]
        while YY[ii] < 0 or YY[ii] > 2:
            epsilon[ii] = np.random.normal(0., sig)
            xx[ii] = phi * xx[ii - 1] + epsilon[ii]
            XX[ii] = xx[ii] * 0.7
            YY[ii] = 1 + XX[ii]
    if semiDistrib:
        YY = np.reshape(YY, (nT, 1))
    else:
        YY = np.reshape(YY, (nT, 1, 1))
    var[:] = var[:] * YY

    return f


def addNoise2LWdown( f, sigma, tau, dt, semiDistrib = False):
    """
    Add noise with temporal correlation.
    """
    print('Treating variable : LWdown')
    var = f.variables['LWdown']
    nT = np.shape(var)[0]  # time length

    xx = np.zeros(nT, float)

    v2 = sigma * sigma
    phi = np.exp( - dt / tau)
    sig2 = v2 * (1 - phi * phi)
    sig = np.sqrt(sig2)
    epsilon = np.random.normal(0., sig, nT)

    for ii in range(nT):
        xx[ii] = phi * xx[ii - 1] + epsilon[ii]
    if semiDistrib:
        xx = np.reshape(xx, (nT, 1))
    else:
        xx = np.reshape(xx, (nT, 1, 1))

    var[:] = var[:] + xx

    return f


def addNoise2Impur( f, varName, sigma, tau, dt, semiDistrib = False, brutal=False):
    """
    Add noise with temporal correlation.
    """
    print('Treating variable : ' + varName)
    var = f.variables[varName]
    nT = np.shape(var)[0]  # time length

    xx = np.zeros((nT, ), float)
    XX = np.zeros((nT, ), float)
    YY = np.zeros((nT, ), float)

    phi = np.exp( - dt / tau)
    sig2 = (1 - phi * phi)
    sig = np.sqrt(sig2)
    epsilon = np.random.normal(0., sig, nT)
    if not brutal:

        for ii in range(nT):
            xx[ii]  = phi * xx[ii - 1] + epsilon[ii]
            XX[ii] = xx[ii] * 0.7
            YY[ii] = 1 + XX[ii]
            while YY[ii] < 0.5 or YY[ii] > 2.:
                epsilon[ii] = np.random.normal(0., sig)
                xx[ii] = phi * xx[ii - 1] + epsilon[ii]
                XX[ii] = xx[ii] * 0.7
                YY[ii] = 1 + XX[ii]

                epsilon[ii] = np.random.normal(0., 100 * sig)
                xx[ii] = phi * xx[ii - 1] + epsilon[ii]
                YY[ii] = xx[ii]
    else:  # brutal : multiply all timesteps by a random factor following a lognormal law
        if '1' in varName:  # case for BC
            # /100 x1 (too much BC in MOCAGE)
            logstdFact = 1
            meanFact = -1
        else:
            # /10*10 (no clue about Dust)
            logstdFact = 1
            meanFact = 0
        fact = np.random.normal(meanFact, logstdFact, 1)
        YY = np.exp(fact) * np.ones((nT, ), float)
    if semiDistrib:
        YY = np.reshape(YY, (np.shape(var)[0], 1))
    else:
        YY = np.reshape(YY, (np.shape(var)[0], 1, 1))

    var[:] = var[:] * YY

    return f


def convertPrecipPhase( f, semiDistrib = False):

    """
    Convert precipitation to solid or liquid according to the disturbed temperature.
    """
    Tair = f.variables['Tair']
    Rainf = f.variables['Rainf']
    Snowf = f.variables['Snowf']

    Tair1 = Tair[:]
    Rainf1 = Rainf[:]
    Snowf1 = Snowf[:]

    Tpos = (Tair1  > 274.5)
    ItSnows = (Snowf1  > 0.)
    keep = np.where(  Tpos * ItSnows  )
    Rainf1[keep] = Snowf1[keep]
    Rainf[:] = Rainf1

    Tneg = (Tair1 < 274.5)
    ItRains = (Rainf1 < 0.)
    keep = np.where( Tneg * ItRains )
    Snowf1[keep] = Rainf1[keep]
    Snowf[:] = Snowf1

    return f


def MakeForEnsemble( f, po, nmembers, o, startmember=1, brutal=False):

    print ''
    print(' -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - ')
    print('Start generating forcing ensemble')
    print(' -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - ')
    print ''
    print(' Reference forcing : ' + f)
    print ''

    if not os.path.exists(o):  # creates o
        os.mkdir(o)

    if o.endswith('/'):  # format o
        o = o[: - 1]

    nL = 4  # length of the digit appended to the folder name

    # read paramaters: sigma and tau for each disturbed variable from a csv file
    param = {}
    with open(po, mode = 'r') as csv_file:
        csv_reader = csv.DictReader(csv_file)
        print 'Param value'
        for row in csv_reader:
            param[row['varName']] = [float(row['std']), float(row['tau'])]
            print(str(row['varName']) + ' | std : ' + str(param[row['varName']][0]) + ' | tau : ' + str(param[row['varName']][1]))

    bn = os.path.basename(f)

    # generate N - 1 forcing and copy the reference forcing (' * _startmember.nc')
    for i in range(startmember, startmember + nmembers):

        outN = str(i).zfill(nL)

        print ''
        print('Generating forcing number : ' + outN)

        oMb = o + '/mb' + str(outN)
        if not os.path.exists(oMb):
            os.mkdir(oMb)

        outFOR = oMb + '/' + bn
        print(outFOR)
        shutil.copy( f, outFOR )

        if i != startmember:

            # generate disturbed forcing
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

            FORCING.close


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description = "Generate ensemble weather forcing.")
    parser.add_argument("-f", dest = "f")
    parser.add_argument("-p", dest = "p")
    parser.add_argument("-nmembers", dest = "nmembers", type=int)
    parser.add_argument("-startmember", dest = "startmember", default=1, type=int)
    parser.add_argument("-o", dest = "o")
    parser.add_argument("--brutalImp", dest = 'brutalImp', default = False, action = "store_true",)
    args = parser.parse_args()
    print args.brutalImp
    MakeForEnsemble( args.f, args.p, args.nmembers, args.o, args.startmember, brutal = args.brutalImp)
