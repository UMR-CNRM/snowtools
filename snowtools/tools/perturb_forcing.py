# -*- coding: utf-8 -*-

"""
Created on 3 Aug. 2022

@author: M Lafaysse, from B. Cluzet methods
"""

import numpy as np
import os
import csv

from snowtools.tools.change_forcing import forcinput_tomodify
from snowtools.DATA import SNOWTOOLS_DATA


def gener_random_serie_autocorr(nt, sigma, tau, dt, semiDistrib=False, fsys=0):
    """
    Generates a serie perturbation_vector of random temporally correlated values, centered on fsys of std sigma and
    temporal correlation tau.

    :param nt: time dimension length
    :type nt: int
    :param sigma: standard deviation of the random perturbation
    :type sigma: float
    :param tau:  temporal correlation length of the random perturbation (hours)
    :type tau: int
    :param dt: time step (hours)
    :type dt: int
    :param fsys: value of the systematic perturbation
    :type fsys: float
    :param semiDistrib: flag if var in SAFRAN geometry or semi-distributed geometry
    :type semiDistrib: bool

    :return perturbation_vector: vector of the same size as var of perturbation
    """

    xx = np.zeros(nt, float)  # random part of the perturbation

    v2 = sigma * sigma  # variance
    phi = np.exp(- dt / tau)
    sig2 = v2 * (1 - phi * phi)
    sig = np.sqrt(sig2)
    np.random.seed(None)
    epsilon = np.random.normal(0., sig, nt)

    for ii in range(nt):
        xx[ii] = phi * xx[ii - 1] + epsilon[ii]  # phi ensures the temporal correlation, epsilon the random part

    # add the random (perturbation_vector) and systematic (fsys) parts of the perturbation
    perturbation_vector = fsys + xx
    if semiDistrib:
        perturbation_vector = np.reshape(perturbation_vector, (nt, 1))
    else:
        perturbation_vector = np.reshape(perturbation_vector, (nt, 1, 1))

    return perturbation_vector


def convertPrecipPhase(Tair, precip, Tmin=272.65, Tmax=274.05):
    """
    Convert precipitation to solid or liquid according to the disturbed temperature.

    :param Tair: air temperature data (K)
    :param precip: precip data
    :param Tmin: minimum temperature for liquid precip (K)
    :param Tmax: maximum temperature for solid precip (K)

    :return: list [Rain, Snow]

    """

    frac_liquid = (Tair - Tmin) / (Tmax - Tmin)  # mix of liquid and solid precip if Tair within [Tmin,Tmax]
    frac_liquid[Tair < Tmin] = 0.  # solid precip if Tair < Tmin
    frac_liquid[Tair > Tmax] = 1.  # liquid precip if Tair > Tmax

    # Write Rainf, Snowf
    Rainfout = frac_liquid * precip
    Snowfout = (1. - frac_liquid) * precip

    return Rainfout, Snowfout


def addNoiseMultiplicative(var, sigma, tau, dt, fsys=1, semiDistrib=False):
    """
    Apply multiplicative perturbation (sigma=std of random perturbation, tau=temporal correlation, fsys=systematic

    :param var: data to be pertubed
    :type var: numpy.array
    :param sigma: standard deviation of the random perturbation
    :type sigma: float
    :param tau:  temporal correlation length of the random perturbation (hours)
    :type tau: float
    :param dt: time step (hour)
    :type dt: int
    :param fsys: value of the systematic perturbation
    :type fsys: float
    :param semiDistrib: flag if var in SAFRAN geometry or semi-distributed geometry
    :type semiDistrib: bool

    :return perturbed variable, same type and shape as var
    """
    nt = np.shape(var)[0]  # time length
    # generates random perturbation factor vector
    perturbation_vector = gener_random_serie_autocorr(nt, sigma, tau, dt, semiDistrib=semiDistrib, fsys=fsys)
    # perturbation cannot be negative !! this will introduce a positive bias in the forcings,
    # especially if fsys<1 or sigma>>mean(var)
    perturbation_vector[perturbation_vector < 0] = 0
    varout = var[:] * perturbation_vector  # multiplicative perturbation of the variable
    return varout


def addNoiseAdditive(var, sigma, tau, dt, fsys = 0, semiDistrib=False):
    """
    Apply additive perturbation (sigma=std of random perturbation, tau=temporal correlation, fsys=systematic
    perturbation) to a time serie.

    :param var: data to be pertubed
    :type var: numpy.array
    :param sigma: standard deviation of the random perturbation
    :type sigma: float
    :param tau:  temporal correlation length of the random perturbation (hours)
    :type tau: float
    :param dt: time step (hour)
    :type dt: int
    :param fsys: value of the systematic perturbation
    :type fsys: float
    :param semiDistrib: flag if var in SAFRAN geometry or semi-distributed geometry
    :type semiDistrib: bool

    :return perturbed variable, same type and shape as var
    """
    nt = np.shape(var)[0]  # time length
    # generates random perturbation factor vector
    perturbation_vector = gener_random_serie_autocorr(nt, sigma, tau, dt, semiDistrib=semiDistrib, fsys=fsys)
    # print('ppp', perturbation_vector.shape)
    varout = var[:] + perturbation_vector  # additive perturbation of the variable
    return varout


def addNoise2Impur(var, varName, sigma, tau, dt, semiDistrib=False, brutal=False):
    """
    Add noise with temporal correlation.

    :param var: data to be pertubed
    :type var: numpy.array
    :param varName: name of the variable
    :type varName: str
    :param sigma: standard deviation of the random perturbation
    :type sigma: float
    :param tau:  temporal correlation length of the random perturbation (hours)
    :type tau: float
    :param dt: time step (hour)
    :type dt: int
    :param semiDistrib: flag if var in SAFRAN geometry or semi-distributed geometry
    :type semiDistrib: bool
    :param brutal: multiply all timesteps by a random factor following a lognormal law
    :type brutal: bool
    """
    if not brutal:
        varout = addNoiseMultiplicative(var, sigma, tau, dt, fsys=1, semiDistrib = semiDistrib)
    else:  # brutal : multiply all timesteps by a random factor following a lognormal law
        nt = np.shape(var)[0]  # time length
        if '1' in varName:  # case for BC
            # /100 x1 (too much BC in MOCAGE)
            logstdFact = 1
            meanFact = -2
        else:  # Case for Dust
            # /10*10 (no clue about Dust) -> (1,0),
            logstdFact = 1
            meanFact = 0
        fact = np.random.normal(meanFact, logstdFact, 1)
        # BC 24/10/19 toggle on the following line for baseline experiment.
        # fact = meanFact
        YY = 10**fact * np.ones((nt, ), float)
        if semiDistrib:
            YY = np.reshape(YY, (np.shape(var)[0], 1))
        else:
            YY = np.reshape(YY, (np.shape(var)[0], 1, 1))

        varout = var[:] * YY

    return varout


class forcinput_perturb(forcinput_tomodify):
    """
    Stochastically perturbed meterological forcing file for assimilation purpose
    """

    seuilmin = dict(Tair = 200.,
                    LWdown = 100.,
                    DIR_SWdown = 0.,
                    SCA_LWdown = 0.,
                    )
                    
    seuilmax = dict(Tair = 330.,
                    LWdown = 450.,
                    DIR_SWdown = 1500.,
                    SCA_LWdown = 800.,
                    )
                    

    def modify(self, init_forcing_file, new_forcing_file, *args):
        """
        Add stochastic perturbations to a meterological forcing file

        :param init_forcing_file: Input file object
        :type init_forcing_file: :class:`utils.prosimu.prosimu`
        :param new_forcing_file: Output file object
        :type new_forcing_file: :class:`utils.prosimu.prosimu`
        """

        np.random.seed()

        po = os.path.join(SNOWTOOLS_DATA, 'perturbations_param.txt')
        brutal = True

        # read parameters: sigma and tau for each disturbed variable from a csv file
        param = {}
        with open(po, mode='r') as csv_file:
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
                param[row['varName']] = [float(row['std']), float(row['tau']), fsysparam]
                print(str(row['varName']) + ' | std : ' + str(param[row['varName']][0]) + ' | tau : ' + str(
                    param[row['varName']][1]) + ' | bias factor : ' + str(param[row['varName']][2]))

        time = init_forcing_file.readtime()
        delta = time[1] - time[0]
        dt = delta.seconds / 3600

        new_forcing_file.createDimension("time", None)

        dictdim = init_forcing_file.listdim()
        del dictdim["time"]

        semiDistrib = init_forcing_file.pointsdim is not None

        for dimname, dim in dictdim.items():
            print("Create dimension " + dimname + " " + str(len(dim)))
            new_forcing_file.createDimension(dimname, len(dim))

        listvar = init_forcing_file.listvar()

        for varname in listvar:

            vartype, rank, array_dim, varFillvalue, var_attrs = init_forcing_file.infovar(varname)
            newvar = new_forcing_file.createVariable(varname, vartype, array_dim, fill_value=varFillvalue)

            # Determine whether the variable should be disturbed or not
            if varname in ['Rainf', 'Snowf'] and 'Precip' in param.keys():
                # Specific case for precip
                modifyvar = param['Precip'][0] != 0
            elif varname in param.keys():
                modifyvar = param[varname][0] != 0
            else:
                modifyvar = False

            # Read initial variable
            var = init_forcing_file.read(varname)
            if modifyvar:
                print("Disturb " + varname)
                # Variable-dependent disturbance before writing new variable
                if varname in ['Tair', 'LWdown']:
                    tempvar = addNoiseAdditive(var, param[varname][0], param[varname][1], dt,
                                                 fsys=param[varname][2], semiDistrib=semiDistrib)

                elif varname in ['DIR_SWdown', 'Wind']:
                    tempvar = addNoiseMultiplicative(var, param[varname][0], param[varname][1], dt,
                                                       fsys=param[varname][2], semiDistrib=semiDistrib)

                elif varname in ['Rainf']:
                    Precip = init_forcing_file.read('Snowf') + init_forcing_file.read('Rainf')
                    Precip = addNoiseMultiplicative(Precip, param['Precip'][0], param['Precip'][1], dt,
                                                    fsys=param['Precip'][2], semiDistrib=semiDistrib)
                    Tair = init_forcing_file.read('Tair')
                    tempvar[:], snowf  = \
                        convertPrecipPhase(Tair, Precip, Tmin=272.65, Tmax=274.05)
                elif varname in ['Snowf']:
                    tempvar = snowf
                elif varname in ['IMPWET1', 'IMPWET2', 'IMPDRY1', 'IMPDRY2']:
                    tempvar = addNoise2Impur(var, varname, param[varname][0], param[varname][1], dt,
                                               semiDistrib=semiDistrib, brutal=brutal)

                else:
                    # Perturbation prescribed in the parameter file but unknown method for this variable
                    raise Exception('Undefined perturbation method for variable ' + varname)
                    
                # Apply thresholds to avoid unrealistic values
                if varname in self.seuilmax.keys():
                    print ('Apply max threshold for ' + varname)
                    tempvar = np.where(tempvar > self.seuilmax[varname], self.seuilmax[varname], tempvar)
                if varname in self.seuilmin.keys():
                    print ('Apply min threshold for ' + varname)
                    tempvar = np.where(tempvar < self.seuilmin[varname], self.seuilmin[varname], tempvar)
                
                newvar[:] = tempvar
                
                # Specify that this is a disturbed variable
                setattr(newvar, 'disturbed', 1)
            else:
                print("Copy " + varname)
                newvar[:] = var[:]

            # Copy attributes from input to output file
            for attname in var_attrs:
                if not attname == '_FillValue':
                    setattr(newvar, attname, init_forcing_file.getattr(varname, attname))


if __name__ == "__main__":
    import sys
    f = forcinput_perturb(sys.argv[1], sys.argv[2])
