#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Created on Mon Jul 20 22:22:22 2020

@author: deschampsbc, cluzetb
Functions to generate perturbed forcings.

Reference : Buizza et al. 1999. Stochastic representation of model uncertainties in the ECMWF ensemble prediction system
"""
import numpy as np


def gener_random_serie_autocorr(nt, sigma, tau, dt, semiDistrib = False, fsys = 0):
    """
    ### generates a serie perturbation_vector of random temporally correlated values, centered on fsys of std sigma and temporal correlation tau.

    IN:
    -nt : time length
    -sigma : standard deviation of the random perturbation
    -tau :  temporal correlation length of the random perturbation
    -dt : time step
    -fsys : value of the systematic perturbation
    -semiDistrib : flag if var in SAFRAN geometry or semi-distributed geometry

    OUT:
    -perturbation_vector: vector of the same size as var of perturbation
    """

    xx = np.zeros(nt, float)  # random part of the perturbation

    v2 = sigma * sigma  # variance
    phi = np.exp( - dt / tau)
    sig2 = v2 * (1 - phi * phi)
    sig = np.sqrt(sig2)
    np.random.seed(None)
    epsilon = np.random.normal(0., sig, nt)

    for ii in range(nt):
        xx[ii] = phi * xx[ii - 1] + epsilon[ii]  # phi ensures the temporal correlation, epsilon the random part

    perturbation_vector = fsys + xx  # add the random (perturbation_vector) and systematic (fsys) parts of the perturbation
    if semiDistrib:
        perturbation_vector = np.reshape(perturbation_vector, (nt, 1))
    else:
        perturbation_vector = np.reshape(perturbation_vector, (nt, 1, 1))

    return perturbation_vector


def convertPrecipPhase( Tair, precip, Tmin=272.65, Tmax=274.05):
    """
    Convert precipitation to solid or liquid according to the disturbed temperature.
    IN:
    -Tair : air temperature data
    -precip : precip data
    -Tmin : minimum temperature for liquid precip
    -Tmax : maximum temperature for solid precip
    """

    frac_liquid = (Tair - Tmin) / (Tmax - Tmin)  # mix of liquid and solid precip if Tair within [Tmin,Tmax]
    frac_liquid[Tair < Tmin] = 0.  # solid precip if Tair < Tmin
    frac_liquid[Tair > Tmax] = 1.  # liquid precip if Tair > Tmax

    # Write Rainf, Snowf
    Rainfout = frac_liquid * precip
    Snowfout = (1. - frac_liquid) * precip

    return Rainfout, Snowfout


def addNoiseMultiplicative( var, sigma, tau, dt, fsys = 1, semiDistrib = False):
    """
    Apply multiplicative perturbation (sigma=std of random perturbation, tau=temporal correlation, fsys=systematic perturbation)to a time serie.
    -var : data to be pertubed
    -sigma : standard deviation of the random perturbation
    -tau :  temporal correlation length of the random perturbation
    -dt : time step
    -fsys : value of the systematic perturbation
    -semiDistrib : flag if var in SAFRAN geometry or semi-distributed geometry

    OUT :
    -varout : perturbed variable
    """
    nt = np.shape(var)[0]  # time length
    perturbation_vector = gener_random_serie_autocorr(nt, sigma, tau, dt, semiDistrib = semiDistrib, fsys = fsys)  # generates random perturbation factor vector
    perturbation_vector[perturbation_vector < 0] = 0  # perturbation cannot be negative !! this will introduce a positive bias in the forcings, especially if fsys<1 or sigma>>mean(var)
    varout = var[:] * perturbation_vector  # multiplicative perturbation of the variable
    return varout


def addNoiseAdditive( var, sigma, tau, dt, fsys = 0, semiDistrib = False):
    """
    Apply additive perturbation (sigma=std of random perturbation, tau=temporal correlation, fsys=systematic perturbation)to a time serie.
    IN :
    -var : data to be pertubed
    -sigma : standard deviation of the random perturbation
    -tau :  temporal correlation length of the random perturbation
    -dt : time step
    -fsys : value of the systematic perturbation
    -semiDistrib : flag if var in SAFRAN geometry or semi-distributed geometry

    OUT :
    -varout : perturbed variable
    """
    nt = np.shape(var)[0]  # time length
    perturbation_vector = gener_random_serie_autocorr(nt, sigma, tau, dt, semiDistrib = semiDistrib, fsys=fsys )  # generates random perturbation factor vector
    print('ppp', perturbation_vector.shape)
    varout = var[:] + perturbation_vector  # additive perturbation of the variable
    return varout


def addNoise2Impur(var, varName, sigma, tau, dt, semiDistrib = False, brutal=False):
    """
    Add noise with temporal correlation.
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
