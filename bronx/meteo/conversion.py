#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Conversion functions or classes.
"""

from __future__ import print_function, absolute_import, unicode_literals, division

import numpy

from .constants import Rd, Rv, T0


#: No automatic export
__all__ = []


def q2R(q, ql=0., qi=0., qr=0., qs=0., qg=0.):
    """
    Computes air specific gas constant R according to specific humidity,
    and hydrometeors if present.

    :param q: specific humidity (kg/kg)
    :param ql: liquid water content (kg/kg) (optional, default=0.)
    :param qi: ice water content (kg/kg) (optional, default=0.)
    :param qr: rain water content (kg/kg) (optional, default=0.)
    :param qs: snow water content (kg/kg) (optional, default=0.)
    :param qg: graupel water content (kg/kg) (optional, default=0.)
    """
    q = numpy.array(q)
    ql = numpy.array(ql)
    qi = numpy.array(qi)
    qr = numpy.array(qr)
    qs = numpy.array(qs)
    qg = numpy.array(qg)
    R = Rd + (Rv - Rd) * q - Rd * (ql + qi + qr + qs + qg)
    return R

def ZEW(Tair) :
    # CALCULATION OF THE SATURATION WATER VAPOUR PRESSURE (Pa)
    # Tair in degre Kelvin
    # GOFF AND GRATCH FORMULA
    TATO = Tair / T0
    TOTA = T0 / Tair
    Z = numpy.log(10) * (10.79574 * (1 - TOTA)
                      +  1.50475E-4 * (1. - numpy.exp( numpy.log(10) * (-8.2969) * ( TATO - 1)  ) )
                                     +  0.42873E-3 * (numpy.exp( numpy.log(10) * 4.76955 * (1- TOTA) ) - 1 )
                  + 0.78614 ) +  5.028 * numpy.log(TOTA)

    return 100 * numpy.exp(Z)

def HUMrelHUMspec(Tair, HUMREL,pressure) :
    ZE = ZEW(Tair )
    ZE1 = ( HUMREL / 100 ) * ZE
    ZMR = 0.62198 * ZE1 / (pressure - ZE1)
    try :
        ZQ = 1 / (1 + 1/ZMR)
    except :
        print("!!!!!!   problem with conversion of the humidity")
        for i,ze1 in enumerate(ZE1) :
            if ze1 == 0. :
                print("     null hunidity is set to 5 for the " + i + "eme date  ")
                print("")
                ze1 = 5
            zmr = 0.62198 * ze1 / (pressure[i] - ze1)
            zq = 1 / (1 + 1/zmr)
            ZQ[i] = zq

    return ZQ
