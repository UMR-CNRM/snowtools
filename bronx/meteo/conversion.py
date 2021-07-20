# -*- coding: utf-8 -*-

"""
Conversion functions or classes.
"""

from __future__ import print_function, absolute_import, unicode_literals, division

import numpy

from .constants import Rd, Rv


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
