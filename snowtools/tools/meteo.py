# -*- coding: utf-8 -*-

"""
:Authors:
    - Diego Monteiro
    - LVG

Common atmosphere/meteorological conversions that are not elsewhere but commonly used
in particular in the FORCING preparation.

Note : The dependency bronx proposes numerous tools to represent atmosphere state
in ``bronx.meteo.thermo module``. If you do not find what you need in this module,
please check what you need is not yet implemented in bronx.

In particular conversion between relative humidity (rh) and specific humidity (qair) could
be done the following way (knowing the air temperature ``Tair`` and atmospheric pressure ``PSurf``):

.. code-block:: python

   import numpy as np
   from bronx.meteo.thermo import Thermo

   # rh -> qair:
   qair = Thermo(['v', 'c'], data=dict(P=PSurf, Huw=hu, T=Tair, rc=np.zeros_like(Tair))).get('qv')

   # qair -> rh
   rh = Thermo(['v', 'c'], data=dict(P=PSurf, qv=qair, qt=qair, T=Tair)).get('Huw')
   # Note: it may be necessary to cap to 100% when there is condensation:
   rh[rh > 100] = 100
   rh[rh < 0] = 0

"""

import numpy as np

CO2_CONCENTRATION = 0.00062
"""Mean CO2 concentration"""


def splitting_prectot(
    prectot: np.array, tair: np.array, rh: np.array, alpha: float = 22, beta: float = -2.7, gamma: float = -0.2
):
    """
    Function used to partition total precipitation into liquid and solid part.

    The computation is using air temperature and relative humidity.

    Alpha, beta and gamma are parameters of logistic values that can be adjusted
    depending on the site considered following Froidurot et al., (2014) method
    (default values are taken from Koistinen and Saltikoff (1998)).

    :param prectot: total precipitation (units in is unit out)
    :type prectot: np.array
    :param tair: air temperature (°C)
    :type tair: np.array
    :param rh:  near surface relative humidity (%)
    :type rh: np.array
    :param alpha: parameters or logistic function, defaults to 22
    :type alpha: float, optional
    :param beta: parameters or logistic function, defaults to -2.7
    :type beta: float, optional
    :param gamma: parameters or logistic function, defaults to -0.2
    :type gamma: float, optional
    :return: Tuple of precliq and precsol (same units as the prectot input)
    :rtype: Tuple of np.array
    """

    # Compute probability of rain and snow using air temperature and relative humidity
    prain = 1 / (1 + np.exp(alpha + beta * tair + gamma * rh))
    psnow = 1 - prain

    # Create output liquid and solid precipitation amount
    # precliq : liquid precipitation (units in is unit out)
    # precsol : solid precipitation (units in is unit out)

    precliq = prectot * prain
    precsol = prectot * psnow

    return precliq, precsol


def psurf(elevation):
    """
    Compute the mean air pressure for a given elevation
    (hypothesis of standard atmosphere)

    :param elevation: Elevation (m a.s.l)
    :type elevation: float or numpy array
    :returns: Surface pressure
    :rtype: same as elevation
    """
    return 101325 * ((288 - 0.0065 * elevation) / 288)**5.255
