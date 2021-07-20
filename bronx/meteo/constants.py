# -*- coding: utf-8 -*-

"""
Atmospheric physics constants.
"""

from __future__ import print_function, absolute_import, unicode_literals, division

import numpy

#: No automatic export
__all__ = []

# FUNDAMENTAL CONSTANTS

# : von karman constant
Karman = 0.4
# : Planck constant
Planck = 6.6260755E-34
# : Boltzman constant
Boltz = 1.380658E-23
# : Avogadro number
Avogadro = 6.0221367E+23
# : reference pressure for potential temperatures
P0 = 100000.
# : gravity constant
g0 = 9.80665

# THERMO
# : Molar mass of dry air
Md = 28.9644E-3
# : Molar mass of water vapor
Mv = 18.0153E-3
# : Specific gas constant, dry air 287.059
Rd = Avogadro * Boltz / Md
# : Specific gas constant, water vapor 461.524
Rv = Avogadro * Boltz / Mv
# : Cp dry air 1004.709
Cpd = 7. * Rd / 2.
# : Cp water vapor
Cpv = 4. * Rv
Cl = 4.218E+3
Ci = 2.106E+3
# : Triple point temperature
T0 = 273.16
# : Vaporisation heat constant
Lv0 = 2.5008E+6
# : Sublimation heat constant
Ls0 = 2.8345E+6
# : Saturation vapor pressure at triple point temperature
Es0 = 611.14
# : Constant for saturation vapor pressure function
gamw = (Cl - Cpv) / Rv
# : Constant for saturation vapor pressure function
betaw = (Lv0 / Rv) + (gamw * T0)
# : Constant for saturation vapor pressure function
alpw = numpy.log(Es0) + (betaw / T0) + (gamw * numpy.log(T0))
# : Constant for saturation vapor pressure function over solid ice
gami = (Ci - Cpv) / Rv
# : Constant for saturation vapor pressure function over solid ice
betai = (Ls0 / Rv) + (gami * T0)
# : Constant for saturation vapor pressure function over solid ice
alpi = numpy.log(Es0) + (betai / T0) + (gami * numpy.log(T0))
