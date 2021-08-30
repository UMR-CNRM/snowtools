# -*- coding: utf-8 -*-

"""
This modules implements constants and conversions used in thermodynamics.

A brief description of the equations can be found in this document:
`thermo.pdf <../../../_static/thermo.pdf>`_

If you find mistakes in the above document or in this module, please,
send an email to sebastien.riette@meteo.fr.
"""

from __future__ import print_function, absolute_import, unicode_literals, division

import sys
import numpy as np

from bronx.fancies import loggers
from bronx.meteo import constants as csts


logger = loggers.getLogger(__name__)

_names = [('T', 'Temperature (K)'),
          ('Theta', 'Potential temperature (K)'),
          ('ThetaV', 'Potential virtual temperature (K)'),
          ('TV', 'Virtual temperature (K)'),

          ('Td', 'Dew point temperature (K)'),
          ('Hui', 'Relative humidity with respect to ice (0-100)'),
          ('Huw', 'Relative humidity with respect to water (0-100)'),
          ('rv', 'Mixing ration of water vapor (kg/kg)'),
          ('rc', 'Mixing ration of cloud liquid water (kg/kg)'),
          ('ri', 'Mixing ration of cloud ice (kg/kg)'),
          ('rr', 'Mixing ration of rain (kg/kg)'),
          ('rs', 'Mixing ration of snow (kg/kg)'),
          ('rg', 'Mixing ration of graupel (kg/kg)'),
          ('rh', 'Mixing ration of hail (kg/kg)'),
          ('qv', 'Specific humidity of water vapor (kg/kg)'),
          ('qc', 'Specific humidity of cloud liquid water (kg/kg)'),
          ('qi', 'Specific humidity of cloud ice (kg/kg)'),
          ('qr', 'Specific humidity of rain (kg/kg)'),
          ('qs', 'Specific humidity of snow (kg/kg)'),
          ('qg', 'Specific humidity of graupel (kg/kg)'),
          ('qh', 'Specific humidity of hail (kg/kg)'),
          ('qci', 'Specific humidity of cloud liquid water and ice (kg/kg)'),
          ('esatw', 'Water vapour pressure at saturation over liquid water (Pa)'),
          ('esati', 'Water vapour pressure at saturation over ice (Pa)'),
          ('rsatw', 'Mixing ratio at saturation of liquid water (kg/kg)'),
          ('rsati', 'Mixing ratio at saturation of ice (kg/kg)'),
          ('e', 'Water vapour pressure (Pa)'),
          ('qliquid', 'Specific content of liquid species (kg/kg)'),
          ('qice', 'Specific content of ice species (kg/kg)'),
          ('rliquid', 'Mixing ratio of liquid species (kg/kg)'),
          ('rice', 'Mixing ratio of ice species (kg/kg)'),
          ('qt', 'Specific content of all species, including water vapor (kg/kg)'),
          ('rt', 'Mixing ratio of all species, including water vapor (kg/kg)'),
          ('R', 'Perfect gaz constant for dry and wet air mixture (J/kg/K)'),
          ('Rstar', "I don't know the name... (J/kg/K)"),
          ('Cp', 'Heat capacity at constant pressure (J/kg/K)'),
          ('Cph', 'Specific heat capacity at constant pressure (J/kg/K)'),

          ('ff', 'Wind force (m/s)'),
          ('dd', 'Wind direction (deg)'),
          ('u', 'Zonal wind (m/s)'),
          ('v', 'Meridional wind (m/s'),
          ('w', 'Vertical wind (m/s)'),
          ('P', 'Pressure (Pa)'),
          ('rho', 'Volumic mass of wet atmosphere'),
          ('rhov', 'Volumic mass of water vapor'),
          ('rhod', 'Volumic mass of dry atmosphere'),
          ('H', 'Height (m)'),
          ('altitude', 'Altitude (m)'),
          ('nebulosity', 'Nebulosity'),  # 0-100 0-1 0-8 ??
          ]
for item in _names:
    sys.modules[__name__].__dict__['N_' + item[0]] = item[0]
    sys.modules[__name__].__dict__['C_' + item[0]] = item[1]


class ThermoError(Exception):
    """Errors in thermo."""


class Thermo(object):
    """This class aims at containing states parameter.

    Values are retrieved by special method which can compute derived parameters.
    One can put anything inside this class but derived parameter computation uses
    defined name for some thermodynamic parameters.
    These names are available in the ``thermo.N_*`` constants and comments are in ``C_*``.
    Values must be all scalars or all numpy.arrays of the same shape.

    Example::

        >>> data = Thermo()
        >>> print(C_T)
        Temperature (K)
        >>> data.set(N_T, 280)   # in K
        >>> data.set(N_P, 75000) # in Pa
        >>> print('{:.2f}'.format(data.get(N_Theta)))
        303.99

        >>> import numpy as np
        >>> data2 = Thermo(hydrometeors=['v', ])
        >>> data2.set(N_T, np.array([280, 300]))       # in K
        >>> data2.set(N_P, np.array([75000, 101300]))  # in Pa
        >>> data2.set(N_rv, np.array([0.05, 0.1]))     # in kg/kg
        >>> print(' '.join(['{:.2f}'.format(r) for r in data2.get(N_rt)]))
        0.05 0.10
        >>> print(' '.join(['{:.2f}'.format(t) for t in data2.get(N_ThetaV)]))
        312.78 315.41

    """

    _known_liquid_hydrometeors = ['c', 'r']
    _known_solid_hydrometeors = ['i', 's', 'g', 'h']

    def __init__(self, hydrometeors=None, data=None):
        """
        :param hydrometeors: lists the hydrometeors to consider.
            If None, only a subset of computations can be done.
            'v', 'c', 'r', 'i', 's', 'g' and 'h' are allowed.
        :param data: A dictionary. Alternatively, **data** can be omited and
            the :meth:`set`` method can be used.

        Example::

            >>> data = Thermo()
            >>> data.set(N_T, 280)
            >>> data.set(N_P, 75000)

            # Is equivalent to...
            >>> data2 = Thermo(data={N_T:280, N_P:75000})

            # Let's check...
            >>> data.get(N_T) == data2.get(N_T)
            True
            >>> data.get(N_P) == data2.get(N_P)
            True

        """
        self._data = {}
        self._shape = None
        if data is not None:
            assert isinstance(data, dict), "data must be a dictionnary."
            for key in data:
                self.set(key, data[key])

        assert hydrometeors is None or isinstance(hydrometeors, list), \
            "hydrometeors must be None or a list"
        self._hydrometeors = hydrometeors
        if self._hydrometeors is not None:
            for h in self._hydrometeors:
                assert h in (['v'] + Thermo._known_liquid_hydrometeors + Thermo._known_solid_hydrometeors), \
                    "hydrometeors " + str(h) + " is not known."
            assert len(set(self._hydrometeors)) == len(self._hydrometeors), \
                "hydrometeors components must appear only once."

        self._allKnownMethods = [(N_Theta, [N_T, N_P], self.T_P2Theta),
                                 (N_T, [N_Theta, N_P], self.Theta_P2T),
                                 (N_ThetaV, [N_Theta, N_rv, N_rt], self.Theta_rv_rt2ThetaV),
                                 (N_ThetaV, [N_Theta, N_qv, N_qt], self.Theta_qv_qt2ThetaV),
                                 (N_TV, [N_ThetaV, N_P], self.Theta_P2T),
                                 (N_ThetaV, [N_TV, N_P], self.T_P2Theta),
                                 (N_ff, [N_u, N_v], self.u_v2ff),
                                 (N_dd, [N_u, N_v], self.u_v2dd),
                                 (N_rho, [N_R, N_T, N_P], self.R_T_P2rho),
                                 (N_rhov, [N_rho, N_qv], self.rho_qv2rhov),
                                 (N_rhod, [N_Rstar, N_T, N_P], self.R_T_P2rho),
                                 (N_qci, [N_qc, N_qi], self.qc_qi2qci),
                                 (N_esatw, [N_T], self.T2esatw),
                                 (N_esati, [N_T], self.T2esati),
                                 (N_rsatw, [N_esatw, N_P], self.e_P2rv),
                                 (N_rsati, [N_esati, N_P], self.e_P2rv),
                                 (N_rv, [N_e, N_P], self.e_P2rv),
                                 (N_qv, [N_e, N_P, N_qliquid, N_qice], self.e_P_qliquid_qice2qv),
                                 (N_e, [N_Huw, N_esatw], self.Huw_esatw2e),
                                 (N_e, [N_Hui, N_esati], self.Hui_esati2e),
                                 (N_e, [N_rv, N_P], self.rv_P2e),
                                 (N_e, [N_qv, N_qt, N_P], self.qv_qt_P2e),
                                 (N_Huw, [N_e, N_esatw], self.e_esatw2Huw),
                                 (N_Hui, [N_e, N_esati], self.e_esati2Hui),
                                 (N_Td, [N_e], self.Td2e),
                                 (N_R, [N_qv, N_qliquid, N_qice], self.qv_qliquid_qice2R),
                                 (N_Rstar, [N_rv], self.rv2Rstar),
                                 (N_qt, [N_qv, N_qliquid, N_qice], self.qv_qliquid_qice2qt),
                                 (N_rt, [N_rv, N_rliquid, N_rice], self.rv_rliquid_rice2rt),
                                 (N_TV, [N_qv, N_qliquid, N_qice, N_T], self.qv_qliquid_qice_T2TV),
                                 (N_Cp, [N_qv, N_qliquid, N_qice], self.qv_qliquid_qice2Cp),
                                 (N_Cph, [N_rv, N_rliquid, N_rice], self.rv_rliquid_rice2Cph),
                                 ]

        if self._hydrometeors is not None:
            for h in self._hydrometeors + ['t']:
                self._allKnownMethods.extend([('q' + h, ['r' + h, N_rt], self.rx_rt2qx),
                                              ('r' + h, ['q' + h, N_qt], self.qx_qt2rx)])
            qlist = ['q' + h for h in self._hydrometeors]
            rlist = ['r' + h for h in self._hydrometeors]
            self._allKnownMethods.extend([(N_qliquid, qlist, self.q2qliquid),
                                          (N_qice, qlist, self.q2qice),
                                          (N_rliquid, rlist, self.r2rliquid),
                                          (N_rice, rlist, self.r2rice)])

    @property
    def hydrometeors(self):
        """The list of hydrometeors."""
        return self._hydrometeors

    def _compute_new_T(self, target, precision):
        """
        This internal method computes the temperature obtained after exchanges
        with vapour target is a dictionary containing mixing ratio after changes
        method returns **T**.

        see the :meth:`evaporate` method for a description of **precision**
        """
        T = self.get(N_T)
        r = {s: self.get('r' + s) for s in self.hydrometeors}
        for s in self.hydrometeors:
            if s != 'v' and s not in target:
                raise ValueError(str(s) + "must be in target.")
        assert 'v' not in target, 'v must not be in target'

        for i, s in enumerate(target):  # We loop over all the hydrometeors
            if s in self._known_liquid_hydrometeors:
                Cx = csts.Cl
                L0 = csts.Lv0
            elif s in self._known_solid_hydrometeors:
                Cx = csts.Ci
                L0 = csts.Ls0
            else:
                raise ValueError("Specie '" + str(s) + "' not known.")
            if i == 0 or precision == 'full':
                L_before = L0 + (csts.Cpv - Cx) * (T - csts.T0)
                Cph_before = csts.Cpd + csts.Cpv * r['v']
                Cph_after = csts.Cpd + csts.Cpv * (r['v'] + r[s] - target[s])
                for k, v in r.iteritems():
                    if k == 'v':
                        pass
                    elif k in self._known_liquid_hydrometeors:
                        Cph_before += v * csts.Cl
                        if k == s:
                            Cph_after += target[s] * csts.Cl
                        else:
                            Cph_after += v * csts.Cl
                    elif k in self._known_solid_hydrometeors:
                        Cph_before += v * csts.Ci
                        if k == s:
                            Cph_after += target[s] * csts.Ci
                        else:
                            Cph_after += v * csts.Ci
                    else:
                        raise ValueError("Specie '" + str(k) + "' not known.")
            elif precision != 'full':
                L_before = L0 + (csts.Cpv - Cx) * (self.get(N_T) - csts.T0)
            if precision == 'standard':
                # Explicit calcul, L and Cph are assumed to be constant during the transformation
                # (they are taken equal to their initial values)
                T = T + L_before * (target[s] - r[s]) / Cph_before
            elif precision == 'full':
                # Cp dT = Lv dqv integrated on the transformation. We have L+/L-=Cp-/Cp+
                T = Cph_before / Cph_after * (L0 / (csts.Cpv - Cx) - csts.T0 + T) + csts.T0 - L0 / (csts.Cpv - Cx)
            else:
                raise ValueError("Value of precision not known: " + str(precision))
            r['v'] = r['v'] + r[s] - target[s]
            r[s] = target[s]
        return T

    def evaporate(self, species, extra_variables=[], precision='full'):
        """
        This method returns a Thermo instance in which **species** are evaporated
        (others are kept untouched).

        Only T and mixing ratios are set in the new object unless
        **extra_variables** is set. **extra_variables** is a list of variables
        to get from the current object and to set in the new one. If
        **extra_variables** contain thermodynamic variables, they can be in
        contradiction with the evaporated state;
        this variable is only useful for variables not impacted by evaporation
        (P, wind...) **precision** can be:

            * ``full``: Cp and Lv values are evolving during the evaporation. Species
              are evaporated in the order given by **species**. Normally, the order
              does not matter.
            * ``standard``: Cp and Lv are computed from the initial state and kept
              constant during evaporation

        """
        result = self.__class__(hydrometeors=self.hydrometeors)
        target = {s: self.get('r' + s) * (0. if s in species else 1.)
                  for s in [k for k in self.hydrometeors if k != 'v']}
        T = self._compute_new_T(target, precision)
        rv = self.get(N_rt)
        for s in target:
            result.set('r' + s, target[s])
            rv = rv - target[s]
        result.set(N_T, T)
        result.set(N_rv, rv)

        for var in extra_variables:
            result.set(var, self.get(var))

        return result

    def adjust(self, species, mix_rule=None, iteration=1, extra_variables=[], precision='full'):
        """
        This method returns a :class:`Thermo` instance in which **species** are
        adjusted with respect to water vapour (others are kept untouched).

        **species** can only contain 'c' and/or 'i':

            * if ``species == ['c']``, we adjust with respect to liquid water
              (mix_rule must be None)
            * if ``species == ['i']``, we adjust with respect to ice (mix_rule
              must be None)
            * if ``c`` and ``i`` are in **species**, mix_rule cannot be ``None``,
              we have to choose an adjustment way:

                * ``same``, we keep the same partition between liquid and ice
                  (entirely liquid if no previous cloud)
                * ``0T-20``, we use a linear partition between liquid and ice
                  according to temperature between 0 and -20

        Liquid/ice fraction is used to compute a weight-averaged saturation mixing
        ratio. Adjustment is an iterative process, **iteration** gives the number of
        iteration we must perform.

        Only T and mixing ratios are set in the new object unless **extra_variables**
        is set. **extra_variables** is a list of variables to get from the
        current object and to set in the new one. If **extra_variables** contains
        thermodynamic variables, they can be in contradiction with the adjusted state;
        this variable is only useful for variables not impacted by adjustment (P, wind...)
        **precision** can be:

            * ``full``: Cp and Lv values are evolving during the evaporation. Species
              are evaporated in the order given by **species**. Normally, the
              order does not matter.
            * ``standard``: Cp and Lv are computed from the initial state and kept
              constant during evaporation

        """
        assert mix_rule in [None, 'same', '0T-20'], "mix_rule must be None, 'same' or '0T-20'"
        if species == ['c']:
            assert mix_rule in [None], "mix_rule must be None or 'same' when adjusting only 'c'"
        elif species == ['i']:
            assert mix_rule in [None], "mix_rule must be None or 'same' when adjusting only 'i'"
        elif set(species) == set(['c', 'i']):
            assert mix_rule in ['same', '0T-20'], "mix_rule must be 'same' or '0T-20'"
        else:
            raise ValueError("species can only contain 'c' and/or 'i'.")

        tempo = self.__class__(hydrometeors=self.hydrometeors)
        for var in [N_T, N_P] + ['r' + s for s in self.hydrometeors]:
            tempo.set(var, self.get(var))

        for myiter in range(iteration):
            if species == ['c']:
                ice_fraction = 0.
                cond = tempo.get(N_rc)
                rsat = tempo.get(N_rsatw)
            elif species == ['i']:
                ice_fraction = 1.
                cond = tempo.get(N_ri)
                rsat = tempo.get(N_rsati)
            elif set(species) == set(['c', 'i']):
                if mix_rule == 'same':
                    ice_fraction = np.where(tempo.get(N_rc) + tempo.get(N_ri) > 0.,
                                            tempo.get(N_ri) / (tempo.get(N_rc) + tempo.get(N_ri)), 0.)
                else:
                    ice_fraction = (csts.T0 - tempo.get(N_T)) / 20.
                    ice_fraction = np.maximum(0., np.minimum(1, ice_fraction))
                cond = tempo.get(N_rc) + tempo.get(N_ri)
                rsat = ice_fraction * tempo.get(N_rsati) + (1. - ice_fraction) * tempo.get(N_rsatw)
            else:
                raise ValueError("species can only contain 'c' and/or 'i'.")

            diff = np.maximum(-cond, tempo.get(N_rv) - rsat)  # if rv>rsat, diff is the part of rv that we
            #                                                               must condensate on cloud
            #                                                   if rv<rsat, diff is the content we must take
            #                                                               to cloud to put it back to vapor
            #                                                               (hence the maximum)
            cond = cond + 0.8 * diff  # Tuning factor to limit oscilations and non-convergence
            target = {s: (tempo.get('r' + s)
                          if s not in species else cond * (ice_fraction if s == 'i' else (1 - ice_fraction)))
                      for s in [k for k in tempo.hydrometeors if k != 'v']}
            T = tempo._compute_new_T(target, precision)

            result = self.__class__(hydrometeors=self.hydrometeors)
            rv = self.get(N_rt)
            for s in target:
                result.set('r' + s, target[s])
                rv = rv - target[s]
            result.set(N_T, T)
            result.set(N_rv, rv)

            for var in extra_variables + ([N_P] if myiter < iteration - 1 else []):
                result.set(var, self.get(var))

            tempo = result  # in case of other iteration

        return result

    def list(self):
        """Iterator over all already known parameters"""
        return self._data.keys()

    def set(self, parameter, value):
        """Sets or updates a value for parameter."""
        valueShape = (1,) if not isinstance(value, np.ndarray) else value.shape
        if self._shape is None:
            self._shape = valueShape
        assert self._shape == valueShape, "All values must have the same shape"
        self._data[parameter] = value

    def changeOrder(self, parameter, increasing=True):
        """Modify the order of all parameters to sort according to parameter."""
        p = self.get(parameter).argsort()
        if not increasing:
            p = p[::-1]
        for param in self.list():
            self.set(param, self.get(param)[p])

    def get(self, parameter, methods=None):
        """Gets or computes a value for parameter"""
        if parameter not in self._data:
            # To allow a better result, one must begin by the more accurate methods
            listOfMethods = methods if methods is not None else self._allKnownMethods
            for iMethod, method in enumerate(listOfMethods):
                if parameter not in self._data and method[0] == parameter:
                    # We try to call this method.
                    # To prevent cyclic calls, we give it the remaining available methods
                    # (excluding recursively the current one)
                    m = list(listOfMethods)
                    m.pop(iMethod)
                    try:
                        logger.debug("Trying to call " + method[2].__name__)
                        args = []
                        for dep in method[1]:
                            logger.debug("Trying to obtain " + dep + " for calling " + method[2].__name__)
                            args.append(self.get(dep, m))
                            logger.debug(dep + " obtained for calling " + method[2].__name__)
                        logger.debug(parameter + " is computed by " + method[2].__name__)
                        self._data[parameter] = method[2](*list(args))
                    except ThermoError:
                        logger.debug("Call to " + method[2].__name__ + " not possible")
                        pass
        if parameter in self._data:
            return self._data[parameter]
        else:
            raise ThermoError("Parameter '" + parameter + "' is not set and cannot be computed.")

    # Thermodynamic functions ######################################################

    @staticmethod
    def Td2e(Td):
        """Computes e from Td"""
        # By definition Td is computed over liquid water (Tf -frost temperature- is computed over ice)
        return Thermo.T_phase2esat(Td, 'L')

    @staticmethod
    def T_P2Theta(T, P):
        """Computes Theta from T and P."""
        return T * (csts.P0 / P)**(csts.Rd / csts.Cpd)

    @staticmethod
    def Theta_P2T(Theta, P):
        """Computes T from Theta and P."""
        return Theta / (csts.P0 / P)**(csts.Rd / csts.Cpd)

    @staticmethod
    def Theta_rv_rt2ThetaV(Theta, rv, rt):
        """Computes Theta v from Theta, rv and rt."""
        return Theta * ((1 + rv * csts.Md / csts.Mv) / (1 + rt))

    @staticmethod
    def Theta_qv_qt2ThetaV(Theta, qv, qt):
        """Computes Theta v from Theta, qv and qt."""
        return Theta * (1 + qv * csts.Md / csts.Mv - qt)

    @staticmethod
    def T_phase2esat(T, phase):
        """
        Computes the water vapor pressure at saturation from T and phase.
        Phase is given by:

            * 'L', 'E' or 'W' for liquid water
            * 'S', 'G' or 'I' for ice
        """
        phase_liquid = ['L', 'W', 'E']
        phase_ice = ['S', 'I', 'G']
        if phase in phase_liquid:
            return np.exp(csts.alpw - csts.betaw / T - csts.gamw * np.log(T))
        elif phase in phase_ice:
            esat_w = np.exp(csts.alpw - csts.betaw / T - csts.gamw * np.log(T))
            esat_i = np.exp(csts.alpi - csts.betai / T - csts.gami * np.log(T))
            return np.where(T > 273.15, esat_w, esat_i)
        else:
            raise ValueError("The phase argument must be L, W, E, S, I or G.")

    @staticmethod
    def T2esatw(T):
        """Computes the water vapor pressure at saturation with respect to liquid water from T"""
        return Thermo.T_phase2esat(T, 'L')

    @staticmethod
    def T2esati(T):
        """Computes the water vapor pressure at saturation with respect to ice from T"""
        return Thermo.T_phase2esat(T, 'I')

    @staticmethod
    def Huw_esatw2e(Huw, esatw):
        """Computes e from Huw and esatw"""
        return Huw * esatw / 100.

    @staticmethod
    def Hui_esati2e(Hui, esati):
        """Computes e from Hui and esati"""
        return Hui * esati / 100.

    @staticmethod
    def e_P_qliquid_qice2qv(e, P, qliquid, qice):
        """Computes qv from e and P."""
        return (1 - qliquid - qice) / (1 + (csts.Rv / csts.Rd) * (P / e - 1))

    @staticmethod
    def e_P2rv(e, P):
        """Computes rv from water e and P."""
        return csts.Rd / csts.Rv * e / (P - e)

    @staticmethod
    def rv_P2e(rv, P):
        """Computes e from rv and P."""
        return P * rv / (csts.Rd / csts.Rv + rv)

    @staticmethod
    def qv_qt_P2e(qv, qt, P):
        """Computes e from qv, qt and P."""
        return P * qv / ((1 - qt) * csts.Rd / csts.Rv + qv)

    @staticmethod
    def e_esatw2Huw(e, esatw):
        """Computes Huw from e and esatw."""
        return 100 * e / esatw

    @staticmethod
    def e_esati2Hui(e, esati):
        """Computes Hui from e and esati."""
        return 100 * e / esati

    def q2qliquid(self, *q):
        """Computes qliquid from specific contents"""
        qliquid = 0.
        for h, v in zip(self.hydrometeors, q):
            if h in Thermo._known_liquid_hydrometeors:
                qliquid += v
        return qliquid

    def q2qice(self, *q):
        """Computes qice from specific contents"""
        qice = 0.
        for h, v in zip(self.hydrometeors, q):
            if h in Thermo._known_solid_hydrometeors:
                qice += v
        return qice

    def r2rliquid(self, *r):
        """Computes rliquid from mixing ratios"""
        rliquid = 0.
        for h, v in zip(self.hydrometeors, r):
            if h in Thermo._known_liquid_hydrometeors:
                rliquid += v
        return rliquid

    def r2rice(self, *r):
        """Computes rice from mixing ratios"""
        rice = 0.
        for h, v in zip(self.hydrometeors, r):
            if h in Thermo._known_solid_hydrometeors:
                rice += v
        return rice

    @staticmethod
    def qv_qliquid_qice2qt(qv, qliquid, qice):
        """Computes qt from qv, qliquid and qice."""
        return qv + qliquid + qice

    @staticmethod
    def rv_rliquid_rice2rt(rv, rliquid, rice):
        """Computes rt from rv, rliquid and rice."""
        return rv + rliquid + rice

    @staticmethod
    def qv_qliquid_qice2R(qv, qliquid, qice):
        """Computes R from specific contents"""
        return csts.Rd + qv * (csts.Rv - csts.Rd) - qliquid * csts.Rd - qice * csts.Rd

    @staticmethod
    def rv2Rstar(rv):
        """Computes Rstar from water vaport mixing ratio"""
        return csts.Rd + rv * csts.Rv

    @staticmethod
    def R_T_P2rho(R, T, P):
        """
        Computes the volumic mass of wet air (rho) from R, T and P
        or the volumic mass of dry air (rhod) from Rstar, T and P.
        """
        return P / (R * T)

    @staticmethod
    def rho_qv2rhov(rho, qv):
        """Computes rhov from rho and qv."""
        return rho * qv

    @staticmethod
    def qc_qi2qci(qc, qi):
        """Computes specific total cloud condensate from qc and qi."""
        return qc + qi

    @staticmethod
    def u_v2dd(u, v):
        """Computes wind direction (in degrees) from zonal and meridional wind speed."""
        return np.arctan2(-u, -v) * 180. / np.pi

    @staticmethod
    def u_v2ff(u, v):
        """Computes wind force (in m/s) from zonal and meridional wind speed."""
        return np.sqrt(u**2 + v**2)

    @staticmethod
    def rx_rt2qx(rx, rt):
        """Computes specifc content from mixing ratio and total mixing ratio."""
        return rx / (1 + rt)

    @staticmethod
    def qx_qt2rx(qx, qt):
        """Computes mixing ratio from specific content and total specific content."""
        return qx / (1 - qt)

    @staticmethod
    def qv_qliquid_qice_T2TV(qv, qliquid, qice, T):
        """Computes the virtual temperature from qv, qliquid, qice and T."""
        return (1 + qv * (csts.Rv / csts.Rd - 1) - qliquid - qice) * T

    @staticmethod
    def qv_qliquid_qice2Cp(qv, qliquid, qice):
        """Computes the heat capacity at constant pressure from specific contents."""
        return csts.Cpd * (1 - qv - qliquid - qice) + csts.Cpv * qv + csts.Cl * qliquid + csts.Ci * qice

    @staticmethod
    def rv_rliquid_rice2Cph(rv, rliquid, rice):
        """Computes the specific heat capacity at constant pressure from mixing-ratios."""
        return csts.Cpd + csts.Cpv * rv + csts.Cl * rliquid + csts.Ci * rice


if __name__ == '__main__':
    import doctest
    doctest.testmod()
