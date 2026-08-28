# -*- coding: utf-8 -*
"""
The "s2m" application
=====================

SAFRAN-SURFEX/Crocus-MEPRA simulations in a standard "SAFRAN-massif" geometry.

Associated  configurations:

* **reanalysis**: production of S2M-like reanalyses (SAFRAN forcings and geometries)

.. automodule:: vortex_cen.s2m.reanalysis

* **reforecast**: production of a S2M reforecast for the training of operationnal new snow forecasts
  post-processing algorithm

.. automodule:: vortex_cen.s2m.reforecast

* **oper (alp, pyr, cor, mac, vog, jur)**: S2M operational model chain
  - oper contains the drivers (common to all domains) and "prepsafran" jobs (common to all domains)
  - [alp|pyr|cor|mac|vog|jur] contains the domain-specific jobs and configuraiton files

.. automodule:: vortex_cen.s2m.oper

"""
