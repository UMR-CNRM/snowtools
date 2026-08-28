# -*- coding: utf-8 -*
"""
The s2m "reforecast" configuration
==================================

Production of a SAFRAN-SURFEX/Crocus-MEPRA (S2M) reforecast for the training of operationnal new snow forecasts
post-processing algorithm

Workflow :
----------

1. SAFRAN inputs

    * prepsafran : Generation of SAFRAN guess files


2. SURFEX inputs

    * safran : Generation of "raw" SAFRAN meteorological forcing files ("flat" geometry and 1 "postes" FORCING file per domain)

    * [init_TG] : Generation of an initial ground temperature]

    * dailyprep : Generation of daily initial snowpack state from the SAFRAN reanalysis

    * [pgd] : Generation of ground physiography

3. FORCING modification

    * add_slopes : Add slopes and aspects to the raw "flat" SAFRAN meteorological forcing files

    * concatenation_postes : Concatenate "postes" forcing files from all domains into a single FORCING file and add solar masks

4. SURFEX

    * surfex_massifs : Generation of SURFEX/Crocus snowpack simulations on an "allslopes" geometry

    * surfex_postes : Generation of SURFEX/Crocus snowpack simulations on the "postes" geometry

"""
