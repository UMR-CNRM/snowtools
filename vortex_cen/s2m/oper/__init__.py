# -*- coding: utf-8 -*
"""
The s2m "oper" configuration
============================

Operational SAFRAN-SURFEX/Crocus-MEPRA (S2M) model chain.

Workflow :
----------

1. SAFRAN inputs

    * prepsafran_analyse : Generation of SAFRAN guess files from NWP analyses

    * prepsafran_prevision : Generation of SAFRAN guess files from NWP forecasts

    * prepsafran_reanlayse : Generation of archives of SAFRAN guess files from the previous 01/08

    * [refill_guess_safran] : Offline generation of an archive of SAFRAN guess files to restart the real-time chain


2. SAFRAN

    * safran_analyse_ensemble : Ensemble (runs 3H, 6H, 9H) or detterministic (run 12H) SAFRAN analyses
    (generation of "flat" FORCING files)

    * safran_analysei_prevision : Ensemble SAFRAN forecasts (generation of "flat" FORCING files)

3. SURFEX

    * ensemble_surfex_tasks_analysis : Update raw SAFRAN forcing files (add slopes and solar masks) + ensemble
    SURFEX/Crocus snowpack simulations

    * ensemble_surfex_tasks_forecast : Update raw SAFRAN forcing files (add slopes and solar masks) + ensemble
    SURFEX/Crocus snowpack simulations

    * monthly_surfex_reanalysis : Deterministic SURFEX/Crocus snowpack simulation from the previous 01/08

    * monthly_surfex_reanalysis_sytron : Deterministic SURFEX/Crocus snowpack simulation from the previous 01/08 with
    the SYTRON snow drift module activated

4. Post-processing

    * hydro_task : Hydrological post-processing

    * postprocess_forecast : Post-processing for the 4 seasons bulletin
"""
