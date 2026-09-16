# -*- coding: utf-8 -*
"""
The s2m "oper" configuration
============================

Operational SAFRAN-SURFEX/Crocus-MEPRA (S2M) model chain.
The actual alp, pyr, cor, mac, vog, jur operational configurations derive from the "oper" configuration :
* "oper" contains the drivers (common to all domains) and "prepsafran" jobs (common to all domains)
* [alp|pyr|cor|mac|vog|jur] contains the domain-specific jobs and configuration files


Workflow :
----------

1. SAFRAN inputs

* prepsafran_analyse : Generation of SAFRAN guess files from NWP analyses

* prepsafran_prevision : Generation of SAFRAN guess files from NWP forecasts

* prepsafran_reanlayse : Generation of archives of SAFRAN guess files from the previous 01/08 until the 18th day of each month (runs on the 22nd day of the month)

* [refill_guess_safran] : Offline generation of an archive of SAFRAN guess files to restart the real-time chain


2. SAFRAN

* safran_analyse_ensemble : Ensemble (runs 3H, 6H, 9H) or detterministic (run 12H) SAFRAN analyses (generation of "flat" FORCING files)

* safran_analysei_prevision : Ensemble SAFRAN forecasts (generation of "flat" FORCING files)

3. SURFEX

* [cold_start_surfex] : Deterministic SURFEX/Crocus snowpack simulation from the previous 01/08 with "custom" initial condition after a geometry or SURFEX version update. For example, to start the 2026 new S2M oper chain with initial conditions from the 2026 release of the ERA5-S2M reanalysis valid on 1st August 2025 :

.. code-block::

    s2m_oper -j cold_start_surfex -d 2026091412 -a prep_xpid=release_2026 prep_user=vernaym prep_datevalidity=2025080106 prep_block=offline prep_vapp=s2m prep_vconf=reanalysis


* ensemble_surfex_tasks_analysis : Update raw SAFRAN forcing files (add slopes and solar masks) + ensemble SURFEX/Crocus snowpack simulations

* ensemble_surfex_tasks_forecast : Update raw SAFRAN forcing files (add slopes and solar masks) + ensemble SURFEX/Crocus snowpack simulations

* monthly_surfex_reanalysis : Deterministic SURFEX/Crocus snowpack simulation from the previous 01/08 until the 18th day of each month (runs on the 22nd day of the month)

* monthly_surfex_reanalysis_sytron : Deterministic SURFEX/Crocus snowpack simulation from the previous 01/08 with the SYTRON snow drift module activated

4. Post-processing

* hydro_task : Hydrological post-processing

* postprocess_forecast : Post-processing for the 4 seasons bulletin
"""
