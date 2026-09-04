# -*- coding: utf-8 -*

"""
The Crocus 'deterministic' configuration
========================================

Any deterministic SURFEX/Crocus simulation involving a single meteorological FORCING and Crocus configuration

Workflow:
^^^^^^^^^

1. Generation of a PGD.nc file with pgd.job

2. Generation of an init_TG file with init_TG.job

3. Generation of a PREP.nc file with prep.job

4. Generation of a spinup with spinup.job. If necessary, the PGD.nc and PREP.nc files will be created.

5. Launch a SURFEX/Crocus simulation with surfex.job. If necessary, the PGD.nc and PREP.nc files will be created.

"""
# Drivers:
#     * init_TG.py
#     * pgd.py
#     * prep.py
#     * surfex.py
