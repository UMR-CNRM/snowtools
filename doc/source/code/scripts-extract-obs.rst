Extract and post-processing observations
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Python class for accessing BDClim data
""""""""""""""""""""""""""""""""""""""

A python class is described below for generating sql queries to bdclim. If you are familiar with SQL, it may also be interesting to use directly the ``psycopg2`` python module as Meteo-France database are managed through PostgreSQL.

Please read the documentation of BDClim on http://tipige.meteo.fr/Climsol/

For additional examples of extractions on BDClim, please have a look to `Existing scripts for querying on BDClim`_ that may provide useful examples. Note that they may be easily adjusted if they does not immediately match your needs, have a look to their code !

.. automodule:: scripts.extract.obs.bdquery
   :members:

Existing scripts for querying on BDClim
"""""""""""""""""""""""""""""""""""""""

An overview of existing scripts is proposed below. They all extract data based on parameters specifying time period (at least) and produces CSV file(s) as an output.

.. include:: ./autoscripts/scripts--extract--obs--extract_obs.rst

.. include:: ./autoscripts/scripts--extract--obs--extract_obs_allhtn.rst

.. include:: ./autoscripts/scripts--extract--obs--extract_obs_meteo.rst

.. include:: ./autoscripts/scripts--extract--obs--extract_obs_meteo_mb.rst

.. todo:: 
   Some scripts related to observation extraction and post-processing are not documented. Have a look in the corresponding folder !
