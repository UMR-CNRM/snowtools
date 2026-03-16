Common files around snow cover simulations
===========================================
Namelist
--------
The namelist is a text file describing the main characteristics of the simulation.
There are infos on the soil and vegetation (for example in NAM_DATA_ISBA and NAM_ISBA), there are some choices for physical options (NAM_ISBA_SNOWN), etc.

You should not try to create a namelist from scratch but use and modify an existing namelist.
There are some namelists in ``snowtools/snowtools/DATA`` or in ``snowtools/snowtools/tests/namelists``.

It is interesting to sort your namelist in order to compare it to others. You can use ``namelist_sort.py`` which is in ``snowtools/cenutils``.
The command is

.. code-block:: bash

   python namelist_sort.py OPTIONS.nam -o OPTIONS_SORT.nam

The NAM_IO_OFFLINE options in namelist
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

``XTSTEP_OUTPUT`` defines the timestep (in seconds) of the outputs. ``XTSTEP_OUTPUT=21600`` means that you'll have diagnostics every 6 hours in your PRO file.

It is possible to compress the netcdf PRO file: ``NOUTPUT_NETCDF = 4`` (the output file is a NetCDF4-classic format, the default is NetCDF3 format) and ``NCOMPRESS_NETCDF = 1`` (the level of compression is 1, this is the default).

By default, the FORCING file is read every timestep. In order to decrease the I/O time, you can choose to read all forcing datas in one time by setting ``NB_READ_FORC = 1``.


Forcing files
-------------

SAFRAN reanalysis is regularly updated.

The directory where you should take SAFRAN meteorological forcing files depend on which slopes you need and on the file system you are using.

**At CEN**, SAFRAN files are stored on the following directories :

- ``/rd/cenfic3/cenmod/era40/vortex/s2m/DOM_allslopes/reanalysis/meteo`` contains SAFRAN reanalysis for all massifs; 0°, 20° and 40° slope values, and 8 aspects
- ``/rd/cenfic3/cenmod/era40/vortex/s2m/DOM_flat/reanalysis/meteo`` SAFRAN reanalysis for all massifs over flat area
- ``/rd/cenfic3/cenmod/era40/vortex/s2m/postes/reanalysis/meteo`` contains SAFRAN reanalysis for all stations (include topographic corrections of radiations)

where ``DOM`` represents the spatial domain which can be "alp" (Alps), "pyr" (Pyrénées), "cor" (Corsica)

For use **at Toulouse**, get it from Hendrix:

From hendrix to any informatic resource in Toulouse, note that it is recommended to get SAFRAN files with vortex. See ``ßcripts/extract/vortex/get_reanalysis.py``.
You should not develop your own transfer script.

On supercomputer, use the combination of ``-m s2m`` (or ``-m safran`` if you want to define your own slopes), ``-r region`` and ``-f reanalysis2020.2@lafaysse`` to get access to reanalysis with s2m command. Regions have to be chosen in [alp_allslopes, alp_flat, pyr_allslopes, pyr_flat, cor_allslopes, cor_flat, postes].

For information, SAFRAN are stored in the following directories:

- ``/home/lafaysse/vortex/s2m/DOM_allslopes/reanalysis/meteo`` contains SAFRAN reanalysis for all massifs; 0°, 20° and 40° slope values, and 8 aspects
- ``/home/lafaysse/vortex/s2m/DOM_flat/reanalysis/meteo`` contains SAFRAN reanalysis for all massifs over flat area
- ``/home/lafaysse/vortex/s2m/postes/reanalysis/meteo`` contains SAFRAN reanalysis for all stations (include topographic corrections of radiations)

If you have to create your own forcing files, please have a look to the ``FORCING_test_base.nc`` file provided in the ``snowtools/DATA`` folder and follow the same structure (same variable names and ranks, same dimensions). All variables are mandatory. CO2air and Wind_DIR have no impact in the case of snow and can be fixed to a constant value. If you do not have distinct data for diffuse and direct solar radiations, you can put everything in the direct radiation for applications not sensitive to this separation (with Crocus, only TARTES and MEB options are sensitive but they are not activated by default).

If you have datas from an observation site in .csv or in .txt format and if you want to create only one or a few FORCING files, then have a look to ``snowtools/scripts/create_forcing/Template_creation_FORCING.py``. If your datas are available only every 3h for example, you have to set the variable FORC_TIME_STP to 10800 instead of 3600 in the FORCING file.

Other files
-----------

.. todo:: Document common localizations and access mode for the different files needed by SURFEX simulations and processing around.
