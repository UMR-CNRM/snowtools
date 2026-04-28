#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
This script needs a shapefile (in Lambert 93 coordinates) of points.
It is useful for simulations on several differents points.

If you want a simulation on a 2D domain delimited by a shapefile:
see shapefile2NetCDF_2D.py instead.

General documentation of shapefile2NetCDF script
-------------------------------------------------
This script creates a NetCDF with the same points than in shapefile.
The NetCDF file is necessary to launch Surfex-Crocus simulations.
It allows to interpolate FORCING values from SAFRAN reanalysis to your shapefile points.
It also allows to take into account the solar mask from mountains around.

Precisely, this script writes skylines in SKYLINE.xml file.
These skylines are necessary for the --addmask option in SURFEX Crocus simulations.
This script also allows to keep the skyline views if you want to check them.

Reminder: if the points in your project are planned to last long, copy them in snowtools/DATA/METADATA.xml

EXAMPLES OF USE
^^^^^^^^^^^^^^^

.. code-block::

   python3 shapefile2NetCDF.py path_shapefile station_name_in_shapefile station_id_in_shapefile
           [--name_alt alti_name_in_shapefile] [-o path_name_of_NetCDF_output] [--MNT_alt path_of_MNT_altitude]
           [--list_skyline all or 1 5 6 if you want skyline for your id_station number 1, 5 and 6]
   
   python3 shapefile2NetCDF.py /home/fructusm/Téléchargements/Plots2020/plots codeplot idplot --name_alt alti
   
   python3 shapefile2NetCDF.py /home/fructusm/Téléchargements/Plots2020/plots codeplot idplot --name_alt alti
           --confirm_overwrite (if you have already work on this project)
           
   python3 shapefile2NetCDF.py /home/fructusm/Téléchargements/Plots2020/plots codeplot idplot --name_alt alti
           --list_skyline 1 34 47 (in folder output, you'll have skylines for stations 1, 34 and 47)
           
   python3 shapefile2NetCDF.py /home/fructusm/Téléchargements/Plots2021/PlotsMaJ2021 codeplot idplot 

   python3 shapefile2NetCDF.py /home/fructusm/Bureau/Shapefile_simu/cn_maille_points/cn_maille_points cd50m ORIG_FID

   python3 shapefile2NetCDF.py /home/fructusm/Travail_Orchamp/Orchamp_nouveaux_sites_2022/Plots_new_2022 codeplot
           idplot

EXAMPLES OF USE OF THE NETCDF FILE IN LOCAL SIMULATION
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Get the interpolation of the FORCING file, interpol_FORCING:

.. code-block:: bash

   s2m research -f path_FORCING -b begin_date -e end_date -r path_netcdf.nc -o output_name -g --extractforcing

Obtain the PRO file for the shapefile points:

.. code-block:: bash

   s2m research -f path_interpol_FORCING -b begin_date -e end_date -o output_name_pour_PRO -g --addmask

Example:

.. code-block:: bash

   s2m research -f /rd/cenfic3/cenmod/era40/vortex/s2m/alp_flat/reanalysis/meteo/FORCING_2017080106_2018080106.nc
                -b 20170801 -e 20180801 -r /home/fructusm/git/snowtools_git/interpolation/NetCDF_from_shapefile.nc
                -o output_test_s2m -g --extractforcing
                
   s2m research -f /home/fructusm/OUTPUT_et_PRO/output_test_s2m/meteo/FORCING_2017080106_2018080106.nc
                -b 20170801 -e 20180801 -o output_test_s2m_Safran -g --addmask


EXAMPLES OF USE OF THE NETCDF FILE IN HPC SIMULATION
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: python

   s2m research -b 19580801 -e 20200801 -m s2m -f reanalysis2020.2@lafaysse
                -r alp_flat:orchamp:/home/cnrm_other/cen/mrns/fructusm/NetCDF_from_shapefile.nc
                -o TEST1 -n OPTIONS_V8.1_NEW_OUTPUTS_NC_reanalysis.nam -g --addmask -a 400


HPC SIMULATION: CLIMATE SIMULATIONS
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Forcing files from SMHI_RCA4_MOHC_HadGEM2_ES_RCP85 (for example) are in Raphaelle Samacoits folders

.. code-block:: bash

   s2m research -b 20100801 -e 20990801 -m adamont -f CLMcom_CCLM4_8_17_CNRM_CERFACS_CNRM_CM5_RCP45@samacoitsr
                -r alp_flat:orchamp:/home/cnrm_other/cen/mrns/fructusm/NetCDF_from_shapefile.nc -o TEST_Raphaelle
                -n OPTIONS_V8.1_NEW_OUTPUTS_NC_reanalysis.nam -x 20200801 --addmask -a 400

HPC SIMULATION: SXCEN TRANSFERT
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Transfer files from Belenos to sxcen:
Use get_reanalysis which is in snowtools/scripts/extract/vortex

On SXCEN:

.. code-block:: bash

   python3 get_reanalysis.py --geometry=orchamp --snow --xpid=SPINUP@fructusm
   python3 get_reanalysis.py --geometry=orchamp --snow --xpid=TEST_Raphaelle@fructusm
                             --byear=2010 --eyear=2099

.. warning::
   DO NOT FORGET THE SPINUP
   DO NOT FORGET THE XML FILE IN ORDER TO USE THE MASKS
   REMOVE AVALANCHES DIAG IN CSELECT OF THE NAMELIST  (in that case, massif_num is necessary)

"""
import os
import argparse
import sys
import re
import logging  # Import pour les log

from netCDF4 import Dataset
import numpy as np

# new way to import gdal (https://gdal.org/api/python_bindings.html)
# 'import gdal' is deprecated
try:
    from osgeo import gdal
except ImportError:
    import gdal

from shapely.geometry import shape
from shapely.ops import transform
from functools import partial
import pyproj

from snowtools.DATA import SNOWTOOLS_DIR
from snowtools.DATA import DIRDATADEM

# Bibliothèque ad hoc de Matthieu L pour ouvrir les shapefiles
import shapefile

# Imports pour "Skyline"
import time
import math
import matplotlib
matplotlib.use('Agg')  # has to be before matplotlib.pyplot, because otherwise the backend is already chosen.
import matplotlib.pyplot as plt

# L. Roussel 2024: fix interpolation angles in 'raster_to_points'

################################################################
# DEFAULT VALUES (but can change with options):
# MNT (30m) and name of the output NetCDF
################################################################
NetCDF_out = 'NetCDF_from_shapefile.nc'

# PATH_MNT
path_MNT_alti_defaut = DIRDATADEM + 'france_30m/DEM_FRANCE_L93_30m_bilinear.tif'
path_MNT_slope_defaut = DIRDATADEM + 'france_30m/SLP_FRANCE_L93_30m_bilinear.tif'
path_MNT_aspect_defaut = DIRDATADEM + 'france_30m/ASP_FRANCE_L93_30m_bilinear.tif'

################################################################
# Infos shapefile massif, normally stable and durable
################################################################
path_shapefile_massif = SNOWTOOLS_DIR + '/DATA/massifs'
indice_record_massif = 0 # If changes in snowtools massif, check that massif number is still record[0]

################################################################
# Log
################################################################
logger = logging.getLogger()
logger.setLevel(logging.WARNING)
console_handler = logging.StreamHandler()
console_handler.setFormatter(logging.Formatter('%(levelname)s :: %(message)s'))
console_handler.setLevel(logging.DEBUG)
logger.addHandler(console_handler)


################################################################
# Creation of data lists
################################################################
def make_dict_list(path_shapefile, id_station, nom_station, nom_alt, nom_asp, nom_slop, path_MNT_alt, path_MNT_asp,
                   path_MNT_slop):
    """
    Create a dictionary of lists from datas in shapefile and data in MNT

    :param path_shapefile: shapefile path
    :type path_shapefile: str
    :param id_station: name of parameter in shapefile for point number (each point has a unique number)
    :type id_station: str
    :param nom_station: name of parameter in shapefile for point name (each point has a unique name)
    :type nom_station: str
    :param nom_alt: if present in shapefile, name of altitude parameter. If not present, value is None.
    :type nom_alt: str
    :param nom_asp: if present in shapefile, name of aspect parameter. If not present, value is None.
    :type nom_asp: str
    :param nom_slop: if present in shapefile, name of slope parameter. If not present, value is None.
    :type nom_slop: str
    :param path_MNT_alt: MNT path for altitude
    :type path_MNT_alt: str
    :param path_MNT_asp: MNT path for aspect
    :type path_MNT_asp: str
    :param path_MNT_slop: MNT path for slopes
    :type path_MNT_slop: str

    :returns: dictionary of lists:{ 'lat': list_latitude, 'lon':list_longitude,
                                    'alt': list_altitude, 'asp': list_aspect,
                                    'm': list_massif, 'slop': list_slope,
                                    'id': list_id_station, 'nom': list_nom_station }
    """
    # Open shapefile.
    r = shapefile.Reader(path_shapefile)
    shapes = r.shapes()
    geomet = r.shapeRecords()

    # Convert from Lambert93 (EPSG 2154) to WGS84 (EPSG 4326)
    # Lambert93: coordinates for France (with Corsica), unit = meters.
    # WSG84: coordinates (lon, lat) like GPS. OK for entire world
    project_from_L93_to_WGS84 = partial(pyproj.transform, pyproj.Proj(init='epsg:2154'), pyproj.Proj(init='epsg:4326'))
    list_shape_WGS84 = [transform(project_from_L93_to_WGS84, shape(shapes[i])) for i in range(len(shapes))]

    # Little print for shapefile: names, coordinates, altitude, ...
    # Just_to_see = geomet[0].record
    # print(Just_to_see)

    # Research of fields index in shapefile
    for i in range(len(r.fields)):
        if nom_alt is not None and r.fields[i][0] == nom_alt:
            indice_record_altitude = i - 1
        if nom_asp is not None and r.fields[i][0] == nom_asp:
            indice_record_aspect = i - 1
        if nom_slop is not None and r.fields[i][0] == nom_slop:
            indice_record_slope = i - 1
        if r.fields[i][0] == id_station:
            indice_record_id_station = i - 1
        if r.fields[i][0] == nom_station:
            indice_record_nom_station = i - 1

    # Work with massif shapefiles (which are in snowtools)
    massif = shapefile.Reader(path_shapefile_massif)
    shape_massif = massif.shapes()
    geomet_massif = massif.shapeRecords()
    # !! If changes in snowtools massif, check that geomet_massif.record[0] is still the massif number.
    # If this is another number, change indice_record_massif value

    # Conversion from Lambert93 (EPSG 2154) to WGS84 (EPSG 4326)
    project_from_L93_to_WGS84 = partial(pyproj.transform, pyproj.Proj(init='epsg:2154'), pyproj.Proj(init='epsg:4326'))
    list_shape_massif_WGS84 = [transform(project_from_L93_to_WGS84, shape(shape_massif[i]))
                               for i in range(len(shape_massif))]

    # Function from Ambroise Guiot: read points in a geotif
    # L.Roussel 2024: bilinear interpolation does not work for angles (in real mean of 1° and 359° = 0°, not 180°)
    def raster_to_points(raster_src, shape, nodata=np.nan, interp_method="bilinear"):
        """
        For each point of GeometryCollection shape, give the bilinear interpolation of the values of
        the closest 4 pixels from raster_src.

        Beware : - geotif file and shape must have same projection
                   the projection should not use rotation (Lambert 93 OK, WSG84 ?)

        :param raster_src: geotif file path (file from which we extract values)
        :type raster_src: str
        :param shape: List of points list.  shapely.geometry.collection.GeometryCollection format
        (get with shape( shapefile.Reader('...').shapes() )
        :param nodata: Values for points without close pixel. Default is np.nan.
        :type nodata: float
        :param interp_method: Option chosen for interpolation, either usual 'bilinear' or for angles 'bilinear_angle'.
        :type interp_method: string

        :returns: List in same order as GeometryCollection given.
        For each point, it has the value coming from geotif file.
        """
        if interp_method not in ["bilinear", "bilinear_angle"]:
            raise ValueError("Parameter 'interp_method' has an impossible value.")
        print(interp_method)
        raster = gdal.Open(raster_src)  # open tif image
        gt = raster.GetGeoTransform()
        # wkt = raster.GetProjection()
        band = raster.GetRasterBand(1)
        nodata_raster = band.GetNoDataValue()
        points_values = []

        for s in shape.geoms:
            # Convert from map to pixel coordinates.
            # Only works for geotransforms with no rotation.
            current_x = (s.x - gt[0]) / gt[1]
            current_y = (s.y - gt[3]) / gt[5]
            px = int(current_x)  # x pixel
            py = int(current_y)  # y pixel

            val = band.ReadAsArray(px, py, 2, 2)
            test_values_not_nodata = (val[0][0] != nodata_raster and val[0][1] != nodata_raster and val[1][0] != nodata_raster
                       and val[1][1] != nodata_raster)

            if val is not None and test_values_not_nodata:
                value = None
                if interp_method == "bilinear":
                    value = ((1 - current_x % 1) * (1 - current_y % 1) * val[0][0]
                            + (current_x % 1) * (1 - current_y % 1) * val[0][1]
                            + (1 - current_x % 1) * (current_y % 1) * val[1][0]
                            + (current_x % 1) * (current_y % 1) * val[1][1])
                elif interp_method == "bilinear_angle":
                    # switch to vectors weighted by euclidian distances to grid point
                    # (no need to normalize as we get only the angle of the vector next)
                    weights = np.array([(1 - current_x % 1) * (1 - current_y % 1),
                                (current_x % 1) * (1 - current_y % 1),
                                (1 - current_x % 1) * (current_y % 1),
                                (current_x % 1) * (current_y % 1)])
                    angle_rad_values = np.array([val[0][0], val[0][1], val[1][0], val[1][1]]) * np.pi / 180 # degrees to radians
                    cos_values, sin_values = np.cos(angle_rad_values), np.sin(angle_rad_values)
                    cos_weighted, sin_weighted = cos_values @ weights, sin_values @ weights
                    # (np.arctan in [-pi/2, pi/2])
                    # np.arctan2 in the four quadrants
                    value = (np.arctan2(sin_weighted, cos_weighted) / np.pi * 180 + 360) % 360 # radian to degree + assert between 0, 360°
                points_values.append(value)
            else:
                points_values.append(nodata)
        return points_values

    ################################################################
    # Creation of lists coming from geotif
    liste_altitude_MNT = raster_to_points(path_MNT_alt, shape(shapes))
    liste_aspect_MNT = raster_to_points(path_MNT_asp, shape(shapes), interp_method="bilinear_angle")
    liste_slope_MNT = raster_to_points(path_MNT_slop, shape(shapes))

    liste_altitude_MNT_arrondie = [int(round(liste_altitude_MNT[i])) for i in range(len(liste_altitude_MNT))]
    liste_aspect_MNT_arrondie = [int(round(liste_aspect_MNT[i])) % 360 for i in range(len(liste_aspect_MNT))]
    liste_slope_MNT_arrondie = [int(min(60, round(liste_slope_MNT[i]))) for i in range(len(liste_slope_MNT))]

    ################################################################
    # Creation lof massif list
    liste_massif = [geomet_massif[j].record[indice_record_massif] for i in range(len(shapes))
                    for j in range(len(shape_massif)) if list_shape_massif_WGS84[j].contains(list_shape_WGS84[i])]
    ''' NON PYTHONIC MODE IN ORDER TO UNDERSTAND BETTER:
    liste_massif = []
    for i in range(len(shapes)):
        for j in range(len(shape_massif)):
            if list_shape_massif_WGS84[j].contains(list_shape_WGS84[i]):
                liste_massif.append(geomet_massif[j].record[0])
    END OF NON PYTHONIC MODE'''

    ################################################################
    # Creation longitude list:
    liste_longitude = [round(list_shape_WGS84[i].x, 6) for i in range(len(list_shape_WGS84))]
    # Creation latitude list:
    liste_latitude = [round(list_shape_WGS84[i].y, 6) for i in range(len(list_shape_WGS84))]
    # Creation "id station" list made with field number from shapefile
    # 8 figures because SURFEX is using OMM convention
    liste_id_station = ['%08d' % (int(geomet[i].record[indice_record_id_station]) + 10000000)
                        for i in range(len(shapes))]
    # Creation "name station" list made with field name from shapefile
    liste_nom_station = [geomet[i].record[indice_record_nom_station] for i in range(len(shapes))]

    ################################################################
    # Creation of lists via shapefile or MNT + Verification and eventually compare with MNT
    if nom_alt is not None:
        liste_altitude = [geomet[i].record[indice_record_altitude] for i in range(len(shapes))]
        print('########### Vérification MNT vs shapefile ##############')
        print('ALTITUDE:')
        print('écart max: ' + str(max([abs(liste_altitude_MNT[i] - liste_altitude[i]) for i in range(len(shapes))])))
        print('écart moyen: ' + str(np.mean([abs(liste_altitude_MNT[i] - liste_altitude[i])
                                             for i in range(len(shapes))])))
    else:
        liste_altitude = liste_altitude_MNT_arrondie
    if nom_asp is not None:
        liste_aspect = [geomet[i].record[indice_record_aspect] for i in range(len(shapes))]
        print('########### Vérification MNT vs shapefile ##############')
        print('ORIENTATION:')
        print('écart max: ' + str(max([abs(liste_aspect_MNT[i] - liste_aspect[i]) for i in range(len(shapes))])))
        print('écart moyen: ' + str(np.mean([abs(liste_aspect_MNT[i] - liste_aspect[i]) for i in range(len(shapes))])))
    else:
        liste_aspect = liste_aspect_MNT_arrondie
    if nom_slop is not None:
        liste_slope = [geomet[i].record[indice_record_slope] for i in range(len(shapes))]
        print('########### Vérification MNT vs shapefile ##############')
        print('PENTE:')
        print('écart max: ' + str(max([abs(liste_slope_MNT[i] - liste_slope[i]) for i in range(len(shapes))])))
        print('écart moyen: ' + str(np.mean([abs(liste_slope_MNT[i] - liste_slope[i]) for i in range(len(shapes))])))
    else:
        liste_slope = liste_slope_MNT_arrondie

    return {'lat': liste_latitude, 'lon': liste_longitude,
            'alt': liste_altitude, 'asp': liste_aspect,
            'm': liste_massif, 'slop': liste_slope,
            'id': liste_id_station, 'nom': liste_nom_station}


################################################################
# Creation of NetCDF
################################################################
def create_NetCDF(all_lists, output_name):
    """
    Creation of 1D NetCDF for simulation reanalysis-projection from dictionary of lists and output name.
    Dictionary of lists is coming from the shapefile and is built with make_dict_list.

    :param all_lists: Dictionary of lists in order to fill NetCDF variables
    :param output_name: output name for the NetCDF file
    :type output_name: str

    :returns: nothing (in a pythonic way). Write a netcdf file.
    """
    outputs = Dataset(output_name, 'w', format='NETCDF4')
    outputs.createDimension('Number_of_points', len(all_lists['alt']))
    A = outputs.createVariable('LAT', np.float64, ('Number_of_points',), fill_value=-9999999)
    B = outputs.createVariable('LON', np.float64, ('Number_of_points',), fill_value=-9999999)
    C = outputs.createVariable('ZS', np.float64, ('Number_of_points',), fill_value=-9999999)
    D = outputs.createVariable('aspect', np.float64, ('Number_of_points',), fill_value=-9999999)
    E = outputs.createVariable('massif_num', int, ('Number_of_points',), fill_value=-999)
    F = outputs.createVariable('slope', np.float64, ('Number_of_points',), fill_value=-9999999)
    G = outputs.createVariable('station', int, ('Number_of_points',), fill_value=-9999999)

    outputs['LAT'][:] = all_lists['lat']  # liste_latitude
    outputs['LON'][:] = all_lists['lon']  # liste_longitude
    outputs['ZS'][:] = all_lists['alt']  # liste_altitude
    outputs['aspect'][:] = all_lists['asp']  # liste_aspect_
    outputs['massif_num'][:] = all_lists['m']  # liste_massif
    outputs['slope'][:] = all_lists['slop']  # liste_slope_
    outputs['station'][:] = all_lists['id']  # liste_id_station

    A.setncatts({'long_name': u"latitude", 'units': u"degrees_north"})
    B.setncatts({'long_name': u"longitude", 'units': u"degrees_east"})
    C.setncatts({'long_name': u"altitude", 'units': u"m"})
    D.setncatts({'long_name': u"slope aspect", 'units': u"degrees from north"})
    E.setncatts({'long_name': u"Massif Number"})
    F.setncatts({'long_name': u"slope angle", 'units': u"degrees from horizontal"})
    G.setncatts({'long_name': u"Station OMM number"})

    outputs.close()


################################################################
# Creation of skylines
################################################################
def create_skyline(all_lists, path_MNT_alt, path_shapefile, list_skyline):
    """
    Add in METADATA.xml the points from shapefile with skylines masks
    (ie the angles of view for the azimuts).

    :param all_lists: Dictionary of lists
    :param path_MNT_alt: MNT path for altitude values
    :type path_MNT_alt: str
    :param path_shapefile: shapefile path
    :param path_shapefile: str
    :param list_skyline: List of identification points of shapefile from which we want the skyline.

    :returns: nothing (in a pythonic way). Plot the skyline and add lines in METADATA.xml
    """
    start_time = time.time()

    in_file = [[all_lists['id'][i], all_lists['alt'][i], all_lists['m'][i], all_lists['nom'][i], all_lists['lat'][i],
                all_lists['lon'][i]] for i in range(len(all_lists['alt'][:]))]
    # à in_file = np.loadtxt(file_in, dtype={'names': ('numposte', 'alt', 'massif', 'nom', 'lat', 'lon'),
    #                        'formats': (int, int, int, '|S24', float, float)})

    #######################################################
    #  ADD NEW SITES IN XML FILE, Part I
    #######################################################

    # ouverture  du fichier en mode "lecture"/"ecriture"
    metadata = open("SKYLINE.xml", 'w+')

    # faire la chaine de caractère des azimuts pour ecriture dans XML file
    azimut_str = ','.join(map(str, range(0, 360, 5)))

    metadata.write('\t<Sites>\n')

    # output folder for skyline graph (if asked via options)
    if not os.path.isdir("graph_skyline") and list_skyline is not None:
        os.mkdir("graph_skyline")

    viewmax = 20000  # 20 km

    r = shapefile.Reader(path_shapefile)
    shapes = r.shapes()
    shape_courant = shape(shapes)  # rajouter un .geoms pour le Deprecated Warning ?

    raster = gdal.Open(path_MNT_alt)  # ouverture de l'image tif
    gt = raster.GetGeoTransform()
    # wkt = raster.GetProjection()
    band = raster.GetRasterBand(1)
    nodata_raster = band.GetNoDataValue()
    step = int((gt[1] + (-gt[5])) / 2)  # for further use in line interpolation
    print("step: ", step)

    for k, s in enumerate(shape_courant.geoms):
        in_stat = in_file[k]
        print('hello', in_stat[0], in_stat[3])
        center_x = (s.x - gt[0]) / gt[1]
        center_y = (s.y - gt[3]) / gt[5]

        px_c = int(center_x)  # x pixel centre
        py_c = int(center_y)  # y pixel centre

        # Idee: faire une interpolation biliénaire: -> voir quel pixel (pour orientation) mais en gros
        # f(x,y) = f(0,0)(1-x)(1-y) + f(1,0)x(1-y) + f(0,1)(1-x)y + f(1,1)xy avec x et y entre 0 et 1.
        # Ici: x est la partie décimale de (shape_courant[k].x - gt[0]) / gt[1] et
        #      y est la partie décimale de (shape_courant[k].y - gt[3]) / gt[5]
        # La fonction f est jouée par le tableau 2x2 band.ReadAsArray(px_c,py_c,2,2)
        # ! band.ReadAsArray(px_c,py_c,2,2)[0][1] correspond à band.ReadAsArray(px_c+1,py_c,1,1)[0][0]
        value_bilin = band.ReadAsArray(px_c, py_c, 2, 2)
        value_c = ((1 - center_x % 1) * (1 - center_y % 1) * value_bilin[0][0]
                   + (center_x % 1) * (1 - center_y % 1) * value_bilin[0][1]
                   + (1 - center_x % 1) * (center_y % 1) * value_bilin[1][0]
                   + (center_x % 1) * (center_y % 1) * value_bilin[1][1])

        print("alt: ", round(value_c, 1))
        anglee = []
        for azimut in range(0, 360, 5):
            angle = []
            for index, dist in enumerate(range(step, viewmax, step)):
                current_x = (s.x + dist * math.sin(math.radians(azimut)) - gt[0]) / gt[1]
                current_y = (s.y + dist * math.cos(math.radians(azimut)) - gt[3]) / gt[5]

                px = int(current_x)  # x pixel le long de l'azimut
                py = int(current_y)  # y pixel le long de l'azimut
                val = band.ReadAsArray(px, py, 2, 2)

                test_ok = (val[0][0] != nodata_raster and val[0][1] != nodata_raster and val[1][0] != nodata_raster
                           and val[1][1] != nodata_raster)

                if val is not None and test_ok:
                    value = ((1 - current_x % 1) * (1 - current_y % 1) * val[0][0]
                             + (current_x % 1) * (1 - current_y % 1) * val[0][1]
                             + (1 - current_x % 1) * (current_y % 1) * val[1][0]
                             + (current_x % 1) * (current_y % 1) * val[1][1])

                    angle.append(math.ceil((math.degrees(math.atan((value - value_c) / dist))) * 100) / 100)
                else:
                    angle.append(0)
            anglee.append(max(angle))

        # print(anglee)
        az = [azimut for azimut in range(0, 360, 5)]
        angle_str = ','.join(map(str, anglee))

        # PLOT
        if list_skyline is not None and all_lists['id'][k] in list_skyline:
            az = np.array(az, 'float')
            anglee = np.array(anglee, 'float')
            fig = plt.figure()
            a = fig.add_subplot(111, polar=True)
            rmax = max(40., max(anglee))
            # print rmax-anglee
            a.fill(az * math.pi / 180., rmax - anglee, '-ob', alpha=0.5, edgecolor='b')
            a.set_rmax(rmax)
            a.set_rgrids([0.01, 10., 20., 30., float(int(rmax))], [str(int(rmax)), '30', '20', '10', '0'])
            a.set_thetagrids([0., 45., 90., 135., 180., 225., 270., 315.], ["N", "NE", "E", "SE", "S", "SW", "W", "NW"])
            a.set_title(str(in_stat[3]) + ' alt mnt:' + str(round(value_c, 1)) + ' m alt poste:' + str(in_stat[1]))
            a.set_theta_zero_location('N')
            a.set_theta_direction(-1)
            plt.savefig('graph_skyline/' + str(in_stat[0]) + '_skyline.png')
            # show()
            plt.close()

        ########################################################
        #  ADD NEW SITES IN XML FILE, Part II
        ########################################################
        print("ajout du site : ", all_lists['nom'][k])

        metadata.write('\t<Site>\n')
        metadata.write('\t\t<name> ' + str(all_lists['nom'][k]) + ' </name>\n')
        metadata.write('\t\t<nameRed> ' + str(all_lists['nom'][k]) + ' </nameRed>\n')
        metadata.write('\t\t<number> ' + str(all_lists['id'][k]) + ' </number>\n')
        metadata.write('\t\t<lat> ' + str(all_lists['lat'][k]) + ' </lat>\n')
        metadata.write('\t\t<lon> ' + str(all_lists['lon'][k]) + ' </lon>\n')
        metadata.write('\t\t<altitude> ' + str(all_lists['alt'][k]) + ' </altitude>\n')
        metadata.write('\t\t<aspect> ' + str(all_lists['asp'][k]) + ' </aspect>\n')
        metadata.write('\t\t<slope> ' + str(all_lists['slop'][k]) + ' </slope>\n')
        metadata.write('\t\t<massif> ' + str(all_lists['m'][k]) + ' </massif>\n')
        metadata.write('\t\t<zref> ' + "1.5" + ' </zref>\n')
        metadata.write('\t\t<uref> ' + "10.0" + ' </uref>\n')
        metadata.write('\t\t<azimut> ' + azimut_str.rstrip() + ' </azimut>\n')
        metadata.write('\t\t<mask> ' + angle_str.rstrip() + ' </mask>\n')
        metadata.write('\t\t<source_mask> ' + 'IGN' + ' </source_mask>\n')
        metadata.write('\t</Site>\n')

    metadata.write('\t</Sites>\n')
    metadata.close()
    print("done in", time.time() - start_time, "seconds")


def parseArguments(args):
    """
    Parsing the arguments when you call the main program.
    :param args: The list of arguments when you call the main program (typically sys.argv[1:] )
    """
    # Create argument parser
    parser = argparse.ArgumentParser()

    # Mandatory argument
    parser.add_argument("path_shape", help="Path to shapefile", type=str)
    parser.add_argument("name_station", help="Shapefile Field Name containing a unique reference for points", type=str)
    parser.add_argument("id_station", help="Shapefile Field Number containing a unique reference for points", type=str)

    # Optional argument
    parser.add_argument("--name_alt", help="Shapefile Field Name containing altitude, if it exists", type=str)
    parser.add_argument("--name_asp", help="Shapefile Field Name containing aspect, if it exists", type=str)
    parser.add_argument("--name_slop", help="Shapefile Field Name containing slope, if it exists", type=str)
    parser.add_argument("-o", "--output", help="Name for NetCDF file to save", type=str, default=NetCDF_out)
    parser.add_argument("--MNT_alt", help="Path for MNT altitude", type=str, default=path_MNT_alti_defaut)
    parser.add_argument("--MNT_asp", help="Path for MNT altitude", type=str, default=path_MNT_aspect_defaut)
    parser.add_argument("--MNT_slop", help="Path for MNT altitude", type=str, default=path_MNT_slope_defaut)
    parser.add_argument("--list_skyline", nargs='*', help="The skyline plot you want", default=None)

    args = parser.parse_args(args)
    return args


def main(args=None):
    """
    Main program: parse argument then launch the creation of NetCDF and skylines
    """
    args = args if args is not None else sys.argv[1:]
    if len(sys.argv) > 1: 
        args = parseArguments(args)

        # argument for command line call
        path_shapefile = args.path_shape
        nom_station = args.name_station
        id_station = args.id_station
        nom_alt = args.name_alt
        nom_asp = args.name_asp
        nom_slop = args.name_slop
        path_MNT_alt = args.MNT_alt
        path_MNT_asp = args.MNT_asp
        path_MNT_slop = args.MNT_slop
        output_name = args.output
        list_skyline = args.list_skyline

        # Check if mandatory path for shapefile is OK
        if not os.path.isfile(path_shapefile + '.shp'):
            logger.critical('Provided path for shapefile file does not exist ({})'.format(path_shapefile))
            sys.exit(1)

        # Check if mandatory field to play the role of station in shapefile is OK
        r = shapefile.Reader(path_shapefile)
        if nom_station not in [r.fields[i][0] for i in range(len(r.fields))]:
            logger.critical('Field {} does not exist in {}'.format(nom_station, path_shapefile))
            sys.exit(3)
        if id_station not in [r.fields[i][0] for i in range(len(r.fields))]:
            logger.critical('Field {} does not exist in {}'.format(id_station, path_shapefile))
            sys.exit(3)

        # Launch the app:
        all_lists = make_dict_list(path_shapefile, id_station, nom_station, nom_alt, nom_asp, nom_slop, path_MNT_alt,
                                   path_MNT_asp, path_MNT_slop)
        
        create_NetCDF(all_lists, output_name)
        if list_skyline == ['all'] or list_skyline == ['All'] or list_skyline == ['ALL']:
            list_skyline = [all_lists['id'][k] for k in range(len(all_lists['id']))]
        elif list_skyline is not None:
            list_skyline = ['%08d' % (10000000 + int(list_skyline[i]))
                            for i in range(len(list_skyline))]
        create_skyline(all_lists, path_MNT_alt, path_shapefile, list_skyline)


if __name__ == '__main__':
    main()