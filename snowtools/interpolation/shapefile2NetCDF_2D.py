#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
This script needs a shapefile (Lambert 93 or (lat-lon) coordinates).
This shapefile can be in .shp or .kml format.
This shapefile is delimitating a 2D domain on which you want to make a SURFEX-Crocus simulation.

If you want to do a simulation for several distincts differents points and not a domain:
see shapefile2NetCDF.py instead.

General documentation of shapefile2NetCDF_2D script
---------------------------------------------------
This script creates a NetCDF file using the shapefile.
The NetCDF file is necessary to launch Surfex-Crocus 2D simulations.
The NetCDF will define a rectangular 2D domain which contains your shapefile.
If your 2D domain is belonging to several "SAFRAN massif", by default:
If more than 50% of points are in one massif, all the points are artificially associated with this massif
After the execution you have some infos for the NAM_IGN part of the namelist.

EXAMPLES OF USE (script launch)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: bash

   python3 shapefile2NetCDF_2D.py SP_Abries_Ristolas.kml -o NetCDF2D_Ristolas.nc


EXAMPLES OF USE OF NETCDF FILE IN HPC SIMULATION
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

First, push on Belenos with scp:

.. code-block:: bash

   scp NetCDF2D_* fructusm@belenos:/home/fructusm/SIMU_TOP2D/.

Then s2m command:

.. code-block:: bash

   s2m research --walltime='2:00:00' -b 20190801 -e 20200801 -m s2m -f reanalysis2020.2@lafaysse 
                -r alp_flat:Aussois250:/home/cnrm_other/cen/mrns/fructusm/SIMU_TOP2D/NetCDF2D_Aussois.nc
                -n /home/cnrm_other/cen/mrns/fructusm/SIMU_TOP2D/Aussois.nam --grid --ntasks=52
                -g -o Aussois_2D

!! DO NOT FORGET THE SPINUP !!

Options:

* -m model
* -f forcing files -> -f reanalysis in order to get the forcing from reanalysis
* -r région: add geometry in vortex/conf/geometries.ini
* -n namelist (get the same options than reanalysis)
* -g if you don't have a prep -> a spinup has to be made
* -a 400 In order to limit Snow Water Equivalent to 400kg/m3 at the end of the year (1rst of august)
* --ntasks 50 if 50 < min(column, lines) of netcdf 2D domain

File transfert from Belenos to sxcen:
use get_reanalysis which is in snowtools/scripts/extract/vortex

On SXCEN:

.. code-block:: bash

   python3 get_reanalysis.py --geometry=orchamp --snow --xpid=TEST_Raphaelle@fructusm
                              -byear=2010 --eyear=2099

"""

import os
import argparse
import subprocess
import sys
import logging  # Import pour les log

from netCDF4 import Dataset
import numpy as np
import shapefile
from shapely.geometry import shape
from shapely.geometry import Point
from shapely.ops import transform
from functools import partial
import pyproj

from snowtools.utils.infomassifs import infomassifs
from snowtools.DATA import SNOWTOOLS_DIR

################################################################
# DEFAULT VALUES (but can change with options):
# MNT (30m) and name of the output NetCDF
################################################################
NetCDF_out = 'NetCDF2D_from_shapefile.nc'

# PATH_MNT
path_MNT_alti_30m = '/rd/cenfic3/cenmod/home/haddjeria/mnt_ange/ange-factory/prod1/france_30m/DEM_FRANCE_L93_30m_bilinear.tif'
path_MNT_alti_250m = '/rd/cenfic3/cenmod/home/haddjeria/mnt_ange/ange-factory/prod1/france_250m/DEM_FRANCE_L93_250m_bilinear.tif'

################################################################
# Infos shapefile massif, normally stable and durable
################################################################
path_shapefile_massif = SNOWTOOLS_DIR + '/DATA/massifs'

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
#   STEP 1: convert kml in shapefile in order to read it
################################################################
def convert_kml2shp(path_shape_kml):
    """
    kml files (Keyhole Markup Language, google format) are transform in .shp (format GIS Geographic
    Information System from ESRI) in order to avoid imports from "non standard" librairies in CEN
    actual configuration of PC.

    :param path_shape_kml: path to kml shapefile
    :type path_shape_kml: str
    :return: a string: the path to the .shp file after conversion from the .kml file.
    """
    path_shape_shp = path_shape_kml[:-3] + 'shp'
    cmd = ['ogr2ogr', '-f', 'ESRI Shapefile', path_shape_shp, path_shape_kml]
    subprocess.call(cmd, stdout=sys.stdout, stderr=sys.stderr)
    return path_shape_shp


################################################################
#   STEP 2: get boundaries of shapefile
################################################################
def get_bounds(path_shape_shp):
    """
    sf.bbox return [6.61446151100006, 45.189366518, 6.8154706149867, 45.2979826160001]
    ie [lon_min, lat_min, lon_max, lat_max]

    :param path_shape_shp: path to shapefile
    :type path_shape_shp: str
    :return: a list of 4 values [lon_min, lat_min, lon_max, lat_max]
    """
    sf = shapefile.Reader(path_shape_shp)
    return sf.bbox


################################################################
#   STEP 2 bis: if (lat - lon), convert to L93
################################################################
def conversion_to_L93_if_lat_lon(list_of_four):
    """
    Idea: we have a list bbox [lon_min, lat_min, lon_max, lat_max]. We convert it in Lambert93
    By default, we raise a little in case of
    Criteria: if in bbox, this is not lower than 200, we are already in L93
    Because coordinates are in WGS84 (ie lat-lon -> everything below 200), or in L93

    REMINDER:     L93 = EPSG 2154      WGS84 = EPSG 4326

    :param list_of_four: list of 4 floats ([lon_min, lat_min, lon_max, lat_max])
    :type list_of_four: list
    :return: a list of 4 values: the same in L93 coordinates
    """
    if max(list_of_four) < 200:
        Lower = Point(list_of_four[0], list_of_four[1])
        Upper = Point(list_of_four[2], list_of_four[3])
        project_from_WGS84_to_L93 = partial(pyproj.transform, pyproj.Proj(init='epsg:4326'),
                                            pyproj.Proj(init='epsg:2154'))
        Lower_L93 = transform(project_from_WGS84_to_L93, Lower)
        Upper_L93 = transform(project_from_WGS84_to_L93, Upper)
        return [int(Lower_L93.x), int(Lower_L93.y), int(Upper_L93.x) + 1, int(Upper_L93.y) + 1]
    else:
        return list_of_four


################################################################
#   STEP 3: clean shapefile
################################################################
def clean_the_mess(path_shape_shp, clean_all):
    """
    Remove all the temporary files created with the script

    :param path_shape_shp: le chemin vers le fichier de shapefile
    :type path_shape_shp: str
    :return: In a pythonic point of view, nothing. Some files are deleted.
    """
    if clean_all:
        for suffixe in ['shp', 'prj', 'shx', 'dbf']:
            path_shape_rm = path_shape_shp[:-3] + suffixe
            cmd = ['rm', '-f', path_shape_rm]
            subprocess.call(cmd, stdout=sys.stdout, stderr=sys.stderr)
    cmd = 'rm -f step1.tif'
    subprocess.call(cmd.split(), stdout=sys.stdout, stderr=sys.stderr)


################################################################
#   STEP 4: create grid (250m * 250m)
################################################################
def create_dict_all_infos(list_of_four, resol_x=250, resol_y=250):
    """
    Create a dictionnary with all the important information (coordinates of the lower-left and upper-right points,
    number of pixel in x and y direction, resolution in x and y direction).

    :param list_of_four: list of four coordinates in L93 format
    :type list_of_four: list
    :param resol_x: resolution in x direction
    :type resol_x: int
    :param resol_y: resolution in y direction
    :type resol_y: int
    :return: a dictionary with all the information
    """
    XLL = list_of_four[0]
    YLL = list_of_four[1]
    XUR = list_of_four[2]
    YUR = list_of_four[3]
    Nb_pixel_x = round((XUR - XLL)/resol_x) + 1
    Nb_pixel_y = round((YUR - YLL)/resol_y) + 1
    new_XUR = XLL + resol_x * (Nb_pixel_x - 1)
    new_YUR = YLL + resol_y * (Nb_pixel_y - 1)
    return {'XLL': int(XLL), 'YLL': int(YLL), 'XUR': int(new_XUR), 'YUR': int(new_YUR),
            'Nb_pixel_x': int(Nb_pixel_x), 'Nb_pixel_y': int(Nb_pixel_y),
            'resol_x': int(resol_x), 'resol_y': int(resol_y)}


################################################################
#   STEP 4 bis: create .tif from grid
################################################################
def create_MNT_tif(dict_info, path_MNT_given=None):
    """
    Use the dictionary with all the information (coordinates of the lower-left and upper-right points, number of pixel
    in x and y direction, resolution in x and y direction) and and a MNT (with the good resolution) to create a .tif
    file with altitude values on the grid define by the information.

    :param dict_info: dictionary with all the information for the grid
    :type dict_info: dict
    :param path_MNT_given: optional, a path for the MNT (without a path, the standard MNT in cenfic3 is used)
    :type path_MNT_given: str
    :return: In a pythonic point of view, nothing. A .tif file is created.
    """
    if abs(dict_info['resol_x'] - 250) < 10 and abs(dict_info['resol_y'] - 250) < 10:
        path_MNT = path_MNT_alti_250m
    elif abs(dict_info['resol_x'] - 30) < 10 and abs(dict_info['resol_y'] - 30) < 10:
        path_MNT = path_MNT_alti_30m
    elif path_MNT_given is not None:
        path_MNT = path_MNT_given
    else:
        print('PB: no MNT provided with this resolution, please have a look to the code')
        pass
    cmd = ['gdalwarp', '-te', str(dict_info['XLL']), str(dict_info['YLL']), str(dict_info['XUR']),
           str(dict_info['YUR']), '-tap', '-tr', str(dict_info['resol_x']), str(dict_info['resol_y']), '-te_srs',
           'EPSG:2154', '-s_srs', 'EPSG:2154', path_MNT, 'step1.tif']
    subprocess.call(cmd, stdout=sys.stdout, stderr=sys.stderr)


################################################################
#   STEP 5: most common massif
################################################################
def find_most_common_massif(dict_info):
    """
    Use the grid info coming from the dictionary and the massif info coming from snowtools to detect what is the most
    common massif on the grid. If there are more than 50% of points in the most common massif, all the points are
    artificially associated with the most common massif. Otherwise, all the points in the grid keep the massif they
    belong.

    :param dict_info: dictionary with all the information for the grid
    :type dict_info: dict
    :return: an integer (the massif number of the most common massif) or a numpy array (if there is not common massif).
    """
    massif_num = np.zeros((dict_info['Nb_pixel_y'], dict_info['Nb_pixel_x']))
    massif_num = massif_num.astype(int)
    r = shapefile.Reader(path_shapefile_massif)
    shapes = r.shapes()
    infos = r.shapeRecords()
    # Enlève les massifs qui sont "trop loin" du shapefile d'intérêt
    massif_potentiel = [True]*len(shapes)
    for i in range(len(shapes)):
        if shape(shapes[i]).bounds[0] > dict_info['XUR']:
            massif_potentiel[i] = False
        elif shape(shapes[i]).bounds[1] > dict_info['YUR']:
            massif_potentiel[i] = False
        elif shape(shapes[i]).bounds[2] < dict_info['XLL']:
            massif_potentiel[i] = False
        elif shape(shapes[i]).bounds[3] < dict_info['YLL']:
            massif_potentiel[i] = False

    # On remplit le tableau de numéro des massifs
    for xi in range(dict_info['Nb_pixel_x']):
        for yj in range(dict_info['Nb_pixel_y']):
            abscisse = dict_info['XLL'] + xi * dict_info['resol_x']
            ordonnee = dict_info['YLL'] + yj * dict_info['resol_y']
            point = Point(abscisse, ordonnee)
            for i in range(len(shapes)):
                if massif_potentiel[i]:
                    polygon = shape(shapes[i])
                    massif_number = infos[i].record[0]
                    if polygon.contains(point):
                        massif_num[yj, xi] = massif_number

    # Les stats sur le tableau massif_num
    stat = np.bincount(massif_num.flatten())
    print('le massif majoritaire est: ', stat.argmax())
    if 100. * max(stat)/sum(stat) > 50.:
        print('sa fréquence est: ', 100. * max(stat)/sum(stat))
        print('tous les points sont mis dans le même massif')
        return stat.argmax()
    else:
        print('sa fréquence est: ', 100. * max(stat)/sum(stat))
        print('cette valeur est trop faible')
        print('on ne regroupe pas tous les points dans le même massif')
        return massif_num
        

################################################################
#   STEP 6: constraint altitudes between min and max of massif
################################################################
def create_netcdf(massif_number, file_out=NetCDF_out):
    """
    Creation of the 2D_NetCDF with 2 values: 'ZS' for the altitude and 'Massif number' for the massif number.

    :param massif_number: an integer or a numpy array depending of the percentage of points in the most common massif
    :param file_out: the name of the output for the netcdf file
    :type file_out: str
    :return: In a pythonic point of view, nothing. A .nc file is created.
    """
    cmd = ['gdal_translate', '-of', 'netCDF', '-co', 'FORMAT=NC4', 'step1.tif', 'step1.nc']
    subprocess.call(cmd, stdout=sys.stdout, stderr=sys.stderr)
    if type(massif_number) == int:
        alt_min_massif = infomassifs().getAltMinMax(massif_number)[0]
        alt_max_massif = infomassifs().getAltMinMax(massif_number)[1]
    """else:
        size_x = massif_number.shape[0]
        size_y = massif_number.shape[1]
        print(size_x)
        print(size_y)
        alt_min_massif = np.zeros(( size_x, size_y))
        alt_max_massif = np.zeros(( size_x, size_y))
        for i in range(size_x):
            print(i)
            for j in range(size_y):
                alt_min_massif[i,j] = infomassifs().getAltMinMax(massif_number[i,j])[0]
                alt_max_massif[i,j] = infomassifs().getAltMinMax(massif_number[i,j])[1]"""
    NetCDF_file = Dataset('step1.nc', 'r+')
    NetCDF_file.renameVariable('Band1', 'ZS')
    ZS = NetCDF_file.variables['ZS'][:]
    ZS = np.ma.filled(ZS, np.nan)
    if type(massif_number) == int:
        ZS = np.where(ZS > alt_min_massif, ZS, alt_min_massif)
        ZS = np.where(ZS < alt_max_massif, ZS, alt_max_massif)
    NetCDF_file.variables['ZS'][:] = ZS
    massif_num_nc = NetCDF_file.createVariable('massif_num', 'int', NetCDF_file.variables['ZS'].dimensions,
                                               fill_value=-9999999)
    massif_num_nc.long_name = 'Massif number'
    if type(massif_number) == int:
        massif_num_nc[:, :] = massif_number
    else:
        massif_num_nc[:, :] = massif_number


    print('NETCDF dimension:')
    print(NetCDF_file.variables['ZS'].shape)

    NetCDF_file.close()

    cmd = 'mv step1.nc ' + file_out
    subprocess.call(cmd.split(), stdout=sys.stdout, stderr=sys.stderr)


################################################################
#   STEP 7: infos for NAM_IGN part of namelist on screen
################################################################
def print_for_namelist(dict_info, output_name):
    """
    Just to have a print on the screen in order to have all the relevant information for the namelist used later in the
    Surfex simulations.

    :param dict_info: dictionary with all the information for the grid
    :type dict_info: dict
    :param output_name: the name of the output for the netcdf file
    :type output_name: str
    :return: In a pythonic point of view, nothing. Some information are printed on the screen.
    """
    print('File: ', output_name)
    print('In namelist &NAM_IGN:')
    print('XCELLSIZE=', dict_info['resol_x'])
    print('XX_LLCORNER=', dict_info['XLL'])
    print('XY_LLCORNER=', dict_info['YLL'])
    print('!! Check with NETCDF dimensions (gap of 1 possible):')
    print('NCOLS=', dict_info['Nb_pixel_x'])
    print('NROWS=', dict_info['Nb_pixel_y'])
    print('!! For BELENOS, take ntasks < min(NCOLS, NROWS)')


################################################################
#   Get arguments 
################################################################
def parseArguments(args):
    """
    Parsing the arguments when you call the main program.
    :param args: The list of arguments when you call the main program (typically sys.argv[1:] )
    """
    # Create argument parser
    parser = argparse.ArgumentParser()

    # Mandatory argument
    parser.add_argument("path_shape", help="Path to shapefile (with extension .kml or .shp)", type=str)

    # Optional argument
    parser.add_argument("-o", "--output", help="Name for NetCDF file to save", type=str, default=NetCDF_out)
    parser.add_argument("-rlon", "--resol_lon", help="Longitude Resolution for 2D grid (250 or 30)", type=int,
                        default=250)
    parser.add_argument("-rlat", "--resol_lat", help="Latitude Resolution for 2D grid (250 or 30)", type=int,
                        default=250)
    parser.add_argument("--MNT_alt", help="Path for MNT altitude", type=str, default=None)
    parser.add_argument("-m", "--massif_number", help="massif number if you want to choose the massif that will be applied to your zone", type=int, default=None)

    args = parser.parse_args(args)
    return args


def main(args=None):
    """
    Main program: parse argument then launch the creation of NetCDF
    """
    args = args if args is not None else sys.argv[1:]
    if len(sys.argv) > 1: 
        args = parseArguments(args)

        # argument for command line call
        path_shapefile = args.path_shape
        output_name = args.output
        resol_x = args.resol_lon
        resol_y = args.resol_lat
        path_MNT_alti = args.MNT_alt
        massif = args.massif_number
        # Check if mandatory path for shapefile is OK
        if not os.path.isfile(path_shapefile):
            logger.critical('Provided path for shapefile file does not exist ({})'.format(path_shapefile))
            sys.exit(1)

        clean_all = False
        if path_shapefile[-3:] == 'kml':
            path_shapefile = convert_kml2shp(path_shapefile)
            clean_all = True
        bounds = get_bounds(path_shapefile)
        bounds = conversion_to_L93_if_lat_lon(bounds)
        dict_info = create_dict_all_infos(bounds, resol_x, resol_y)
        create_MNT_tif(dict_info, path_MNT_alti)
        if massif is not None:
            massif_num = massif
        else:
            massif_num = find_most_common_massif(dict_info)

        massif_num = find_most_common_massif(dict_info)
        create_netcdf(massif_num, output_name)
        clean_the_mess(path_shapefile, clean_all)
        print_for_namelist(dict_info, output_name)


if __name__ == '__main__':
    main()
