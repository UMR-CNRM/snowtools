#!/usr/bin/env python3
# -*- coding: utf-8 -*-

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
# On part d'un shapefile au format .kml ou bien .shp
# Ce shapefile peut être en coordonnées (lat, lon) ou bien en Lambert93
# De ce shapefile, le programme fournit un NetCDF2D, ie un NetCDF pour simulations 2D
##################################
# Exemple Lancement
##################################
# python3 shapefile2NetCDF_2D.py SP_Abries_Ristolas.kml -o NetCDF2D_Ristolas.nc
#
####################
# Utilisation du fichier NetCDF sur belenos: 
####################
# d'abord, on pushe sur Belenos avec scp:
# scp NetCDF2D_* fructusm@belenos:/home/fructusm/SIMU_TOP2D/.
#
# s2m  --walltime='2:00:00' -b 20190801 -e 20200801 -m s2m -f reanalysis2020.2@lafaysse 
# -r alp_flat:Aussois250:/home/cnrm_other/cen/mrns/fructusm/SIMU_TOP2D/NetCDF2D_Aussois.nc
# -n /home/cnrm_other/cen/mrns/fructusm/SIMU_TOP2D/Aussois.nam --grid --ntasks=52 -g -o Aussois_2D
#
# s2m  --walltime='6:00:00' -b 19580801 -e 20210801 -m s2m -f reanalysis2020.2@lafaysse
# -r alp_flat:Belledonne250:/home/cnrm_other/cen/mrns/fructusm/SIMU_TOP2D/NetCDF2D_Belledonne.nc
# -n /home/cnrm_other/cen/mrns/fructusm/SIMU_TOP2D/Belledonne.nam --grid --ntasks=44 -x 19680801 -o Belledonne_2D
#
# s2m  --walltime='6:00:00' -b 19580801 -e 20210801 -m s2m -f reanalysis2020.2@lafaysse
# -r alp_flat:Cayolle250:/home/cnrm_other/cen/mrns/fructusm/SIMU_TOP2D/NetCDF2D_Cayolle.nc
# -n /home/cnrm_other/cen/mrns/fructusm/SIMU_TOP2D/Cayolle.nam --grid --ntasks=38 -x 19680801 -o Cayolle_2D
#
# s2m  --walltime='6:00:00' -b 19580801 -e 20210801 -m s2m -f reanalysis2020.2@lafaysse
# -r alp_flat:Oisan250:/home/cnrm_other/cen/mrns/fructusm/SIMU_TOP2D/NetCDF2D_Oisan.nc
# -n /home/cnrm_other/cen/mrns/fructusm/SIMU_TOP2D/Oisan.nam --grid --ntasks=45 -x 19680801 -o Oisan_2D
#
# s2m  --walltime='6:00:00' -b 19580801 -e 20210801 -m s2m -f reanalysis2020.2@lafaysse 
# -r alp_flat:Ristolas250:/home/cnrm_other/cen/mrns/fructusm/SIMU_TOP2D/NetCDF2D_Ristolas.nc 
# -n /home/cnrm_other/cen/mrns/fructusm/SIMU_TOP2D/Ristolas.nam --grid --ntasks=65 -x 19680801 -o Ristolas_2D
#
#  
# !!!!
# PENSER AU SPINUP
# !!!!
#
#
# Options: 
# -m pour le modèle
# -f pour les fichiers de forcing -> on va les chercher chez Matthieu pour reanalyse. Bientôt -f reanalysis
# -r pour la région: penser à rajouter la géométrie dans vortex/conf/geometries.ini
# -g car on n'a pas de prep au début -> il faut faire un spinup
# -a 400 pour limiter le Snow Water Equivalent à 400kg/m3 au 1er août
# --ntasks 50 si on n'a que 50 points dans un fichier netcdf 2D (je ne sais plus si c'est le min des lignes ou le 
# min des colonnes qu'il faut prendre)
#
# Pour transférer les fichiers de Belenos à sxcen:
# utiliser get_reanalysis qui est dans snowtools_git/scripts/extract/vortex
# Sur SXCEN: python3 get_reanalysis.py --geometry=orchamp --snow --xpid=SPINUP@fructusm
# Sur SXCEN: python3 get_reanalysis.py --geometry=orchamp --snow --xpid=TEST_Raphaelle@fructusm
#                                      --byear=2010 --eyear=2099
################################################################


################################################################
# VALEURS PAR DEFAUT CHANGEABLE PAR OPTION:
# MNT (30m) et Nom NetCDF de sortie
################################################################
NetCDF_out = 'NetCDF2D_from_shapefile.nc'

# PATH_MNT
path_MNT_alti_30m = '/rd/cenfic3/sentinel/mnt_ange/ange-factory/prod1/france_30m/DEM_FRANCE_L93_30m_bilinear.tif'
path_MNT_alti_250m = '/rd/cenfic3/sentinel/mnt_ange/ange-factory/prod1/france_250m/DEM_FRANCE_L93_250m_bilinear.tif'

################################################################
# Infos shapefile massif, a priori pérenne 
################################################################
path_shapefile_massif = SNOWTOOLS_DIR + '/DATA/massifs_Lbrt93_2019'

################################################################
#            FIN DUR DANS LE CODE
################################################################


################################################################
# Mise en place des log
################################################################
logger = logging.getLogger()
logger.setLevel(logging.WARNING)
console_handler = logging.StreamHandler()
console_handler.setFormatter(logging.Formatter('%(levelname)s :: %(message)s'))
console_handler.setLevel(logging.DEBUG)
logger.addHandler(console_handler)


################################################################
#   ETAPE 1: convertir le kml en shapefile pour pouvoir le lire
################################################################
def convert_kml2shp(path_shape_kml):
    """
    Les fichiers en .kml (Keyhole Markup Language, google format) sont transformés en .shp (format GIS Geographic
    Information System de l'ESRI) pour éviter d'avoir à rajouter des imports de librairie "non standard" dans la
    configuration actuelle des PC du CEN. C'est un choix évidemment discutable.
    :param path_shape_kml: le chemin vers le fichier de shapefile au format kml
    :type path_shape_kml: str
    :return: a string: the path to the .shp file after conversion from the .kml file.
    """
    path_shape_shp = path_shape_kml[:-3] + 'shp'
    cmd = ['ogr2ogr', '-f', 'ESRI Shapefile', path_shape_shp, path_shape_kml]
    subprocess.call(cmd, stdout=sys.stdout, stderr=sys.stderr)
    return path_shape_shp


################################################################
#   ETAPE 2: récupérer bornes du shapefile
################################################################
def get_bounds(path_shape_shp):
    """
    sf.bbox retourne [6.61446151100006, 45.189366518, 6.8154706149867, 45.2979826160001]
    ie [lon_min, lat_min, lon_max, lat_max]
    :param path_shape_shp: le chemin vers le fichier de shapefile
    :type path_shape_shp: str
    :return: a list of 4 values [lon_min, lat_min, lon_max, lat_max]
    """
    sf = shapefile.Reader(path_shape_shp)
    return sf.bbox


################################################################
#   ETAPE 2 bis: si c'est du lat - lon, convertir en L93
################################################################
def conversion_to_L93_if_lat_lon(list_of_four):
    """
    Idée: on a une liste bbox [lon_min, lat_min, lon_max, lat_max] que l'on convertit éventuellement en Lambert93
    Par défaut, on agrandit un peu au cas où
    Critère: si dans la bbox, ce n'est pas inférieur à 200, alors on est déjà en L93
    L'idée est que les coordonnées sont soit en WGS84 (ie coordonnées lat-lon -> tout est inférieur à 200), soit en L93

    POUR RAPPEL:     L93 = EPSG 2154      WGS84 = EPSG 4326

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
#   ETAPE 3: nettoyer shapefile
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
#   ETAPE 4: faire la grille en (250m * 250m)
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
#   ETAPE 4 bis: créer le .tif de la grille
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
#   ETAPE 5: repérer le massif prédominant
################################################################
def find_most_common_massif(dict_info):
    """
    Use the grid info coming from the dictionary and the massif info coming from snowtools to detect what is the most
    common massif on the grid. If there are more than 50% of points in the most common massif, all the points are
    artificially associated with the most common massif. Otherwise, all the points in the grid keep the massif they
    belong.
    :param dict_info: dictionary with all the information for the grid
    :type dict_info: dict
    :return: an integer (the massif number of the most common massif) if there is more than 50% point in the most common
     massif, a numpy array with the size of the grid otherwise.
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
#   ETAPE 6: confiner les altitudes entre le min et le max du massif
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
#   ETAPE 7: sortie écran pour les infos à mettre dans la namelist
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
    print('pour le fichier: ', output_name)
    print('dans la namelist &NAM_IGN, on prend:')
    print('XCELLSIZE=', dict_info['resol_x'])
    print('XX_LLCORNER=', dict_info['XLL'])
    print('XY_LLCORNER=', dict_info['YLL'])
    print('A vérifier avec NETCDF dimension (décalage de 1 possible):')
    print('NCOLS=', dict_info['Nb_pixel_x'])
    print('NROWS=', dict_info['Nb_pixel_y'])
    print('pour lancement BELENOS, prendre ntasks < min(NCOLS, NROWS)')


################################################################
#   Récupération des arguments 
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
