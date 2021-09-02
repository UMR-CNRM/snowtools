#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import argparse
import sys
import re
from netCDF4 import Dataset
import numpy as np
import gdal
from shapely.geometry import shape, Polygon
from shapely.ops import transform
from functools import partial
import pyproj

from utils.infomassifs import infomassifs
from utils.FileException import FileNameException, FileOpenException

# Import pour les log
import logging

# Imports pour "Skyline"
import csv
import time
import math
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from osgeo import ogr, osr

# Bibliothèque ad hoc de Matthieu L pour ouvrir les shapefiles
import shapefile

################################################################
# On part d'un shapefile en Lambert 93
# De ce shapefile, le programme fournit un NetCDF pour les besoins du lancement de Surfex (ou de réanalyse) dessus
# Il fournit aussi les skyline qui sont ajoutées dans MEDATADA.xml
# Il est possible de garder certain tracés des skylines.

# NB: PROJECT NUMBER, C'EST QUOI ?
# Comme nous allons ajouter des points dans le fichier METADATA.xml, il s'agit de savoir si ces points existent déjà.
# A priori à la conception de l'algo, on se dit que:
# - il y aura moins de 10 000 points par projet
# - il y aura moins d'une centaine de projets
# (et sinon, on réfléchira à quelque chose de mieux)
# Le but est d'éviter d'avoir en double un code station (sur 8 chiffres comme les codes OMM)
# Bref:
# - les nombres entre 10000001 et 10009999 sont pour le projet 0
# - les nombres entre 10010001 et 10019999 sont pour le projet 1
# - etc
#############
# TRACABILITE DES NUMEROS DE PROJET
#############
#
# Au 27 août 2021:
# ANR TOP = projet 0 = geometrie orchamp dans vortex/conf/geometries.ini
#
#############
# METTRE A JOUR CI DESSUS A CHAQUE NOUVEAU PROJET
#############
#
#############
# Exemple Lancement:
#############
# python3 shapefile2NetCDF.py path_shapefile station_name_in_shapefile station_id_in_shapefile project_number [--name_alt alti_name_in_shapefile] [-o path_name_of_NetCDF_output] [--MNT_alt path_of_MNT_altitude] [--confirm_overwrite] [--list_skyline all or 1 5 6 if you want skyline for your id_station number 1, 5 and 6]
#
# python3 shapefile2NetCDF.py /home/fructusm/Téléchargements/Plots2020/plots codeplot idplot 0 --name_alt alti
# python3 shapefile2NetCDF.py /home/fructusm/Téléchargements/Plots2020/plots codeplot idplot 0 --name_alt alti --confirm_overwrite si on a déjà travaillé sur ce projet
# python3 shapefile2NetCDF.py /home/fructusm/Téléchargements/Plots2020/plots codeplot idplot 0 --name_alt alti --list_skyline 1 34 47 (pour avoir dans le dossier output les tour d'horizon des stations numéros 1, 34 et 47
#
#
#
##############
# Utilisation du fichier NetCDF pour reanalyse ou simulation en local
##############
# s2m -f path_FORCING -b begin_date -e end_date -r path_netcdf.nc -o output_name -g --extractforcing
# give interpol_FORCING
# s2m -f path_interpol_FORCING -b begin_date -e end_date -o output_name_pour_PRO -g --addmask
# give the PRO file for the shapefile points. 
#
# Exemple:
# s2m -f /rd/cenfic2/era40/vortex/s2m/alp_flat/reanalysis/meteo/FORCING_2017080106_2018080106.nc -b 20170801 -e 20180801 -r /home/fructusm/git/snowtools_git/fortran/NetCDF_from_shapefile.nc -o output_test_s2m -g --extractforcing
# s2m -f /home/fructusm/OUTPUT_et_PRO/output_test_s2m/meteo/FORCING_2017080106_2018080106.nc -b 20170801 -e 20180801 -o output_test_s2m_Safran -g --addmask
# (NB: export NINTERPOL=1 if MPI problem for the extractforcing)
#
####################
# Utilisation du fichier NetCDF sur belenos:
####################
#
# s2m -b 19580801 -e 20200801 -m s2m -f reanalysis2020.2@lafaysse -r alp_flat:orchamp:/home/cnrm_other/cen/mrns/fructusm/NetCDF_from_shapefile.nc -o TEST1 -n OPTIONS_V8.1_NEW_OUTPUTS_NC_reanalysis.nam -g --addmask -a 400
#
# PENSER AU SPINUP
#
# Options: 
# -m pour le modèle
# -f pour les fichiers de forcing -> on va les chercher chez Matthieu pour reanalyse. On devrait bientôt pouvoir faire -f reanalysis
# -r pour la région: penser à rajouter la géométrie dans vortex/conf/geometries.ini
# -n prendre les mêmes options que pour la réanalyse
# -g car on n'a pas de prep au début -> il faut faire un spinup
# --addmask pour tenir compte des masques calculés lors de la génération du fichier NetCDF
# -a 400 pour limiter le Snow Water Equivalent à 400kg/m3 au 1er août
#
# question pour simulation: où trouver les forcing de SMHI_RCA4_MOHC_HadGEM2_ES_RCP85 (par exemple) ?
# reponse: chez Raphaelle Samacoits d'où une commande avec quelque chose comme: 
# -m adamont -f RCM_GCM_EXP@samacoitsr -r alp_flat:geometryout:fichier.nc
################################################################




################################################################
# VALEURS PAR DEFAUT CHANGEABLE PAR OPTION:
# MNT (30m) et Nom NetCDF de sortie
################################################################
NetCDF_out = 'NetCDF_from_shapefile.nc'

# A CHANGER PLUS TARD
path_MNT_alti_defaut = '/home/fructusm/MNT_FRANCEandBORDER_30m_fusion:IGN5m+COPERNICUS30m_EPSG:2154_INT:AVERAGE_2021-03.tif'
path_MNT_slope_defaut = '/home/fructusm/MNT_slope.tif'
path_MNT_aspect_defaut = '/home/fructusm/MNT_aspect.tif'
#path_MNT_defaut = '/rd/cenfic2/manto/haddjeria/MNT/finalized/MNT_FRANCEandBORDER_30m_fusion:IGN5m+COPERNICUS30m_EPSG:2154_INT:AVERAGE_2021-03.tif'


################################################################
# Infos shapefile massif, a priori pérenne 
################################################################
path_shapefile_massif = '/home/fructusm/git/snowtools_git/DATA/massifs_Lbrt93_2019'
Indice_record_massif = 0

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
# Creation des listes de données
################################################################
def make_dict_list(path_shapefile, Id_station, Nom_station, Nom_alt, Nom_asp, Nom_slop, path_MNT_alt, path_MNT_asp, path_MNT_slop, add_for_METADATA):
    """
    Crée un dictionnaire de listes à partir des données du shapefile et des données des MNT

    :param path_shapefile: Chemin du shapefile dont on souhaite extraire les points.
    :type path_shapefile: str
    :param Id_station: Numéro de l'attribut du shapefile correspondant à un identifiant unique pour chaque point
    :type Id_station: str
    :param Nom_alt: si présent dans le shapefile, nom de l'attribut d'altitude. Si absent, ce champ est à None
    :type Nom_alt: str
    :param Nom_asp: si présent dans le shapefile, nom de l'attribut d'orientation. Si absent, ce champ est à None
    :type Nom_asp: str
    :param Nom_slop: si présent dans le shapefile, nom de l'attribut de pente. Si absent, ce champ est à None
    :type Nom_slop: str
    :param path_MNT_alt: Chemin du MNT contenant les valeurs d'altitude
    :type path_MNT_alt: str
    :param path_MNT_asp: Chemin du MNT contenant les valeurs d'aspect
    :type path_MNT_asp: str
    :param path_MNT_slop: Chemin du MNT contenant les valeurs de pente
    :type path_MNT_slop: str

    :returns: dictionnaire de listes:{ 'lat': liste_latitude, 'lon':liste_longitude, 
                                       'alt': liste_altitude, 'asp': liste_aspect, 
                                       'm': liste_massif, 'slop': liste_slope,
                                       'id': liste_id_station, 'nom': liste_nom_station } 
    """
    # Ouverture du shapefile d'intérêt. Conversion éventuelle
    r = shapefile.Reader(path_shapefile)
    shapes = r.shapes()
    geomet = r.shapeRecords()

    # Permet de convertir du Lambert93 (EPSG 2154) en WGS84 (EPSG 4326)
    # Lambert93: coordonnées en mètre sur la France métropolitaine élargie (avec Corse)
    # WSG84: coordonnées en (lon, lat) type GPS pour le monde entier
    project_from_L93_to_WGS84 = partial(pyproj.transform, pyproj.Proj(init='epsg:2154'),pyproj.Proj(init='epsg:4326'))
    list_shape_WGS84 = [transform(project_from_L93_to_WGS84,shape(shapes[i])) for i in range(len(shapes))]

    # Un petit print pour voir les attributs du shapefile: noms, coordonnées, altitude, ... 
    # Just_to_see = geomet[0].record
    # print(Just_to_see)

    # Recherche des indices de champs pour le shapefile
    for i in range(len(r.fields)):
        if Nom_alt is not None and r.fields[i][0]==Nom_alt:
            Indice_record_altitude = i - 1
        if Nom_asp is not None and r.fields[i][0]==Nom_asp:
            Indice_record_aspect = i - 1
        if Nom_slop is not None and r.fields[i][0]==Nom_slop:
            Indice_record_slope = i - 1
        if r.fields[i][0]==Id_station:
            Indice_record_id_station = i - 1
        if r.fields[i][0]==Nom_station:
            Indice_record_nom_station = i - 1

    ################################################################
    # Ouverture du shapefile massif. Conversion éventuelle
    '''
    # Version snwotools_git/DATA actuelle
    massif = shapefile.Reader(path_shapefile_massif)
    shape_massif = massif.shapes()
    geomet_massif = massif.shapeRecords()

    # Permet de convertir du Lambert_II (EPSG 27572) en WGS84 (EPSG 4326)
    # Lambert_II: coordonnées en mètre sur la France métropolitaine (sans la Corse ? voir https://epsg.io/27572 ) 
    # WSG84: coordonnées en (lon, lat) type GPS pour le monde entier
    project_from_LII_to_WGS84 = partial(pyproj.transform, pyproj.Proj(init='epsg:27572'),pyproj.Proj(init='epsg:4326'))
    list_shape_massif_WGS84 = [transform(project_from_LII_to_WGS84,shape(shape_massif[i])) for i in range(len(shape_massif))]'''

    # Version snwotools_git/DATA future
    # NB: Bien vérifier sur le shapefile massif que le numéro de massif est record[0].
    # Par exemple, pour les shapefile massif en LambertII, c'est record[1] qu'il faut prendre -> à changer dans DUR DANS LE CODE
    massif = shapefile.Reader(path_shapefile_massif)
    shape_massif = massif.shapes()
    geomet_massif = massif.shapeRecords()

    # Permet de convertir du Lambert93 (EPSG 2154) en WGS84 (EPSG 4326)
    # Lambert93: coordonnées en mètre sur la France métropolitaine élargie (avec Corse)
    # WSG84: coordonnées en (lon, lat) type GPS pour le monde entier
    project_from_L93_to_WGS84 = partial(pyproj.transform, pyproj.Proj(init='epsg:2154'),pyproj.Proj(init='epsg:4326'))
    list_shape_massif_WGS84 = [transform(project_from_L93_to_WGS84,shape(shape_massif[i])) for i in range(len(shape_massif))]


    # Fonction d'Ambroise Guiot pour lire les valeurs d'un geotif en des points.
    def raster_to_points(raster_src, shape, nodata=np.nan):
        """
        Associe pour chaque point de la GeometryCollection shape en entrée l'interpolation bilinéaire des valeurs
        des 4 pixels les plus proches issus du raster_src.
    
        Attention : - le fichier géotif et le shape doivent être dans la même projection
                    - la projection ne doit pas utiliser de rotation (Lambert 93 OK, WSG84 pas clair du tout)

        :param raster_src: Chemin du fichier géotif dont on souhaite extraire les valeurs.
        :type raster_src: str
        :param shape: Liste contenant la liste des points. Format en shapely.geometry.collection.GeometryCollection (obtenu avec shape( shapefile.Reader('...').shapes() ) 
        :param nodata: Valeurs attribuée aux points n'ayant pas de pixel à proximité. The default is np.nan.
        :type nodata: float

        :returns: Liste dans le même ordre que la GeometryCollection fournie en entrée et contenant pour chaque point la valeur issue du fichier geotif.
        """
        raster = gdal.Open(raster_src) # ouverture de l'image tif
        gt = raster.GetGeoTransform()
        wkt = raster.GetProjection()
        band = raster.GetRasterBand(1)
        nodata_raster = band.GetNoDataValue()
        points_values = []

        for i in range(len(shape)):
            #Convert from map to pixel coordinates.
            #Only works for geotransforms with no rotation.
            current_x = (shape[i].x - gt[0]) / gt[1]
            current_y = (shape[i].y - gt[3]) / gt[5]
            px = int(current_x) #x pixel
            py = int(current_y) #y pixel

            val = band.ReadAsArray(px,py,2,2)
            test_ok = (val[0][0] != nodata_raster and val[0][1] != nodata_raster and val[1][0] != nodata_raster and val[1][1] != nodata_raster)

            if val is not None and test_ok:
                value = ((1 - current_x%1) * (1 - current_y%1) * val[0][0]
                        + (current_x%1) * (1 - current_y%1) * val[0][1]
                        + (1 - current_x%1) * (current_y%1) * val[1][0]
                        + (current_x%1) * (current_y%1) * val[1][1])
                points_values.append(value)
            else :
                points_values.append(nodata)
        return points_values


    ################################################################
    # Création des listes d'intérêts venant des geotif
    liste_altitude_MNT = raster_to_points(path_MNT_alt, shape(shapes))
    liste_aspect_MNT = raster_to_points(path_MNT_asp, shape(shapes))
    liste_slope_MNT = raster_to_points(path_MNT_slop, shape(shapes))

    liste_altitude_MNT_arrondie = [ int(round(liste_altitude_MNT[i])) for i in range(len(liste_altitude_MNT)) ]
    liste_aspect_MNT_arrondie = [ int(round(liste_aspect_MNT[i]))%360 for i in range(len(liste_aspect_MNT)) ]
    liste_slope_MNT_arrondie = [ int(round(liste_slope_MNT[i])) for i in range(len(liste_slope_MNT)) ]

    ################################################################
    # Création liste massif
    liste_massif = [geomet_massif[j].record[Indice_record_massif] for i in range(len(shapes)) for j in range(len(shape_massif)) if list_shape_massif_WGS84[j].contains(list_shape_WGS84[i]) ]
    ''' MODE NON PYTHONIQUE POUR COMPRENDRE SI BESOIN: 
    liste_massif = []
    for i in range(len(shapes)):
        for j in range(len(shape_massif)):
            if list_shape_massif_WGS84[j].contains(list_shape_WGS84[i]):
                liste_massif.append(geomet_massif[j].record[0])
    FIN MODE NON PYTHONIQUE '''

    ################################################################
    # Création liste longitudes:
    liste_longitude = [round(list_shape_WGS84[i].x,6) for i in range(len(list_shape_WGS84))]
    # Création liste latitudes:
    liste_latitude = [round(list_shape_WGS84[i].y,6) for i in range(len(list_shape_WGS84))]
    # Création liste "id station" faite avec le field number de référence dans le shapefile
    # 8 chiffres significatifs pour être compatible avec les codes de infomassifs
    liste_id_station = ['%08d' % int(geomet[i].record[Indice_record_id_station] + add_for_METADATA) for i in range(len(shapes))]
    # Création liste "name station" faite avec le field name de référence dans le shapefile
    liste_nom_station = [geomet[i].record[Indice_record_nom_station] for i in range(len(shapes))]

    ################################################################
    # Création des listes via shapefile ou MNT + Vérification-Comparaison éventuelle avec MNT
    if Nom_alt is not None:
        liste_altitude = [geomet[i].record[Indice_record_altitude] for i in range(len(shapes))]
        print('########### Vérification MNT vs shapefile ##############')
        print('ALTITUDE:')
        print('écart max: ' + str(max([abs(liste_altitude_MNT[i] - liste_altitude[i]) for i in range(len(shapes))])))
        print('écart moyen: ' + str(np.mean([abs(liste_altitude_MNT[i] - liste_altitude[i]) for i in range(len(shapes))])))
    else:
        liste_altitude = liste_altitude_MNT_arrondie
    if Nom_asp is not None:
        liste_aspect = [geomet[i].record[Indice_record_aspect] for i in range(len(shapes))]
        print('########### Vérification MNT vs shapefile ##############')
        print('ORIENTATION:')
        print('écart max: ' + str(max([abs(liste_aspect_MNT[i] - liste_aspect[i]) for i in range(len(shapes))])))
        print('écart moyen: ' + str(np.mean([abs(liste_aspect_MNT[i] - liste_aspect[i]) for i in range(len(shapes))])))
    else:
        liste_aspect = liste_aspect_MNT_arrondie
    if Nom_slop is not None:
        liste_slope = [geomet[i].record[Indice_record_slope] for i in range(len(shapes))]
        print('########### Vérification MNT vs shapefile ##############')
        print('PENTE:')
        print('écart max: ' + str(max([abs(liste_slope_MNT[i] - liste_slope[i]) for i in range(len(shapes))])))
        print('écart moyen: ' + str(np.mean([abs(liste_slope_MNT[i] - liste_slope[i]) for i in range(len(shapes))])))
    else:
        liste_slope = liste_slope_MNT_arrondie


    return { 'lat': liste_latitude, 'lon':liste_longitude, 
             'alt': liste_altitude, 'asp': liste_aspect, 
             'm': liste_massif, 'slop': liste_slope,
             'id': liste_id_station, 'nom': liste_nom_station }


def check_Id_station_in_Metadata(all_lists):
    """
    Vérifie que les id des stations (création via make_dict_list) ne sont pas présentes dans METADATA.xml.
    On ne passe pas par cette routine si l'option confirm_overwrite est activée

    :param all_lists: Dictionnaire de listes pour avoir la liste des id_station
    :returns: au sens Python, ne retourne rien.

    Ecritures écrans et sortie du programme si les id stations sont présentes dans METADATA.xml. Sinon, il ne se passe rien
    """
    # chemin d ecriture du fichier XML
    chemxml = os.environ['SNOWTOOLS_CEN'] + "/DATA"
    # ouverture  du fichier en mode "lecture"/"ecriture"
    metadata = open(chemxml + "/METADATA.xml", 'r')
    # lire le fichier
    readfile = metadata.read()

    for station in all_lists['id']:
        if station in readfile:
            print(station, ' is in METADATA.xml')
            print('it means you are working with points with existing ID')
            print('IF you are REALLY sure of what you are doing:')
            print('you can avoid this message by using the option --confirm_overwrite')
            print('It is also possible that you are using an existing number project')
            sys.exit(10)

    metadata.close()



################################################################
# Creation du NetCDF
################################################################
def create_NetCDF(all_lists, output_name):
    """
    Crée un NetCDF 1D pour simulation réanalyse-projection à partir d'un dictionnaire de listes et d'un nom de sortie.
    Le dictionnaire de listes est issue d'un shapefile et se construit avec la routine make_dict_list.

    :param all_lists: Dictionnaire de listes pour remplir les variables du NetCDF
    :param output_name: Le nom du fichier NetCDF qui sera produit
    :type output_name: str

    :returns: au sens Python, ne retourne rien. Permet d'écrire un fichier à l'emplacement donné par output_name. 
    """
    outputs = Dataset(output_name, 'w', format='NETCDF4')
    outputs.createDimension('Number_of_points', len(all_lists['alt']))
    A = outputs.createVariable('LAT', np.float64,('Number_of_points',), fill_value=-9999999)
    B = outputs.createVariable('LON', np.float64,('Number_of_points',), fill_value=-9999999)
    C = outputs.createVariable('ZS', np.float64, ('Number_of_points',), fill_value=-9999999)
    D = outputs.createVariable('aspect', np.float64, ('Number_of_points',), fill_value=-9999999)
    E = outputs.createVariable('massif_num',int,('Number_of_points',), fill_value=-999)
    F = outputs.createVariable('slope', np.float64, ('Number_of_points',), fill_value=-9999999)
    G = outputs.createVariable('station', int,('Number_of_points',), fill_value=-9999999)

    outputs['LAT'][:] = all_lists['lat'] #liste_latitude
    outputs['LON'][:] = all_lists['lon'] #liste_longitude
    outputs['ZS'][:] = all_lists['alt'] #liste_altitude
    outputs['aspect'][:] = all_lists['asp'] #liste_aspect_
    outputs['massif_num'][:] = all_lists['m'] #liste_massif
    outputs['slope'][:] = all_lists['slop'] #liste_slope_
    outputs['station'][:] = all_lists['id'] #liste_id_station

    A.setncatts({'long_name': u"latitude", 'units': u"degrees_north"})    
    B.setncatts({'long_name': u"longitude", 'units': u"degrees_east"})
    C.setncatts({'long_name': u"altitude", 'units': u"m"})    
    D.setncatts({'long_name': u"slope aspect", 'units': u"degrees from north"})
    E.setncatts({'long_name': u"Massif Number"})
    F.setncatts({'long_name': u"slope angle", 'units': u"degrees from horizontal"})    
    G.setncatts({'long_name': u"Station OMM number"})
    
    outputs.close()


################################################################
# Creation des skyline
################################################################
def create_skyline(all_lists, path_MNT_alt, path_shapefile, list_skyline):
    """
    Ajoute dans le fichier METADATA.xml les points du shapefile en y ajoutant les masques
    (ie les hauteurs d'angle de vue pour les différents azimuts).


    :param all_lists : Dictionnaire de listes
    :param path_MNT_alt: Chemin du MNT contenant les valeurs d'altitude
    :type path_MNT_alt: str
    :param path_shapefile: Chemin du shapefile dont on souhaite extraire les points.
    :param path_shapefile: str
    :list_skyline: Liste des identifiants du shapefile dont on souhaite avoir le tracé des lignes d'horizon

    :returns: au sens Python, ne retourne rien. Permet d'une part de tracer les graphiques de ligne d'horizon et d'autre part de compléter METADATA.xml
    """
    start_time = time.time()

    in_file = [ [all_lists['id'][i], all_lists['alt'][i], all_lists['m'][i], all_lists['nom'][i], all_lists['lat'][i], all_lists['lon'][i] ] for i in range(len(all_lists['alt'][:])) ]
    # à in_file = np.loadtxt(file_in, dtype={'names': ('numposte', 'alt', 'massif', 'nom', 'lat', 'lon'), 'formats': (int, int, int, '|S24', float, float)})


    #######################################################
    #  AJOUT DE NOUVEAUX SITES DANS LE FICHIER XML Partie I
    #######################################################
    # Sites existants:
    SitesExistants = infomassifs().getListSites()

    # chemin d ecriture du fichier XML
    chemxml = os.environ['SNOWTOOLS_CEN'] + "/DATA"
    # ouverture  du fichier en mode "lecture"/"ecriture"
    metadata = open(chemxml + "/METADATA.xml", 'r')
    metadataout = open(chemxml + "/METADATA_out.xml", 'w')

    # faire la chaine de caractère des azimuts pour ecriture dans XML file
    azimut_str = ','.join( map(str, range(0, 360, 5)) )

    while True:
        line = metadata.readline()
        metadataout.write(line)
        if '<Sites>' in line:
            break
    #####################
    #  FIN AJOUT Partie I
    #####################


    # output folder for skyline graph (if asked via options)
    if not os.path.isdir("output"):
        os.mkdir("output")

    viewmax = 20000  # 20 km

    r = shapefile.Reader(path_shapefile)
    shapes = r.shapes()
    shape_courant = shape(shapes)

    raster = gdal.Open(path_MNT_alt) # ouverture de l'image tif
    gt = raster.GetGeoTransform()
    wkt = raster.GetProjection()
    band = raster.GetRasterBand(1)
    nodata_raster = band.GetNoDataValue()
    step = int((gt[1] + (-gt[5])) / 2)  # for further use in line interpolation
    print("step: ", step)

    for k in range(len(shape_courant)):
        in_stat = in_file[k]
        print('hello', in_stat[0], in_stat[3])
        center_x = (shape_courant[k].x - gt[0]) / gt[1]
        center_y = (shape_courant[k].y - gt[3]) / gt[5]

        px_c = int(center_x) #x pixel centre
        py_c = int(center_y) #y pixel centre

        # Idee: faire une interpolation biliénaire: -> voir quel pixel (pour orientation) mais en gros
        # f(x,y) = f(0,0)(1-x)(1-y) + f(1,0)x(1-y) + f(0,1)(1-x)y + f(1,1)xy avec x et y entre 0 et 1.
        # Ici, x et y sont les parties décimales de (shape_courant[k].x - gt[0]) / gt[1] et (shape_courant[k].y - gt[3]) / gt[5]
        # La fonction f est jouée par le tableau 2x2 band.ReadAsArray(px_c,py_c,2,2)
        # ! band.ReadAsArray(px_c,py_c,2,2)[0][1] correspond à band.ReadAsArray(px_c+1,py_c,1,1)[0][0]
        value_bilin = band.ReadAsArray(px_c,py_c,2,2)
        value_c = ((1 - center_x%1) * (1 - center_y%1) * value_bilin[0][0]
                  + (center_x%1) * (1 - center_y%1) * value_bilin[0][1]
                  + (1 - center_x%1) * (center_y%1) * value_bilin[1][0]
                  + (center_x%1) * (center_y%1) * value_bilin[1][1])

        print("alt: ", round(value_c,1))
        anglee = []
        for azimut in range(0, 360, 5):
            angle=[]
            for index,dist in enumerate(range(step, viewmax, step)):
                current_x = (shape_courant[k].x + dist * math.sin(math.radians(azimut)) - gt[0]) / gt[1]
                current_y = (shape_courant[k].y + dist * math.cos(math.radians(azimut)) - gt[3]) / gt[5]

                px = int(current_x) #x pixel le long de l'azimut
                py = int(current_y) #y pixel le long de l'azimut
                val = band.ReadAsArray(px,py,2,2)

                test_ok = (val[0][0] != nodata_raster and val[0][1] != nodata_raster and val[1][0] != nodata_raster and val[1][1] != nodata_raster)

                if val is not None and test_ok :
                    value = ((1 - current_x%1) * (1 - current_y%1) * val[0][0]
                            + (current_x%1) * (1 - current_y%1) * val[0][1]
                            + (1 - current_x%1) * (current_y%1) * val[1][0]
                            + (current_x%1) * (current_y%1) * val[1][1])

                    angle.append(math.ceil((math.degrees(math.atan(  (value - value_c) / dist ) )) * 100) / 100)
                else:
                    angle.append(0)
            anglee.append(max(angle))

        #print(anglee)
        az = [azimut for azimut in range(0, 360, 5)]
        angle_str = ','.join( map(str, anglee) )


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
            a.set_title(in_stat[3] + ' alt mnt:' + str(round(value_c,1)) + ' m alt poste:' + str(in_stat[1]))
            a.set_theta_zero_location('N')
            a.set_theta_direction(-1)
            plt.savefig('output/' + str(in_stat[0]) + '_skyline.png')
            # show()
            plt.close()


        ########################################################
        #  AJOUT DE NOUVEAUX SITES DANS LE FICHIER XML Partie II
        ########################################################
        if str(all_lists['id'][k]) not in SitesExistants:
            print("ajout du site : ", all_lists['nom'][k])

            metadataout.write('\t<Site>\n')
            metadataout.write('\t\t<name> ' + all_lists['nom'][k] + ' </name>\n')
            metadataout.write('\t\t<nameRed> ' + all_lists['nom'][k] + ' </nameRed>\n')
            metadataout.write('\t\t<number> ' + str(all_lists['id'][k]) + ' </number>\n')
            metadataout.write('\t\t<lat> ' + str(all_lists['lat'][k]) + ' </lat>\n')
            metadataout.write('\t\t<lon> ' + str(all_lists['lon'][k]) + ' </lon>\n')
            metadataout.write('\t\t<altitude> ' + str(all_lists['alt'][k]) + ' </altitude>\n')
            metadataout.write('\t\t<aspect> ' + str(all_lists['asp'][k]) + ' </aspect>\n')
            metadataout.write('\t\t<slope> ' + str(all_lists['slop'][k]) + ' </slope>\n')
            metadataout.write('\t\t<massif> ' + str(all_lists['m'][k]) + ' </massif>\n')
            metadataout.write('\t\t<zref> ' + "1.5" + ' </zref>\n')
            metadataout.write('\t\t<uref> ' + "10.0" + ' </uref>\n')
            metadataout.write('\t\t<azimut> ' + azimut_str.rstrip() + ' </azimut>\n')
            metadataout.write('\t\t<mask> ' + angle_str.rstrip() + ' </mask>\n')
            metadataout.write('\t\t<source_mask> ' + 'IGN' + ' </source_mask>\n')
            metadataout.write('\t</Site>\n')

        else:
            lati_base, longi_base, alti_base = infomassifs().infoposte(str(all_lists['id'][k]))
            expo_base, slope_base = infomassifs().exposlopeposte(str(all_lists['id'][k]))
            try:
                # Ce truc va planter s'il n'y a pas encore de champ massif
                massif_base = infomassifs().massifposte(str(all_lists['id'][k]))
            except Exception:
                massif_base = -1

            if all_lists['alt'][k] != alti_base:
                print("WARNING ALTITUDE : " + all_lists['id'][k])
                print(all_lists['alt'][k], alti_base)

            if all_lists['asp'][k] != expo_base:
                print("WARNING ASPECT : " + all_lists['id'][k])
                print(all_lists['asp'][k], expo_base)

            if all_lists['slop'][k] != slope_base:
                print("WARNING SLOPE : " + all_lists['id'][k])
                print(all_lists['slop'][k], slope_base)

            if all_lists['lat'][k] != lati_base:
                print("WARNING LATITUDE : " + all_lists['id'][k])
                print(all_lists['lat'][k], lati_base)

            if all_lists['lon'][k] != longi_base:
                print("WARNING LONGITUDE : " + all_lists['id'][k])
                print(all_lists['lon'][k], longi_base)

            if all_lists['m'][k] != massif_base:
                print("UPDATE MASSIF NUMBER : " + all_lists['id'][k])
                metadataout.write('\t<Site>\n')
                metadataout.write('\t\t<name> ' + all_lists['nom'][k] + ' </name>\n')
                metadataout.write('\t\t<nameRed> ' + all_lists['nom'][k] + ' </nameRed>\n')
                metadataout.write('\t\t<number> ' + str(all_lists['id'][k]) + ' </number>\n')
                metadataout.write('\t\t<lat> ' + str(all_lists['lat'][k]) + ' </lat>\n')
                metadataout.write('\t\t<lon> ' + str(all_lists['lon'][k]) + ' </lon>\n')
                metadataout.write('\t\t<altitude> ' + str(all_lists['alt'][k]) + ' </altitude>\n')
                metadataout.write('\t\t<aspect> ' + str(all_lists['asp'][k]) + ' </aspect>\n')
                metadataout.write('\t\t<slope> ' + str(all_lists['slop'][k]) + ' </slope>\n')
                metadataout.write('\t\t<massif> ' + str(all_lists['m'][k]) + ' </massif>\n')
                metadataout.write('\t\t<zref> ' + "1.5" + ' </zref>\n')
                metadataout.write('\t\t<uref> ' + "10.0" + ' </uref>\n')
                metadataout.write('\t\t<azimut> ' + azimut_str.rstrip() + ' </azimut>\n')
                metadataout.write('\t\t<mask> ' + angle_str.rstrip() + ' </mask>\n')
                metadataout.write('\t\t<source_mask> ' + 'IGN' + ' </source_mask>\n')
                metadataout.write('\t</Site>\n')

    # On écrit la fin du fichier
    for line in metadata.readlines():
        metadataout.write(line)

        if "<number>"in line:
            if re.match("^.*(\d{8}).*$", line):
                codestation = re.split("^.*(\d{8}).*$", line)[1]
                if codestation == all_lists['id'][k]:
                    metadataout.write('\t\t<massif> ' + str(all_lists['m'][k]) + ' </massif>\n')


    metadata.close()
    os.system('mv -f ' + chemxml + '/METADATA_out.xml ' + chemxml + '/METADATA.xml')
    print("done in", time.time() - start_time, "seconds")

def parseArguments(args):
    """Parsing the arguments when you call the main program.

    :param args: The list of arguments when you call the main program (typically sys.argv[1:] )
    """

    # Create argument parser
    parser = argparse.ArgumentParser()

    # Mandatory argument
    parser.add_argument("path_shape", help = "Path to shapefile", type = str)
    parser.add_argument("name_station", help = "Shapefile Field Name containing a unique reference for points", type = str)
    parser.add_argument("id_station", help = "Shapefile Field Number containing a unique reference for points", type = str)
    parser.add_argument("project_number", help = "Project Number in order to get unique reference in METADATA", type = int)

    # Optional argument
    parser.add_argument("--name_alt", help = "Shapefile Field Name containing altitude, if it exists", type = str, default = None)
    parser.add_argument("--name_asp", help = "Shapefile Field Name containing altitude, if it exists", type = str, default = None)
    parser.add_argument("--name_slop", help = "Shapefile Field Name containing altitude, if it exists", type = str, default = None)
    parser.add_argument("-o", "--output", help = "Name for NetCDF file to save", type = str, default = NetCDF_out)
    parser.add_argument("--MNT_alt", help = "Path for MNT altitude", type = str, default = path_MNT_alti_defaut)
    parser.add_argument("--MNT_asp", help = "Path for MNT altitude", type = str, default = path_MNT_aspect_defaut)
    parser.add_argument("--MNT_slop", help = "Path for MNT altitude", type = str, default = path_MNT_slope_defaut)
    parser.add_argument("--list_skyline", nargs='*', help = "The skyline plot you want", default = None)
    parser.add_argument("--confirm_overwrite", help = "Confirm you want to overwrite", action = "store_true")

    args = parser.parse_args(args)
    return args

def main(args=None):
    """Main program: parse argument then launch plot and text comparison for the 2 PRO files

    """
    args = args if args is not None else sys.argv[1:]
    if len(sys.argv) > 1: 
        args = parseArguments(args)

        # argument for command line call
        path_shapefile = args.path_shape
        Nom_station = args.name_station
        Id_station = args.id_station
        Project_number = args.project_number
        Nom_alt = args.name_alt
        Nom_asp = args.name_asp
        Nom_slop = args.name_slop
        path_MNT_alt = args.MNT_alt
        path_MNT_asp = args.MNT_asp
        path_MNT_slop = args.MNT_slop
        output_name = args.output
        list_skyline = args.list_skyline
        confirm_overwrite = args.confirm_overwrite

        # Check if mandatory path for shapefile is OK
        if not os.path.isfile(path_shapefile + '.shp'):
            logger.critical('Provided path for shapefile file does not exist ({})'.format(path_shapefile))
            sys.exit(1)

        # Check if mandatory field to play the role of station in shapefile is OK
        r = shapefile.Reader(path_shapefile)
        if Nom_station not in [r.fields[i][0] for i in range(len(r.fields))]: 
            logger.critical('Field {} does not exist in {}'.format(Nom_station, path_shapefile))
            sys.exit(3)
        if Id_station not in [r.fields[i][0] for i in range(len(r.fields))]: 
            logger.critical('Field {} does not exist in {}'.format(Id_station, path_shapefile))
            sys.exit(3)

        # Check if mandatory project number
        if Project_number > 99:
            logger.critical('Project Number should be between 0 and 99. Use your favorite editor to read the code shapefile2NetCDF.py')
            sys.exit(1)

        # Launch the app:
        all_lists = make_dict_list(path_shapefile, Id_station, Nom_station, Nom_alt, Nom_asp, Nom_slop, path_MNT_alt, path_MNT_asp, path_MNT_slop, 10000000 + 10000*Project_number)
        if not confirm_overwrite:
            check_Id_station_in_Metadata(all_lists)
        create_NetCDF(all_lists, output_name)
        if list_skyline == ['all'] or list_skyline == ['All'] or list_skyline == ['ALL']:
            list_skyline = [all_lists['id'][k] for k in range(len(all_lists['id']))]
        elif list_skyline is not None:
            list_skyline = ['%08d' % (10000000 + 10000*Project_number + int( list_skyline[i])) for i in range(len(list_skyline))]
        create_skyline(all_lists, path_MNT_alt, path_shapefile, list_skyline)

if __name__ == '__main__':
    main()
