#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import argparse
import sys
from netCDF4 import Dataset
import numpy as np
import gdal
from shapely.geometry import shape, Polygon
from shapely.ops import transform
from functools import partial
import pyproj

import logging
logger = logging.getLogger()
logger.setLevel(logging.WARNING)
console_handler = logging.StreamHandler()
console_handler.setFormatter(logging.Formatter('%(levelname)s :: %(message)s'))
console_handler.setLevel(logging.DEBUG)
logger.addHandler(console_handler)

# Bibliothèque ad hoc de Matthieu L pour ouvrir les shapefiles
import shapefile


################################################################
# On part d'un shapefile en Lambert 93
# De ce shapefile, le programme fournit un NetCDF pour les besoins du lancement de Surfex (ou de réanalyse) dessus
#
# Lancement:
# python3 shapefile2NetCDF.py path_shapefile nom_point_in_shapefile id_point_in_shapefile [--name_alt alti_name_in_shapefile] [-o path_name_of_NetCDF_output] [--MNT_alt path_of_MNT_altitude]
#
# Exemple d'appel
# python3 shapefile2NetCDF.py /home/fructusm/Téléchargements/Plots2020/plots codeplot idplot --name_alt alti
#
################################################################



################################################################
############### EN DUR DANS LE CODE:
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
############### FIN DUR DANS LE CODE:
################################################################





################################################################
# Creation des listes de données
################################################################
def make_list(path_shapefile, Id_station, Nom_alt, Nom_asp, Nom_slop, path_MNT_alt, path_MNT_asp, path_MNT_slop):
    """
    Crée une liste de listes à partir des données du shapefile et des données des MNT

    :param str path_shapefile : Chemin du shapefile dont on souhaite extraire les valeurs.
    :param str Id_station : Numéro de l'attribut du shapefile correspondant à un identifiant unique pour chaque point
    :param str Nom_alt : si présent dans le shapefile, nom de l'attribut d'altitude. Si absent, ce champ est à None
    :param str Nom_asp : si présent dans le shapefile, nom de l'attribut d'orientation. Si absent, ce champ est à None
    :param str Nom_slop : si présent dans le shapefile, nom de l'attribut de pente. Si absent, ce champ est à None
    :param str path_MNT_alt : Chemin du MNT contenant les valeurs d'altitude
    :param str path_MNT_asp : Chemin du MNT contenant les valeurs d'aspect
    :param str path_MNT_slop : Chemin du MNT contenant les valeurs de pente

    :returns: liste de listes dans l'ordre suivant: [ liste_latitude, liste_longitude, liste_altitude, liste_aspect, liste_massif, liste_slope, liste_station ] 
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
            Indice_record_station = i - 1

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
        Associe pour chaque point de la GeometryCollection shape en entrée la valeur du pixel le plus proche issu du raster_src.
    
        Attention : - le fichier géotif et le shape doivent être dans la même projection
                    - la projection ne doit pas utiliser de rotation (Lambert 93 OK, WSG84 pas clair du tout)

        :param str raster_src : Chemin du fichier géotif dont on souhaite extraire les valeurs.
        :param shape : Liste contenant la liste des points. Format en shapely.geometry.collection.GeometryCollection (obtenu avec shape( shapefile.Reader('...').shapes() ) 
        :param float nodata: Valeurs attribuée aux points n'ayant pas de pixel à proximité. The default is np.nan.

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
            px = int((shape[i].x - gt[0]) / gt[1]) #x pixel
            py = int((shape[i].y - gt[3]) / gt[5]) #y pixel

            value = band.ReadAsArray(px,py,1,1)

            if value is not None and value[0][0] != nodata_raster  :
                points_values.append(value[0][0])
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
    liste_longitude = [list_shape_WGS84[i].x for i in range(len(list_shape_WGS84))]
    # Création liste latitudes:
    liste_latitude = [list_shape_WGS84[i].y for i in range(len(list_shape_WGS84))]
    # Création liste "station" faite avec le field name de référence dans le shapefile
    liste_station = [geomet[i].record[Indice_record_station] for i in range(len(shapes))]

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


    return [liste_latitude, liste_longitude, liste_altitude, liste_aspect, liste_massif, liste_slope, liste_station]


################################################################
# Creation du NetCDF
################################################################
def create_NetCDF(all_lists, output_name):
    """
    Crée un NetCDF 1D pour simulation réanalyse-projection à partir d'une liste de listes et d'un nom de sortie.
    
    La liste de listes est issue d'un shapefile. L'ordre est important:
    [ liste_latitude, liste_longitude, liste_altitude, liste_aspect, liste_massif, liste_slope, liste_station ]  


    :param all_lists : Liste de listes pour remplir les variables du NetCDF
    :param str output_name : Le nom du fichier NetCDF qui sera produit

    :returns: au sens Python, ne retourne rien. Permet d'écrire un fichier à l'emplacement donné par output_name. 
    """
    outputs = Dataset(output_name, 'w', format='NETCDF4')
    outputs.createDimension('Number_of_points', len(all_lists[0]))
    A = outputs.createVariable('LAT', np.float64,('Number_of_points',), fill_value=-9999999)
    B = outputs.createVariable('LON', np.float64,('Number_of_points',), fill_value=-9999999)
    C = outputs.createVariable('ZS', np.float64, ('Number_of_points',), fill_value=-9999999)
    D = outputs.createVariable('aspect', np.float64, ('Number_of_points',), fill_value=-9999999)
    E = outputs.createVariable('massif_num',int,('Number_of_points',), fill_value=-999)
    F = outputs.createVariable('slope', np.float64, ('Number_of_points',), fill_value=-9999999)
    G = outputs.createVariable('station', int,('Number_of_points',), fill_value=-9999999)

    outputs['LAT'][:] = all_lists[0] #liste_latitude
    outputs['LON'][:] = all_lists[1] #liste_longitude
    outputs['ZS'][:] = all_lists[2] #liste_altitude
    outputs['aspect'][:] = all_lists[3] #liste_aspect_
    outputs['massif_num'][:] = all_lists[4] #liste_massif
    outputs['slope'][:] = all_lists[5] #liste_slope_
    outputs['station'][:] = all_lists[6] #liste_station

    A.setncatts({'long_name': u"latitude", 'units': u"degrees_north"})    
    B.setncatts({'long_name': u"longitude", 'units': u"degrees_east"})
    C.setncatts({'long_name': u"altitude", 'units': u"m"})    
    D.setncatts({'long_name': u"slope aspect", 'units': u"degrees from north"})
    E.setncatts({'long_name': u"Massif Number"})
    F.setncatts({'long_name': u"slope angle", 'units': u"degrees from horizontal"})    
    G.setncatts({'long_name': u"Station OMM number"})
    
    outputs.close()



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

    # Optional argument
    parser.add_argument("--name_alt", help = "Shapefile Field Name containing altitude, if it exists", type = str, default = None)
    parser.add_argument("--name_asp", help = "Shapefile Field Name containing altitude, if it exists", type = str, default = None)
    parser.add_argument("--name_slop", help = "Shapefile Field Name containing altitude, if it exists", type = str, default = None)
    parser.add_argument("-o", "--output", help = "Name for NetCDF file to save", type = str, default = NetCDF_out)
    parser.add_argument("--MNT_alt", help = "Path for MNT altitude", type = str, default = path_MNT_alti_defaut)
    parser.add_argument("--MNT_asp", help = "Path for MNT altitude", type = str, default = path_MNT_aspect_defaut)
    parser.add_argument("--MNT_slop", help = "Path for MNT altitude", type = str, default = path_MNT_slope_defaut)

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
        Nom_alt = args.name_alt
        Nom_asp = args.name_asp
        Nom_slop = args.name_slop
        path_MNT_alt = args.MNT_alt
        path_MNT_asp = args.MNT_asp
        path_MNT_slop = args.MNT_slop
        output_name = args.output

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

        # Launch the app
        all_lists = make_list(path_shapefile, Id_station, Nom_alt, Nom_asp, Nom_slop, path_MNT_alt, path_MNT_asp, path_MNT_slop)
        create_NetCDF(all_lists, output_name)

if __name__ == '__main__':
    main()

