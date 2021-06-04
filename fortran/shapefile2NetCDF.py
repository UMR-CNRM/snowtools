#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import netCDF4 as nc
from netCDF4 import Dataset
import numpy as np
import gdal
from shapely.geometry import shape, Polygon, LineString
from shapely.ops import transform
from functools import partial
import pyproj

# Bibliothèque ad hoc de Matthieu L pour ouvrir les shapefiles
import shapefile

################################################################
# On part d'un shapefile pour l'ANR TOP (en Lambert93)
# Ce shapefile va être transformé en NetCDF pour les besoins
# du lancement de Surfex (ou de réanalyse) dessus
# Pour cela, il faut: 
# - savoir à quels massifs les points appartiennent
# - trouver la correspondance des points sur le MNT pour en donner la pente et l'orientation
# - NB: on mettra aussi l'altitude pour vérification avec les données proposées 
#
################################################################




################################################################
############### EN DUR DANS LE CODE:
################################################################
################################################################
# Path pour le(s) MNT (! ici choix du MNT 30m):
################################################################
path_MNT = '/rd/cenfic2/manto/haddjeria/MNT/finalized/MNT_FRANCEandBORDER_30m_fusion:IGN5m+COPERNICUS30m_EPSG:2154_INT:AVERAGE_2021-03.tif'
path_MNT_work_hors_cen = '/home/fructusm/MNT_FRANCEandBORDER_30m_fusion:IGN5m+COPERNICUS30m_EPSG:2154_INT:AVERAGE_2021-03.tif'
path_MNT_slope = '/home/fructusm/MNT_slope.tif'
path_MNT_aspect = '/home/fructusm/MNT_aspect.tif'

################################################################
# Path pour lecture shapefile
################################################################
path_shapefile = '/home/fructusm/Téléchargements/Plots2020/plots'
path_shapefile_massif = '/home/fructusm/git/snowtools_git/DATA/massifs_Lbrt93_2019' #L93
#path_shapefile_massif = '/home/fructusm/git/snowtools_git/DATA/massifs_alpes' # Lambert II

################################################################
# Indice de colonnes d'intérêt (numéro massif, équivalent station shapefile d'entrée
################################################################
Indice_record_massif = 0
Indice_record_station = 5
Indice_record_altitude = 4

################################################################
# Nom NetCDF de sortie
################################################################
NetCDF_out = 'NetCDF_ANR_TOP.nc'

################################################################
############### FIN DUR DANS LE CODE:
################################################################




################################################################
# Ouverture du shapefile d'intérêt. Conversion éventuelle
################################################################
r = shapefile.Reader(path_shapefile)
shapes = r.shapes()
geomet = r.shapeRecords()

# Permet de convertir du Lambert93 (EPSG 2154) en WGS84 (EPSG 4326)
# Lambert93: coordonnées en mètre sur la France métropolitaine élargie (avec Corse)
# WSG84: coordonnées en (lon, lat) type GPS pour le monde entier
project_from_L93_to_WGS84 = partial(pyproj.transform, pyproj.Proj(init='epsg:2154'),pyproj.Proj(init='epsg:4326'))
list_shape_WGS84 = [transform(project_from_L93_to_WGS84,shape(shapes[i])) for i in range(len(shapes))]

# Un petit print pour voir les attributs du shapefile: noms, coordonnées, altitude, ... 
Just_to_see = geomet[0].record
print(Just_to_see)


################################################################
# Ouverture du shapefile massif. Conversion éventuelle
################################################################
'''#######
# Version snwotools_git/DATA actuelle
#######
# Ouverture du shapefile massif
massif = shapefile.Reader(path_shapefile_massif)
shape_massif = massif.shapes()
geomet_massif = massif.shapeRecords()

# Permet de convertir du Lambert_II (EPSG 27572) en WGS84 (EPSG 4326)
# Lambert_II: coordonnées en mètre sur la France métropolitaine (sans la Corse ? voir https://epsg.io/27572 ) 
# WSG84: coordonnées en (lon, lat) type GPS pour le monde entier
project_from_LII_to_WGS84 = partial(pyproj.transform, pyproj.Proj(init='epsg:27572'),pyproj.Proj(init='epsg:4326'))
list_shape_massif_WGS84 = [transform(project_from_LII_to_WGS84,shape(shape_massif[i])) for i in range(len(shape_massif))]'''

#######
# Version snwotools_git/DATA future
#######
# Ouverture du shapefile massif
# NB: Bien vérifier sur le shapefile massif que le numéro de massif est record[0].
# Par exemple, pour les shapefile massif en LambertII, c'est record[1] qu'il faut prendre -> à changer dans Détermination du massif
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
    Associe pour chaque point du tableau gdf_points la valeur du pixel le plus proche
    issu du raster_src.
    
    Attention : - le fichier géotif et le shape doivent être dans la même projection
                - la projection ne doit pas utiliser de rotation (Lambert 93 OK, WSG84 pas clair du tout)
    
    Parameters
    ----------
    raster_src : str
        Chemin du fichier géotif dont on souhaite extraire les valeurs.
    shape : shapely.geometry.collection.GeometryCollection (obtenu avec shape( shapefile.Reader('...').shapes() ) 
        Liste contenant la liste des points.
    nodata : Int,float, optional
        Valeurs attribuée aux points n'ayant pas de pixel à proximité. The default is np.nan.

    Returns
    -------
    points_values : Liste
        Liste dans le même ordre que la GeometryCollection fournie en entrée,
        et contenant pour chaque point la valeur issue du fichier geotif.

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
################################################################
liste_altitude_MNT = raster_to_points(path_MNT_work_hors_cen, shape(shapes))
liste_aspect_MNT = raster_to_points(path_MNT_aspect, shape(shapes))
liste_slope_MNT = raster_to_points(path_MNT_slope, shape(shapes))


################################################################
# Détermination du massif
################################################################
liste_massif = [geomet_massif[j].record[Indice_record_massif] for i in range(len(shapes)) for j in range(len(shape_massif)) if list_shape_massif_WGS84[j].contains(list_shape_WGS84[i]) ]
''' MODE NON PYTHONIQUE POUR COMPRENDRE SI BESOIN: 
liste_massif = []
for i in range(len(shapes)):
    for j in range(len(shape_massif)):
        if list_shape_massif_WGS84[j].contains(list_shape_WGS84[i]):
            liste_massif.append(geomet_massif[j].record[0])
FIN MODE NON PYTHONIQUE '''


################################################################
# Création des listes d'intérêts (latitude, longitude, attributs divers...)
################################################################
# Création de la liste des longitudes:
liste_longitude = [list_shape_WGS84[i].x for i in range(len(list_shape_WGS84))]

# Création de la liste des latitudes:
liste_latitude = [list_shape_WGS84[i].y for i in range(len(list_shape_WGS84))]

# Création de la liste des altitudes via les attributs
liste_altitude = [geomet[i].record[Indice_record_altitude] for i in range(len(shapes))]

# Création de la liste "station" faite avec les id_plot
liste_station_idplot = [geomet[i].record[Indice_record_station] for i in range(len(shapes))]

################################################################
# Evaluation du MNT en regardant la correspondance altitude MNT vs altitude du shapefile donné
################################################################
print(max([abs(liste_altitude_MNT[i] - liste_altitude[i]) for i in range(len(liste_altitude))]))
print(np.mean([abs(liste_altitude_MNT[i] - liste_altitude[i]) for i in range(len(liste_altitude))]))



################################################################
# Creation du NetCDF
################################################################



outputs = Dataset('NetCDF_ANR_TOP.nc', 'w', format='NETCDF4')
outputs.createDimension('Number_of_points', len(liste_latitude))
A = outputs.createVariable('LAT', np.float64,('Number_of_points',), fill_value=-9999999)
B = outputs.createVariable('LON', np.float64,('Number_of_points',), fill_value=-9999999)
C = outputs.createVariable('ZS', np.float64, ('Number_of_points',), fill_value=-9999999)
D = outputs.createVariable('aspect', np.float64, ('Number_of_points',), fill_value=-9999999)
E = outputs.createVariable('massif_num',int,('Number_of_points',), fill_value=-999)
F = outputs.createVariable('slope', np.float64, ('Number_of_points',), fill_value=-9999999)
G = outputs.createVariable('station', int,('Number_of_points',), fill_value=-9999999)

outputs['LAT'][:] = liste_latitude
outputs['LON'][:] = liste_longitude
outputs['ZS'][:] = liste_altitude
outputs['aspect'][:] = liste_aspect_MNT
outputs['massif_num'][:] = liste_massif
outputs['slope'][:] = liste_slope_MNT
outputs['station'][:] = liste_station_idplot


A.setncatts({'long_name': u"latitude",\
             'units': u"degrees_north"})    
B.setncatts({'long_name': u"longitude",\
              'units': u"degrees_east"})
C.setncatts({'long_name': u"altitude",\
             'units': u"m"})    
D.setncatts({'long_name': u"slope aspect",\
              'units': u"degrees from north"})
E.setncatts({'long_name': u"Massif Number"})
F.setncatts({'long_name': u"slope angle",\
             'units': u"degrees from horizontal"})    
G.setncatts({'long_name': u"Station OMM number"})
    
outputs.close()




