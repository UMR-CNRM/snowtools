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
# Il va aussi fournir les skyline
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




# Mise en place des log
logger = logging.getLogger()
logger.setLevel(logging.WARNING)
console_handler = logging.StreamHandler()
console_handler.setFormatter(logging.Formatter('%(levelname)s :: %(message)s'))
console_handler.setLevel(logging.DEBUG)
logger.addHandler(console_handler)


################################################################
# Creation des listes de données
################################################################
def make_dict_list(path_shapefile, Id_station, Nom_station, Nom_alt, Nom_asp, Nom_slop, path_MNT_alt, path_MNT_asp, path_MNT_slop):
    """
    Crée un dictionnaire de listes à partir des données du shapefile et des données des MNT

    :param str path_shapefile : Chemin du shapefile dont on souhaite extraire les valeurs.
    :param str Id_station : Numéro de l'attribut du shapefile correspondant à un identifiant unique pour chaque point
    :param str Nom_alt : si présent dans le shapefile, nom de l'attribut d'altitude. Si absent, ce champ est à None
    :param str Nom_asp : si présent dans le shapefile, nom de l'attribut d'orientation. Si absent, ce champ est à None
    :param str Nom_slop : si présent dans le shapefile, nom de l'attribut de pente. Si absent, ce champ est à None
    :param str path_MNT_alt : Chemin du MNT contenant les valeurs d'altitude
    :param str path_MNT_asp : Chemin du MNT contenant les valeurs d'aspect
    :param str path_MNT_slop : Chemin du MNT contenant les valeurs de pente

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
    # Création liste "id station" faite avec le field number de référence dans le shapefile
    liste_id_station = [geomet[i].record[Indice_record_id_station] for i in range(len(shapes))]
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


################################################################
# Creation du NetCDF
################################################################
def create_NetCDF(all_lists, output_name):
    """
    Crée un NetCDF 1D pour simulation réanalyse-projection à partir d'une liste de listes et d'un nom de sortie.
    
    La liste de listes est issue d'un shapefile. L'ordre est important:
    [ liste_latitude, liste_longitude, liste_altitude, liste_aspect, liste_massif, liste_slope, liste_id_station ]  


    :param all_lists : Liste de listes pour remplir les variables du NetCDF
    :param str output_name : Le nom du fichier NetCDF qui sera produit

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
def create_skyline(all_lists, path_MNT_alt):
    start_time = time.time()
    mnt = path_MNT_alt

    in_file = [ [all_lists['id'][i], all_lists['alt'][i], all_lists['m'][i], all_lists['nom'][i], all_lists['lat'][i], all_lists['lon'][i] ] for i in range(len(all_lists['alt'][:])) ]
    # à in_file = np.loadtxt(file_in, dtype={'names': ('numposte', 'alt', 'massif', 'nom', 'lat', 'lon'), 'formats': (int, int, int, '|S24', float, float)})

    n = int(len(all_lists['alt'][:]))

    # load complete raster
    img = gdal.Open(mnt)  # 50 m Lambert 93
    band1 = img.GetRasterBand(1)
    rastinit = img.GetGeoTransform()
    step = int((rastinit[1] + (-rastinit[5])) / 2)  # for further use in line interpolation

    # x,y geographic reference matrix
    imgx = np.zeros((1, img.RasterXSize)).astype(np.float)
    imgy = np.zeros((img.RasterYSize, 1)).astype(np.float)
    for i in range(0, imgx.shape[1]):
        imgx[0, i] = rastinit[0] + (i * rastinit[1])
    for i in range(0, imgy.shape[0]):
        imgy[i, 0] = rastinit[3] + (i * rastinit[5])

    # output to csv file
    if not os.path.isdir("output"):
        os.mkdir("output")

    viewmax = 20000  # 20 km

    # reprojection to L93
    source = osr.SpatialReference()
    source.ImportFromEPSG(4326)
    target = osr.SpatialReference()
    target.ImportFromEPSG(2154)
    transform = osr.CoordinateTransformation(source, target)








    # Quels sont les sites existants ?
    SitesExistants = infomassifs().getListSites()
    ################################################################################################
    #                                                                                              #
    #  AJOUT DE NOUVEAUX SITES DANS LE FICHIER XML                                   #
    #                                                                                              #
    ################################################################################################
    # chemin d ecriture du fichier XML
    chemxml = os.environ['SNOWTOOLS_CEN'] + "/DATA"
    # ouverture  du fichier en mode "lecture"/"ecriture"
    metadata = open(chemxml + "/METADATA.xml", 'r')
    metadataout = open(chemxml + "/METADATA_out.xml", 'w')

    # faire la chaine de caractère des azimuts pour ecriture dans XML file
    azimut_str = ''
    for x in map(str, range(0, 360, 5) ):
        azimut_str = azimut_str + x + ' '

    while True:
        line = metadata.readline()
        metadataout.write(line)
        if '<Sites>' in line:
            break







    # extract from original raster
    for k in range(n):
        in_stat = in_file[k]


        print('hello', in_stat[0], in_stat[3])
        # tranformation des coordonnées geo en L93
        point = ogr.Geometry(ogr.wkbPoint)
        # print in_stat[4], in_stat[5]
        point.AddPoint(in_stat[5], in_stat[4])  # coord lat lon
        point.Transform(transform)
        coord = point.ExportToWkt()
        # print coord
        xx = math.floor(point.GetX())
        yy = math.floor(point.GetY())
        print(xx, yy)
        ####



        # Find row/col information et xy normalization
        xmin = rastinit[0] + ((math.floor(((xx - viewmax) - rastinit[0]) / rastinit[1])) * rastinit[1])
        xmax = rastinit[0] + ((math.floor(((xx + viewmax) - rastinit[0]) / rastinit[1])) * rastinit[1])
        ymin = rastinit[3] - ((math.ceil((rastinit[3] - (yy - viewmax)) / rastinit[5])) * rastinit[5])
        ymax = rastinit[3] - ((math.ceil((rastinit[3] - (yy + viewmax)) / rastinit[5])) * rastinit[5])
        stax = rastinit[0] + ((math.floor((xx - rastinit[0]) / rastinit[1])) * rastinit[1])
        stay = rastinit[3] - (math.ceil((rastinit[3] - yy) / rastinit[5]) * rastinit[5])
        if ymax >= max(imgy):
            minrow = 0
        else:
            minrow = np.unique(np.argwhere(imgy == ymax))[1]
        if ymin <= min(imgy):
            maxrow = imgy.shape[0]
        else:
            maxrow = np.unique(np.argwhere(imgy == ymin))[1]
        if xmin <= min(imgx[0, ]):
            mincol = 0
        else:
            mincol = np.unique(np.argwhere(imgx == xmin))[1]
        if xmax >= max(imgx[0, ]):
            maxcol = imgx.shape[1]
        else:
            maxcol = np.unique(np.argwhere(imgx == xmax))[1]
            # douteux: les 6 lignes qui suivent devraient être "désindentés" -> fait au cas où. 
        starow = maxrow - np.unique(np.argwhere(imgy == stay))[1]
        stacol = np.unique(np.argwhere(imgx == stax))[1] - mincol
        starow = starow.astype('int64')
        stacol = stacol.astype('int64')
        #sta_xy = (stax + (rastinit[1] / 2), stay + (rastinit[5] / 2)) pas d'utilité ?
        sta_rc = (starow, stacol)
        # Extract array from raster
        print(mincol, minrow, maxcol - mincol, maxrow - minrow)
        height = band1.ReadAsArray(int(mincol), int(minrow), int(maxcol - mincol), int(maxrow - minrow))
        height = height.astype('int64')
        # get width and heigth of image
        w, h = height.shape
        print("raster extracted", w, h)
        z_alt = height[sta_rc]
        print(height[sta_rc])
        # Get all intersected cells on azimuth 




        az = []
        anglee = []
        angle_str=''
        for azimut in range(0, 360, 5):
            i = 0
            angle = np.zeros((1, (viewmax // step) )).astype(np.float)  # initialize container for angles
            points = []  # initialize container for points
            pt_dist = []
            for dist in range(step, viewmax, step):
                ptx = xx + (dist * math.sin(math.radians(azimut)))
                pty = yy + (dist * math.cos(math.radians(azimut)))
                pt = (ptx, pty)
                points.append(pt)
                pt_dist.append(dist)
            # get row col information
                if ptx < xmax and ptx > xmin:
                    x = rastinit[0] + ((math.floor((ptx - rastinit[0]) / rastinit[1])) * rastinit[1])
                    ptcol = np.unique(np.argwhere(imgx == x))[1] - mincol
                if pty < ymax and pty > ymin:
                    y = rastinit[3] - ((math.ceil((rastinit[3] - pty) / rastinit[5])) * rastinit[5])
                    ptrow = np.unique(np.argwhere(imgy == y))[1] - minrow
                    ptrc = (ptrow, ptcol)
                # print dist, height[ptrc]-height[sta_rc], x,y, stax, stay, ptrc, sta_rc
                # calculate corresponding angle to reach the height of pt
                if ptrow < w and ptcol < h:
                    b = height[ptrc] - height[sta_rc]  # sta[7]
                    b = b.astype('float')
                # print b, b/dist, type(b), type(dist), type(b/dist)#)))*100)/100
                    if b > 0:
                        angle[0, i] = math.ceil((math.degrees(math.atan(b / dist))) * 100) / 100
                    else:
                        angle[0, i] = 0
                i = i + 1

            az = az + [azimut]
            anglee = anglee + [max(angle[0, ])]
            angle_str = angle_str + str(max(angle[0, ])) + ' '

 


        # PLOT
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
        a.set_title(in_stat[3] + ' alt mnt:' + str(z_alt) + ' m alt poste:' + str(in_stat[1]))
        a.set_theta_zero_location('N')
        a.set_theta_direction(-1)
        plt.savefig('output/' + str(in_stat[0]) + '_skyline.png')
        # show()





        if all_lists['id'][k] not in SitesExistants:
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
            metadataout.write('\t\t<azimut> ' + azimut_str + '</azimut>\n')
            metadataout.write('\t\t<mask> ' + angle_str + '</mask>\n')
            metadataout.write('\t</Site>\n')

        else:
            lati_base, longi_base, alti_base = infomassifs().infoposte(all_lists['id'][k])
            expo_base, slope_base = infomassifs().exposlopeposte(all_lists['id'][k])
            try:
                # Ce truc va planter s'il n'y a pas encore de champ massif
                massif_base = infomassifs().massifposte(all_lists['id'][k])
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
                metadataout.write('\t\t<azimut> ' + azimut_str + '</azimut>\n')
                metadataout.write('\t\t<mask> ' + angle_str + '</mask>\n')
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
        all_lists = make_dict_list(path_shapefile, Id_station, Nom_station, Nom_alt, Nom_asp, Nom_slop, path_MNT_alt, path_MNT_asp, path_MNT_slop)
        create_NetCDF(all_lists, output_name)
        create_skyline(all_lists, path_MNT_alt)

if __name__ == '__main__':
    main()

