#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse
import os
import numpy as np
import pandas as pd
import shutil
import shapefile
from shapely.geometry import Point, Polygon

from snowtools.scripts.extract.obs.bdquery import question
from snowtools.DATA import SNOWTOOLS_CEN


departements = ["04", "05", "06", "26", "38", "73", "74", "09", "31", "64", "65", "66", "99", "20", "07", "11", "15", "30", "34", "42", "43", "48", "63", "81", "54", "57", "67", "68", "70", "88", "90", "01", "25", "39"]
departements_etrangers = ["204", "205", "203"]
carpost_fields = ["num_poste", "alti", "massif_nivo", "nom_usuel", "lat_dg", "lon_dg", "datouvr", "datferm", "exposition_nivo", "pente_nivo", "type_nivo"]


def extraction():

    listpostes = list()
    fields = ["num_poste"]
    table = "poste"

    # Liste "num_poste" stations françaises
    postes_france = question(
            listvar=[f"{table}.{field}" for field in fields],
            table=table,
            listjoin=[f"CATALOGUE_MESURE on {table}.num_poste=CATALOGUE_MESURE.num_poste"],
            # Selection des stations si :
            # - dans un département contenant des massifs de montagne
            # - passant des HTN
            # - altitude > 600m
            listconditions=["(num_dep in (" + ",".join(departements) + ") and  mesure_donnee = 1 and CATALOGUE_MESURE.parametre in ('H_NEIGETOT','Q_NEIGETOT06') and alti > 600)"],
            )
    out = postes_france.get()
    listpostes.extend(set([str(poste[0]) for poste in out]))

    # Liste "num_poste" stations étrangères
    postes_etranger = question(
            listvar=[f"{table}.{field}" for field in fields],
            table=table,
            listconditions=["(num_dep in (" + ",".join(departements_etrangers) + ") and mesure_donnee=1)"],
            )
    out = postes_etranger.get()
    listpostes.extend(set([str(poste[0]) for poste in out]))

    # Liste sondages
    postes_sondages = question(
        listvar=["num_poste"],
        table="poste_nivo",
        listconditions=["type_nivo=2", "num_dep in (" + ",".join(departements) + ")", "datferm is null"],
    )
    out = postes_sondages.get()
    listpostes.extend(set([str(poste[0]) for poste in out]))

    table = "poste_nivo"
    # METADONNEES stations "CARPOSTS"
    carposts = question(
            listvar=[f"{table}.{field}" for field in carpost_fields],
            table=table,
            listconditions=[f"{table}.num_poste in ({','.join(listpostes)})"],
            listorder=['num_poste'],
            )
    #carposts.get(outputfile=f'carposts.csv', sep=' ', header=True)
    df=pd.DataFrame(carposts.get(), columns=carpost_fields)   
    
    # Convertir 'datferm' en datetime, les erreurs deviennent NaT
    df["datferm"] = pd.to_datetime(df["datferm"], errors='coerce')
    # Convertir 'datouvr' en datetime, les erreurs deviennent NaT
    df["datouvr"] = pd.to_datetime(df["datouvr"], errors='coerce')

    # Remplacer les NaT par une date par défaut AVANT d'appliquer strftime
    df["datferm"] = df["datferm"].fillna(pd.to_datetime("2199-12-31"))
    df["datouvr"] = df["datouvr"].fillna(pd.to_datetime("2199-12-31"))

    # Formater la date en YYYY-MM-DD
    df["datferm"] = df["datferm"].dt.strftime('%Y-%m-%d')    
    df["datouvr"] = df["datouvr"].dt.strftime('%Y-%m-%d')    
    
    return df


def check_massif_number(df):

    # Sécurité : si le poste n'est associé à aucun massif, on cherche à lui associer le massif dans lequel il se trouve
    # WARNING le shapefile dans snowtools n'est pas dans la projection lat/lon des postes
    #shapefile_path = os.path.join(SNOWTOOLS_CEN, 'snowtools', 'DATA')
    from snowtools.DATA import SNOWTOOLS_DATA
    from osgeo import ogr, osr
    filename = 'massifs.shp'
    shp = shapefile.Reader(os.path.join(SNOWTOOLS_DATA, filename))
    
    
    # Définir les systèmes de référence
    source = osr.SpatialReference()
    source.ImportFromEPSG(4326)  # WGS84 (lat/lon)

    target = osr.SpatialReference()
    target.ImportFromEPSG(2154)  # Lambert 93
    
    transform = osr.CoordinateTransformation(source, target)
    
    
    def set_massif_number(row):
        if pd.isna(row["massif_nivo"]) or row["massif_nivo"] == 99:

            lon = row["lon_dg"]
            lat = row["lat_dg"]
            
            # Convertir les coordonnées de WGS84 à Lambert 93
            point = ogr.Geometry(ogr.wkbPoint)
            point.AddPoint(float(lat), float(lon))
            point.Transform(transform)
            x_lambert = point.GetX()
            y_lambert = point.GetY()
            
            for shape in shp.shapeRecords():
                massif_data = shape.record
                massif_number = int(massif_data[0])
                massif_coords = shape.shape.points
                poly = Polygon(massif_coords)
                if Point(x_lambert, y_lambert).within(poly):
                    print ('Poste ', row["nom_usuel"], 'Attributed massif=', massif_number)
                    return massif_number
            print ('Poste ', row["nom_usuel"], 'No massif found')
            return np.nan
        elif row["massif_nivo"] >= 100:
            # poste occasionnels que SAFRAN ne sait pas traiter
            return np.nan
        return row["massif_nivo"]


    # Créer une copie explicite du DataFrame pour éviter les SettingWithCopyWarning
    df = df.copy()

    # Appliquer la fonction à chaque ligne
    df.loc[:, "massif_nivo"] = df.apply(set_massif_number, axis=1)

    # Filtrer les lignes où massif_nivo est NaN et créer une copie explicite
    df = df.loc[df['massif_nivo'].notna()].copy()

    # Convertir explicitement chaque valeur en entier
    df["massif_nivo"] = df["massif_nivo"].apply(lambda x: int(x))

    return df


def convert_aspect(row):
    aspect = float(row["exposition_nivo"])
    if aspect <= 36 :
        #BDCLIM exposition en rose de 36
        aspect = aspect * 10.
    else:
        #BDCLIM : 96=crête, 97=fond de vallée, 98=plateau
        aspect = 0.
    return aspect

def make_carposts(df):

    col_fmt = dict(
        massif_nivo = "02d",
        nom_usuel = "",
        lat_dg = "9.6f",
        lon_dg = "9.6f",
        alti = "04d",
        pente_nivo = "02d",
        exposition_nivo = "#3.0f",
        num_poste = "09d",
    )

    # Convert columns to proper types
    df["lon_dg"] = df['lon_dg'].astype(float)
    df["lat_dg"] = df['lat_dg'].astype(float)
    df["pente_nivo"] = df['pente_nivo'].astype(int)
    df["nom_usuel"] = df["nom_usuel"].str[:22]
    df["num_poste"] = df['num_poste'].astype(int)

    fields = ["num_poste", "alti", "massif_nivo", "nom_usuel", "lat_dg", "lon_dg", "datferm"]
    massif_map = dict(
        alp = [*range(1, 28)],
        pyr = [*range(64, 76), *range(80, 92)],
        cor = [40, 41],
        mac = [*range(48, 55), *range(59, 63)],
        jur = [*range(55, 59)],
        vog = [*range(45, 48)],
    )
    rundir = os.getcwd()
    for dom in massif_map.keys():
        carpath = 'carpost_{0:s}'.format(dom)
        if os.path.exists(carpath):
            shutil.rmtree(carpath)
        os.mkdir(carpath)
        os.chdir(carpath)
        tmp = df[df.massif_nivo.isin(massif_map[dom])].reset_index(drop=True)[[key for key in col_fmt.keys()]]

        for key, fmt in col_fmt.items():
            tmp[key] = tmp[key].apply(lambda x : f"{x:{fmt}}")
        for index, row in tmp.iterrows():
            write_carpost(index, row)

        os.chdir(rundir)

def write_carpost(index, row):

    carname = "CARPOST{0:0=3d}".format(index)
    with open(carname, 'w') as c:
        c.write(','.join(row.iloc[:7].values.tolist() + ['1.00'] + ["00"]*36 + ['0', row.iloc[-1] + '\n']))


def main():

    # Extract metadata from BDClim
    df = extraction()
    # Check / fix  massif numbers
    df = check_massif_number(df)
    # Convert aspect unit
    df["exposition_nivo"] = df.apply(convert_aspect, axis=1)
    # Remplacer les guillemets et les espaces par des tirets dans la colonne "nom_usuel"
    df["nom_usuel"] = df["nom_usuel"].str.replace('"', '').str.replace(' ', '-')
    # Write information file on extracted carposts
    df.to_csv('carposts_info.csv', sep=' ', header=True, index=False)
    # Write carposts
    make_carposts(df)

if __name__ == "__main__":
    main()
