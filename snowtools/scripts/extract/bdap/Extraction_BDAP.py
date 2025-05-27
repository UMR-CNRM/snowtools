#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Auteur: Matthieu Vernay
# Date : 20/03/2024

import os
import sys
import argparse
from bronx.stdtypes.date import Date, Period
import numpy as np
import xarray as xr

"""
Outil d'extraction de données de la BDAP. Les données sont extraites sous forme de multiples fichiers grib (un
par date et échéance) et converties en un unique fichier netcdf par xarray.

Documentation :

- Contenu BDAP :
http://confluence.meteo.fr/pages/viewpage.action?pageId=397740883#Documentationsrelatives%C3%A0laBDAP-Docutilisateurs
- Pour Alpha   :
http://confluence.meteo.fr/pages/viewpage.action?pageId=397740883&preview=/397740883/437632340/BDAP_PPROD.pdf
- extraction   :
http://confluence.meteo.fr/pages/viewpage.action?pageId=397740883&preview=/397740883/397740896/DOC_UTILISATEURS_BDAP_EXTR.pdf
  --> ce script utilise la commande dap3_dev

Exemples :
----------

1. Extraction des ISO_TPW 0°C 1°C de l'analyse AROME entre le 2017073106 et le 2018080106 sur le domaine
   des GrandesRousses (domain par défaut, coordonnées dans le dictionnaire 'coords').
   La grille EURW1S40 est displonible depuis le 05/12/2017, avant cette date il faut extraire la grille
   FRANGP0025.
   Le parametre ISO_WETBT s'appelle ISO_TPW avant le 01/07/2019

>>> p Extraction_BDAP.py -b 2017073106 -e 2018080106 -t 0 -v ISO_TPW -l 27315 27415 -g FRANGP0025 -m PAAROME

    La même chose en archivant le fichier netcdf produit avec Vortex:

>>> p Extraction_BDAP.py -b 2017073106 -e 2018080106 -t 0 -v ISO_TPW -l 27315 27415 -g FRANGP0025 -m PAAROME -a

    ==> Durée d'extraction : environ 1h


2. Extraction de la WETBT de l'analyse AROME (TPW avant 01/07/2019) sur tous les niveaux hauteur disponibles et sur
   la même période/grille que l'exemple 1 :

>>> p Extraction_BDAP.py -b 2017073106 -e 2018080106 -t 0 -v HAUTEUR -p TPW -g FRANGP0025 -m PAAROME

    ==> Durée d'extraction : environ 26h

3. Pour extraire l'altitude du sol de la grille FRANGP0025 :

>>> p Extraction_BDAP.py -b 2017073106 -t 0 -v SOL -g FRANGP0025 -m PAAROME -l SOL

4 Extraction des précipitations horaires ANTILOPE sur les Alpes :

>>> p ~/snowtools/snowtools/scripts/extract/bdap/Extraction_BDAP.py -b 2025052607 -e 2025052706 -d alp -p PRECIP
-m ANTILOPEH -t 1 -g FRANXL1S100 -v SOL

"""

# Liste des coordonnées attendues par la commande dap3: lat_max, lat_min, lon_max, lon_min
# TODO : assurer que les points de grilles du sous domaines (coordonnées + pas lat/lon) sont
# bien confondus aves les points de la grille native pour éviter une interpolation
coords = dict(
    alp = ['46800', '43700', '5000', '7600'],
    pyr = ['43500', '42000', '-2000', '3500'],
    cor = ['43000', '41000', '8000', '11500'],
    GrandesRousses = ['45640', '44590', '5610', '7100'],  # includes 0.2° margin
)

known_grids = dict(
    FRANXL0025  = dict(latmax=51.5, lonmin=-6, maille=0.25),
    FRANGP0025  = dict(latmax=53, lonmin=-8, maille=0.25),
    FRANXL1S100 = dict(latmax=51.5, lonmin=-6, maille=0.01),
    EUROC25     = dict(latmax=61, lonmin=-15, maille=0.25),
    GLOB025     = dict(latmax=90, lonmin=0, maille=0.25),
    EURW1S10    = dict(latmax=55.4, lonmin=-12, maille=0.1),
    EURW1S40    = dict(latmax=55.4, lonmin=-12, maille=0.025),
    EURW1S100   = dict(latmax=55.4, lonmin=-12, maille=0.01),
    EURAT01     = dict(latmax=72, lonmin=-32, maille=0.1),
    EURAT1S20   = dict(latmax=72, lonmin=-32, maille=0.05),
)


model_map = dict(
    PAAROME   = dict(vapp='arome', vconf='3dvarfr'),  # Analyse AROME
    PAROME    = dict(vapp='arome', vconf='3dvarfr'),  # prevision AROME
    PAA       = dict(vapp='arpege', vconf='4dvarfr'),  # Analyse ARPEGE
    PA        = dict(vapp='arpege', vconf='4dvarfr'),  # Analyse ARPEGE
    ANTILOPEH = dict(vapp=None, vconf=None),
)

default_levels = dict(
    ISOBARE   = [100, 125, 150, 175, 200, 225, 250, 275, 300, 350, 400, 450, 500, 550, 600, 650, 700, 750, 800, 850,
                 900, 925, 950, 1000],
    HAUTEUR   = [2, 10, 20, 35, 50, 75, 100, 150, 200, 250, 375, 500, 625, 750, 875, 1000, 1125, 1250, 1375, 1500,
                 1750, 2000, 2250, 2500, 2750, 3000],
    ISO_T     = [25315, 26115, 26315, 27315],
    ISO_TPW   = [27315, 27415],
    ISO_WETBT = [27315, 27415],
    SOL       = ['SOL']
)


def parse_command_line():
    description = "BDAP extraction of NWP (AS-PEAROME) model data."
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument('-b', '--datebegin', help='Begining date of extraction, format YYYYMMDDHH or YYMMDDHH',
                        required=True)
    parser.add_argument('-e', '--dateend', help = 'Final date of extraction (default=datebegin)', default=None)
    parser.add_argument('-d', '--domain', help='Domain of the file', choices=coords.keys(), required=True)
    # WARNING : the 'parameters' and 'level-type' arguments depend on each other.
    # TODO : find a way (a very complex dict ?) ton ensure consistency
    parser.add_argument('-p', '--parameter', help='Parameter to extract', required=True,
                        choices=['PRECIP', 'WETBT', 'ALTITUDE', 'TPW'])
    parser.add_argument('-l', '--levels', nargs='+', help='Level(s) of the parameter to extract', default=None)
    parser.add_argument('-v', '--level_type', help='Type of level of the parameter to extract', required=True,
                        choices=['ISO_WETBT', 'SOL', 'HAUTEUR', 'ISOBARE', 'ISO_T', 'ISO_TPW'])
    parser.add_argument('-w', '--workdir', help="Runing directory (soprano's default)",)
    parser.add_argument('-m', '--model', help='NWP model from which the data must be extracted', required=True,
                        choices=model_map.keys())
    # TODO : documenter les périodes de disponibilités modèle/grille
    parser.add_argument('-g', '--grid', help='BDAP grid name from which to extract data', required=True,
                        choices=known_grids.keys())
    parser.add_argument('-t', '--echeances', help='Echeances à extraire (integer or "first:last" format)', default=1)
    parser.add_argument('-H', '--pdt', help="Pas de temps du réseau d'extraction", type=int, default=None)
    parser.add_argument('-a', '--archive', help="Archive output file with Vortex", action="store_true", default=False)
    parser.add_argument('--interpolation', action='store_true', help='Extraction on the user-defined subdomain'
                        '(slower)', default=False)
    parser.add_argument('--dlat', help='Traget latitude step (1/1000 of °) in case *interpolation*=True', default=None,
                        required='--interpolation' in sys.argv)
    parser.add_argument('--dlon', help='Traget longitude step (1/1000 of °) in case *interpolation*=True', default=None,
                        required='--interpolation' in sys.argv)

    args = parser.parse_args()
    args.datebegin  = Date(args.datebegin)
    if args.dateend is not None:
        args.dateend = Date(args.dateend)
    else:
        args.dateend = args.datebegin

    if args.level_type == 'SOL' and args.levels is None:
        args.levels = ['SOL']

    return args


def goto(path):
    if not os.path.exists(path):
        os.makedirs(path)
    os.chdir(path)


def clean():
    import glob
    for fic in glob.glob('*.idx') + glob.glob('*.grib'):
        os.remove(fic)


class ExtractBDAP(object):

    def __init__(self, model, date, ech, parameter, level_type, levels, grid, domain, dlat=None, dlon=None):

        self.model          = model
        self.date           = date
        self.ech            = ech
        self.parameter      = parameter
        self.level_type     = level_type
        self.levels         = levels
        self.grid           = grid.upper()
        self.domain         = domain
        self.gribname       = f'{level_type}_{date.ymdh}_{ech}.grib'
        self.dlat           = dlat
        self.dlon           = dlon

    def requete(self):
        self.rqst = 'requete.tmp'
        f = open(self.rqst, "w")
        f.write('#RQST\n')
        f.write(f'#NFIC {self.gribname}\n')  # Output file name
        f.write(f'#MOD {self.model}\n')  # BDAP model name (see doc)
        f.write(f'#PARAM {self.parameter}\n')  # paramater
        f.write(f'#Z_REF {self.grid}\n')  # BDAP grib name (see doc)
        if args.interpolation:
            f.write("#Z_EXTR INTERPOLATION\n")  # Interpolation on a user-defined grid
            f.write('#Z_GEO ' + ' '.join(coords[self.domain]) + '\n')  # Coordonées de la grille de sortie (N S WA E)
            # f.write('#Z_STP ' + ' '.join(dl[self.grid]) + '\n')  # Pas en lat/lon de la grille de sortie
            f.write(f'#Z_STP {self.dlat} {self.dlon} \n')  # Pas en lat/lon de la grille de sortie (TODO : check order)
        else:
            f.write("#Z_EXTR SOUS_GRILLE\n")
            f.write(f'#Z_GEO {self.j1} {self.j2} {self.i1} {self.i2} \n')
            f.write('#Z_STP 1 1\n')
        f.write('#L_LST ' + ' '.join(self.levels) + '\n')  # List de niveaux
        f.write(f'#L_TYP {self.level_type}\n')  # Type de niveau (SOL, ISOBARE, HAUTEUR, MER, ISO*,...)

    def extract_from_bdap(self, cmd='dap3_dev'):
        """
        Utiliser la commande 'dap3_dev' pour les données archivées (>5jours ?)
        """
        self.requete()
        os.system(f"{cmd} {self.ech} {self.rqst}".format(cmd, self.ech, self.rqst))  # Pour 'dap3_dev'
        # os.system(f"{cmd} {self.rqst}")  # Pour 'dap3_dev_echeance'
        if os.path.exists(self.gribname):
            if os.path.getsize(self.gribname) > 0:
                return True
            else:
                print('Removing empty file {0:s}'.format(self.gribname))
                os.remove(self.gribname)
        else:
            print(f'File {self.gribname} could not be extracted')
        return False

    def run(self):
        if os.path.exists(self.gribname):
            if os.path.getsize(self.gribname) > 0:
                return self.gribname
            else:
                print('Removing empty file {0:s}'.format(self.gribname))
                os.remove(self.gribname)

        os.environ["DMT_DATE_PIVOT"] = self.date.ymdhms
        self.get_subgrid()
        result = self.extract_from_bdap()

        if result:
            return self.gribname
        else:
            return None

    def get_subgrid(self):
        # Get SOUS GRILLE coordinates for dap3 command :
        i0 = known_grids[args.grid]['lonmin']
        j0 = known_grids[args.grid]['latmax']
        maille = known_grids[args.grid]['maille']
        latmax = int(coords[domain][0]) / 1000
        latmin = int(coords[domain][1]) / 1000
        lonmin = int(coords[domain][2]) / 1000
        lonmax = int(coords[domain][3]) / 1000
        self.j1 = int(1 + np.round(j0 - latmin, 2) / maille)
        self.j2 = int(1 + np.round(j0 - latmax, 2) / maille)
        self.i1 = int(1 + np.round(lonmin - i0, 2) / maille)
        self.i2 = int(1 + np.round(lonmax - i0, 2) / maille)


if __name__ == "__main__":
    args = parse_command_line()
    datebegin  = args.datebegin
    dateend    = args.dateend
    parameter  = args.parameter
    level_type = args.level_type
    model      = args.model
    grid       = args.grid
    domain     = args.domain
    parameter  = args.parameter
    if args.levels is None:
        levels = [str(lvl) for lvl in default_levels[level_type]]
    else:
        levels = args.levels
    if ':' in args.echeances:
        first, last = args.echeances.split(':')
        echeances = range(int(first), int(last) + 1)
    else:
        echeances  = [int(args.echeances)]
    if args.pdt is None:
        # The default beahvior to try to get hourly outputs
        # --> extract all N=H1-H0 echeances from H0 execution to cover H0 --> H1
        dt = max(echeances)

    if args.workdir is None:
        workdir = os.path.join(os.environ['HOME'], 'workdir', f'extraction_{args.model.upper()}', domain, parameter)
    else:
        workdir = args.workdir
    print(workdir)
    goto(workdir)

    if dateend != datebegin:  # The extraction covers a period
        outname = f'{model}_{parameter}_{level_type}_{grid}_{datebegin.ymdh}_{dateend.ymdh}.nc'
    else:  # The extraction is associated to a date
        outname = f'{model}_{parameter}_{level_type}_{grid}_{datebegin.ymdh}.nc'
    if not os.path.exists(outname):
        extractedfiles = list()
        date = datebegin
        # Loop over dates and lead times
        while date <= dateend:
            for ech in echeances:
                # Launch extraction
                extractor = ExtractBDAP(model, date, ech, parameter, level_type, levels, grid, domain,
                        dlat=args.dlat, dlon=args.dlon)
                grib = extractor.run()
                if grib is not None:
                    extractedfiles.append(grib)
            date = date + Period(hours=max(dt, 1))  # Avoid infinite loops

        # Open all extracted files with xarray
        print('Opening grib files with xarray...')
        ds  = xr.open_mfdataset(extractedfiles, concat_dim='valid_time', combine='nested', engine='cfgrib')
        # TODO : set proper variable names, check/set attributes,...
        print('Dataset created')
        ds.to_netcdf(outname)

    clean()

    ############################################
    # Everything beyond this point is optional #
    ############################################

    if args.archive:
        import vortex
        from snowtools.scripts.extract.vortex import vortexIO as io

        t = vortex.ticket()

        if level_type.startswith('ISO_'):
            kind = level_type
        elif level_type == 'HAUTEUR':
            kind = parameter
        else:
            print('Kind not yet defined for this extraxction')
            raise NotImplementedError
        out = io.put_meteo(
            kind           = kind,
            vapp           = 'edelweiss',
            vconf          = domain,
            source_app     = model_map[model]['vapp'],
            source_conf    = model_map[model]['vconf'],
            geometry       = grid,
            experiment     = f'oper@{os.environ["USER"]}',
            datebegin      = args.datebegin,
            dateend        = args.dateend,
            filename       = outname,
        )
        out[0].quickview()
        print(t.prompt, 'Output location =', out[0].location())
        print()
