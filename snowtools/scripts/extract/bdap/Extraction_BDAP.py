#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Auteur: Matthieu Vernay
# Date : 20/03/2024

import os
import sys
import argparse
import numpy as np
import xarray as xr
import time

from vortex import toolbox

from bronx.stdtypes.date import Date, Period

"""
Outil d'extraction de données de la BDAP. Les données sont extraites sous forme de fichiers grib
et converties en un unique fichier netcdf par xarray.

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
   des GrandesRousses (domain par défaut, coordonnées dans le dictionnaire 'known_domains').
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

>>> p ~/snowtools/snowtools/scripts/extract/bdap/Extraction_BDAP.py -b 2025052607 -e 2025052706 --domain alp -p PRECIP
-m ANTILOPEH -t 1 -g FRANXL1S100 -v SOL

"""


# Liste des coordonnées attendues par la commande dap3: lat_max, lat_min, lon_max, lon_min
# TODO : assurer que les points de grilles du sous domaines (coordonnées + pas lat/lon) sont
# bien confondus aves les points de la grille native pour éviter une interpolation
known_domains = dict(
    alp = ['46900', '43000', '4500', '8000'],
    pyr = ['43500', '42000', '-2000', '3500'],
    cor = ['43000', '41000', '8000', '10500'],
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
    PAAROME      = dict(vapp='arome', vconf='3dvarfr', block='meteo'),  # Analyse AROME
    PAROME       = dict(vapp='arome', vconf='3dvarfr', block='meteo'),  # Prévision AROME
    PEAROME      = dict(vapp='arome', vconf='pearome', block='meteo'),  # Prévision d'ensemble AROME
    PAA          = dict(vapp='arpege', vconf='4dvarfr', block='meteo'),  # Analyse ARPEGE
    PA           = dict(vapp='arpege', vconf='4dvarfr', block='meteo'),  # Analyse ARPEGE
    ANTILOPE     = dict(vapp='antilope', vconf='[geometry:area]', block='raw'),
    ANTILOPEJP1  = dict(vapp='antilope', vconf='[geometry:area]', block='rawJP1'),
    ANTILOPEH    = dict(vapp='antilope', vconf='[geometry:area]', block='Hourly'),
    ANTILOPEJP1H = dict(vapp='antilope', vconf='[geometry:area]', block='HourlyJP1'),
    ANTILOPEQ    = dict(vapp='antilope', vconf='[geometry:area]', block='Daily'),
    ANTILOPEJP1Q = dict(vapp='antilope', vconf='[geometry:area]', block='DailyJP1'),
)

default_levels = dict(
    ISOBARE   = [100, 125, 150, 175, 200, 225, 250, 275, 300, 350, 400, 450, 500, 550, 600, 650, 700, 750, 800, 850,
                 900, 925, 950, 1000],
    HAUTEUR   = [2, 10, 20, 35, 50, 75, 100, 150, 200, 250, 375, 500, 625, 750, 875, 1000, 1125, 1250, 1375, 1500,
                 1750, 2000, 2250, 2500, 2750, 3000],
    ISO_T     = [25315, 26115, 26315, 27315],
    ISO_TPW   = [27315, 27415],
    ISO_WETBT = [27315, 27415],
    ISO_TP    = [1500, 2000],
    SOL       = ['SOL'],
    MER       = ['MER'],
    TOP       = ['TOP'],
)

valid_geometries = dict(
    PAAROME      = ['EURW1S40', 'EURW1S100'],
    PAROME       = ['EURW1S40', 'EURW1S100'],
    PEAROME      = ['EURW1S40'],
    ASPEAROME    = ['FRANXL0025'],
    PA           = ['ATOURX01', 'EURAT01', 'GLOB025', 'EURAT1S20', 'GLOB01'],
    PAA          = ['ATOURX01', 'EURAT01', 'GLOB025', 'EURAT1S20', 'GLOB01'],
    ANTILOPE     = ['FRANXL1S100'],
    ANTILOPEJP1  = ['FRANXL1S100'],
    ANTILOPEH    = ['FRANXL1S100'],
    ANTILOPEJP1H = ['FRANXL1S100'],
    ANTILOPEQ    = ['FRANXL1S100'],
    ANTILOPEJP1Q = ['FRANXL1S100'],
)


valid_run_times = dict(
    PAROME = dict(
        hour   = range(0, 24, 3),
        minute = [0],
    ),
    PAAROME = dict(
        hour   = range(0, 24, 1),
        minute = [0],
    ),
    PEAROME = dict(
        hour   = range(3, 24, 6),
        minute = [0],
    ),
    ASPEAROME = dict(
        hour   = [9, 21],
        minute = [0],
    ),
    PAA = dict(
        hour   = range(0, 24, 6),
        minute = [0],
    ),
    PA = dict(
        hour   = range(0, 24, 6),
        minute = [0],
    ),
    ANTILOPE = dict(
        hour   = range(0, 24, 1),
        minute = [5, 15, 60],
    ),
    ANTILOPEJP1 = dict(
        hour   = range(0, 24, 1),
        minute = [5, 15, 60],
    ),
    ANTILOPEH = dict(
        hour   = range(0, 24, 1),
        minute = [0],
    ),
    ANTILOPEJP1H = dict(
        hour   = range(0, 24, 1),
        minute = [0],
    ),
    ANTILOPEQ = dict(
        hour   = [0, 6],
        minute = [0],
    ),
    ANTILOPEJP1Q = dict(
        hour   = [0, 6],
        minute = [0],
    ),

)

valid_lead_times = dict(
    PAAROME = {
        geometry: {str(key): [0, 1] for key in range(24)}
        for geometry in valid_geometries['PAAROME']
    },
    PAROME = {
        geometry: {
            '0': range(0, 49),
            '3': range(0, 46),
            '6': range(0, 43),
            '9': range(0, 8),
            '12': range(0, 49),
            '15': range(0, 8),
            '18': range(0, 43),
            '21': range(0, 8),
        } for geometry in valid_geometries['PAROME']
    },
    PEAROME = {
        geometry: {
            '3': range(1, 46),
            '9': range(1, 52),
            '15': range(1, 46),
            '21': range(1, 52),
        } for geometry in valid_geometries['PEAROME']
    },
    ASPEAROME = {
        geometry: {
            '9': range(1, 46),
            '21': range(1, 46),
        } for geometry in valid_geometries['ASPEAROME']
    },
    PA = dict(
        ATOURX01 = {
            '0': range(0, 105, 3),
            '6': range(0, 75, 3),
            '12': range(0, 117, 3),
            '18': range(0, 63, 3),
        },
        GLOB025 = {
            '0': range(0, 105, 3),
            '6': range(0, 75, 3),
            '12': range(0, 117, 3),
            '18': range(0, 63, 3),
        },
        EURAT01 = {
            '0': range(0, 103, 1),
            '6': range(0, 73, 1),
            '12': range(0, 115, 1),
            '18': range(0, 61, 1),
        },
        GLOB01 = {
            '0': range(0, 103, 1),
            '6': range(0, 73, 1),
            '12': range(0, 115, 1),
            '18': range(0, 61, 1),
        },
        EURAT1S20 = {
            '0': range(0, 103, 1),
            '6': range(0, 73, 1),
            '12': range(0, 115, 1),
            '18': range(0, 61, 1),
        },
    ),
    PAA = dict(
        ATOURX01  = {str(key): [0, 6] for key in ['0', '6', '12', '18']},
        GLOB025   = {str(key): [0, 6] for key in ['0', '6', '12', '18']},
        EURAT01   = {str(key): range(0, 7) for key in ['0', '6', '12', '18']},
        GLOB01    = {str(key): range(0, 7) for key in ['0', '6', '12', '18']},
        EURAT1S20 = {str(key): range(0, 7) for key in ['0', '6', '12', '18']},
    ),
    ANTILOPE = dict(
        FRANXL1S100 = {str(key): [1, 5, 15] for key in range(0, 24)},
    ),
    ANTILOPEJP1 = dict(
        FRANXL1S100 = {str(key): [1, 5, 15] for key in range(0, 24)},
    ),
    ANTILOPEH = dict(
        FRANXL1S100 = {str(key): [1] for key in range(0, 24)},
    ),
    ANTILOPEJP1H = dict(
        FRANXL1S100 = {str(key): [1] for key in range(0, 24)},
    ),
    ANTILOPEQ = dict(
        FRANXL1S100 = {'0': 0, '6': 24},
    ),
    ANTILOPEJP1Q = dict(
        FRANXL1S100 = {'0': 0, '6': 24},
    ),

)


def parse_command_line():
    description = "BDAP extraction of NWP (AS-PEAROME) model data."
    parser = argparse.ArgumentParser(description=description)

    parser.add_argument('-b', '--datebegin', required=True,
                        help='Begining date of extraction, format YYYYMMDDHH or YYMMDDHH')

    parser.add_argument('-e', '--dateend', default=None,
                        help = 'Final date of extraction (default=datebegin)')

    parser.add_argument('-m', '--model', help='NWP model from which the data must be extracted', required=True,
                        choices=model_map.keys())

    # WARNING : the 'parameters' and 'level-type' arguments depend on each other.
    # TODO : find a way (a very complex dict ?) ton ensure consistency
    parser.add_argument('-p', '--parameter', help='Parameter to extract', required=True,
                        choices=['PRECIP', 'WETBT', 'ALTITUDE', 'TPW', 'THETAPW'])

    parser.add_argument('-v', '--level_type', required=True,
                        choices=['ISO_WETBT', 'SOL', 'HAUTEUR', 'ISOBARE', 'ISO_T', 'ISO_TPW'],
                        help='Type of level of the parameter to extract')

    # TODO : documenter les périodes de disponibilités modèle/grille
    parser.add_argument('-g', '--grid', required=True, choices=known_grids.keys(),
                        help='BDAP grid name from which to extract data')

    parser.add_argument('-d', '--domain', type=str, required=True,
                        help='Extraction domain')

    parser.add_argument('-c', '--coordinates', nargs=4, type=float, default=None,
                        help='list of min/max coordinates to extract, format:'
                        '[latmax, latmin, lonmin, lonmax] (unit: °)')

    parser.add_argument('-l', '--levels', nargs='+', default=None,
                        help='Level(s) of the parameter to extract')

    parser.add_argument('-w', '--workdir', help="Runing directory (soprano's default)",)

    parser.add_argument('-t', '--echeances', default='1', type=str,
                        help='Lead times to be extracted for each run time (integer or "first:last" format)')

    parser.add_argument('-H', '--pdt', type=int, default=None,
                        help="Run time step")

    parser.add_argument('-a', '--archive', action="store_true", default=False,
                        help="Archive output file with Vortex")

    parser.add_argument('-k', '--kind',
                        help="Kind of the output resource in case it is archived with Vortex")

    parser.add_argument('--interpolation', action='store_true', default=False,
                        help='Extraction on the user-defined subdomain (slower)')

    parser.add_argument('--dlat', default=None, required='--interpolation' in sys.argv,
                        help='Traget latitude step (1/1000 of °) in case *interpolation*=True')

    parser.add_argument('--dlon', default=None, required='--interpolation' in sys.argv,
                        help='Traget longitude step (1/1000 of °) in case *interpolation*=True')

    parser.add_argument('--test', action='store_true', default=False,
                        help='Switch to test mode : write the request but do not try to execute it')

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


def speedtest(function):
    def wrapper(*args, **kw):
        t0 = time.time()
        result = function(*args, **kw)
        t1 = time.time()
        monitoring = f'monitoring_{model}_{parameter}_{level_type}_{grid}_{datebegin.ymdh}_{dateend.ymdh}.txt'
        extraction_time = (t1 - t0) / 60
        with open(monitoring, 'w') as f:
            f.write(f"Extraction time : {extraction_time} minutes \n")
        return result
    return wrapper


class GeometryError(Exception):
    """
    Exception when the required extraction grid does not exists for the given model

    :param model: specified model
    :type model: str
    :param grid: specified grid
    :type grid: str
    """
    def __init__(self, model, grid):
        self.model = model
        self.grid = grid

    def __str__(self):
        return f"The grid {self.grid} does not exists for model {self.model}. \n"\
               "Valid grids for this model are : " + " ".join(valid_geometries[self.model])


class RunTimeError(Exception):
    """
    Exception when the required run time does not exist for the given model

    :param model: specified model
    :type model: str
    :param date: specified run time
    :type date: Date
    """
    def __init__(self, model, date):
        self.model = model
        self.date = date

    def __str__(self):
        return f"Run time {self.date.hh}:{self.date.minute} does not exists for model {self.model}. \n" \
               "Valid run times for this model are: \n" \
               "- Hour in " + ",".join([str(x) for x in valid_run_times[self.model]['hour']]) + "\n" \
               "- Minute in " + " ".join([str(x) for x in valid_run_times[self.model]['minute']])


class LeadTimeError(Exception):
    """
    Exception when the required lead time does not exist for the given model, run time and geometry

    :param model: specified model
    :type model: str
    :param date: specified run time
    :type date: Date
    :param geometry: specified geometry
    :type geometry: str
    :param ech: specified lead time
    :type ech: str
    """
    def __init__(self, model, date, geometry, ech):
        self.model = model
        self.date = date
        self.geomerty = geometry
        self.ech = ech

    def __str__(self):
        return f"Lead time {self.ech} does not exists for model {self.model}, grid {self.geometry}\n" \
               f"and run time {self.date}.\n" \
               "Valid lead times for this configuration are:\n" \
               ",".join(valid_run_times[self.model][self.geometry][self.date.hour])


class ExtractBDAP(object):

    def __init__(self, model: str, date: str or list, ech: str or list, parameter: str, level_type: str,
            levels: list, grid: str, coordinates: tuple, dlat=None, dlon=None, interpolation=False, test=False,
            member=None):

        self.model          = model
        self.date           = date
        self.ech            = ech
        self.parameter      = parameter
        self.level_type     = level_type
        self.levels         = levels
        self.grid           = grid.upper()
        self.coordinates    = coordinates
        if member is None:
            self.gribname       = f'{level_type}_{date.ymdh}_{ech}.grib'
        else:
            self.gribname       = f'{level_type}_{date.ymdh}_{ech}_mb{member:03d}.grib'
        self.dlat           = dlat
        self.dlon           = dlon
        self.test           = test
        self.interpolation  = interpolation
        if self.model == 'PEAROME':
            self.MOD = f'PEAROME{member:03d}'
        elif self.model == 'ASPEAROME':
            self.MOD = f'PG1PEAROM{member:03d}'
        else:
            self.MOD = self.model

    def requete(self, cmd='dap3_dev'):
        """
        Write dap3 request file
        Documentaiton :
        http://confluence.meteo.fr/pages/viewpage.action?pageId=397740883&preview=/397740883/397740896/DOC_UTILISATEURS_BDAP_EXTR.pdf

        :param cmd: dap3 command (values : dap3_dev, dap3_dev_echeance, dap3_dev_date_ech)
        :type cmd: str
        """
        self.rqst = 'requete.tmp'
        f = open(self.rqst, "w")
        f.write('#RQST\n')
        f.write(f'#NFIC {self.gribname}\n')  # Output file name
        f.write(f'#MOD {self.MOD}\n')  # BDAP model name (see doc)
        f.write(f'#PARAM {self.parameter}\n')  # paramater
        f.write(f'#Z_REF {self.grid}\n')  # BDAP grib name (see doc)
        if self.interpolation:
            f.write("#Z_EXTR INTERPOLATION\n")  # Interpolation on a user-defined grid
            # Coordonées de la grille de sortie (N S WA E) :
            f.write('#Z_GEO ' + ' '.join(self.coordinates) + '\n')
            # f.write('#Z_STP ' + ' '.join(dl[self.grid]) + '\n')  # Pas en lat/lon de la grille de sortie
            f.write(f'#Z_STP {self.dlat} {self.dlon} \n')  # Pas en lat/lon de la grille de sortie (TODO : check order)
        else:
            f.write("#Z_EXTR SOUS_GRILLE\n")
            f.write(f'#Z_GEO {self.j1} {self.j2} {self.i1} {self.i2}\n')
            f.write('#Z_STP 1 1\n')
        # Use a list of dates / ech
        # WARNING : there is a limit on the number of fields that can be extracted with one command.
        # The BDAP documentation recommends to rather loop over dates and lead times ("ech")
        if cmd in ['dap3_dev_echeance', 'dap3_dev_date_ech']:
            f.write("#T_LST " + " ".join(self.ech) + " \n")  # self.ech = list of ech (int)
        if cmd == 'dap3_dev_date_ech':
            f.write("#D_LST " + " ".join(self.date) + " \n")  # self.date = list of dates (fmt YYYYMMDDHHmmss)

        f.write('#L_LST ' + ' '.join(self.levels) + '\n')  # List de niveaux
        f.write(f'#L_TYP {self.level_type}\n')  # Type de niveau (SOL, ISOBARE, HAUTEUR, MER, ISO*,...)
        f.write('#FORM GRIB2_C_MAX\n')  # Format de sortie

    def extract_from_bdap(self, cmd='dap3_dev'):
        """
        Prepare extraction environement (check arguments, export date_pivot,...)
        and return extraction result

        NB :
        ----
        Utiliser la commande 'dap3_dev' pour les données archivées (>5jours ?)
        Utilisation déconseilée de dap3_dev_date_ech et dap3_dev_echance

        :param cmd: dap3 command (values : dap3_dev, dap3_dev_echeance, dap3_dev_date_ech)
        :type cmd: str
        """
        self.check_request()
        self.requete(cmd=cmd)
        if self.test:
            return False
        else:
            if cmd in ['dap3_dev', 'dap3_dev_echeance']:
                os.environ["DMT_DATE_PIVOT"] = self.date.ymdhms
            if cmd == 'dap3_dev':
                os.system(f"{cmd} {self.ech} {self.rqst}")  # Pour 'dap3_dev'
            elif cmd in ['dap3_dev_date_ech', 'dap3_dev_echeance']:
                os.system(f"{cmd} {self.rqst}")

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

    def check_request(self):
        """
        Check arguments consistency
        """
        self.check_geometry()
        self.check_date()
        self.check_ech()

    def check_geometry(self):
        """
        Check geometry consistency
        """
        if self.grid not in valid_geometries[self.model]:
            raise GeometryError(self.model, self.grid)

    def check_date(self):
        """
        Check run time consistency
        """

        if isinstance(self.date, list):
            # TODO
            pass
        else:
            if ((self.date.hour not in valid_run_times[self.model]['hour']) or
            (self.date.minute not in valid_run_times[self.model]['minute'])):
                raise RunTimeError(self.model, self.date)

    def check_ech(self):
        """
        Check lead time consistency
        """

        if isinstance(self.date, list):
            pass
        else:
            if not int(self.ech) in valid_lead_times[self.model][self.grid][self.date.h]:
                raise LeadTimeError(self.model, self.date, self.grid, self.ech)

    def run(self):
        """
        Main method to be called on an ExtractBDAP object
        """
        if os.path.exists(self.gribname):
            if os.path.getsize(self.gribname) > 0:
                return self.gribname
            else:
                print('Removing empty file {0:s}'.format(self.gribname))
                os.remove(self.gribname)

        self.get_subgrid()
        result = self.extract_from_bdap()

        if result:
            return self.gribname
        else:
            return None

    def get_subgrid(self):
        """
        Compute sub_grid indices from min/max coordinates of the extraction domain

        Documentation:
        http://confluence.meteo.fr/pages/viewpage.action?pageId=397740883&preview=/397740883/397740896/DOC_UTILISATEURS_BDAP_EXTR.pdf
        """
        # Get SOUS GRILLE coordinates for dap3 command :
        i0 = known_grids[self.grid]['lonmin']
        j0 = known_grids[self.grid]['latmax']
        maille = known_grids[self.grid]['maille']
#        latmax = int(known_domains[self.domain][0]) / 1000
#        latmin = int(known_domains[self.domain][1]) / 1000
#        lonmin = int(known_domains[self.domain][2]) / 1000
#        lonmax = int(known_domains[self.domain][3]) / 1000
        latmax = self.coordinates[0] / 1000
        latmin = self.coordinates[1] / 1000
        lonmin = self.coordinates[2] / 1000
        lonmax = self.coordinates[3] / 1000
        self.j1 = int(1 + np.round(j0 - latmin, 2) / maille)
        self.j2 = int(1 + np.round(j0 - latmax, 2) / maille)
        self.i1 = int(1 + np.round(lonmin - i0, 2) / maille)
        self.i2 = int(1 + np.round(lonmax - i0, 2) / maille)


@speedtest
def execute():
    extractedfiles = list()
    date = datebegin
    # Loop over dates and lead times
    while date <= dateend:
        for ech in echeances:
            # Launch extraction
            if model in ['PEAROME', 'ASPEAROME']:
                for mb in range(1, 17):
                    extraction = ExtractBDAP(model, date, ech, parameter, level_type, levels, grid, coordinates,
                            dlat=args.dlat, dlon=args.dlon, interpolation=args.interpolation, test=args.test, member=mb)
                    grib = extraction.run()
                    if grib is not None:
                        extractedfiles.append(grib)
            else:
                extraction = ExtractBDAP(model, date, ech, parameter, level_type, levels, grid, coordinates,
                        dlat=args.dlat, dlon=args.dlon, interpolation=args.interpolation, test=args.test)
                grib = extraction.run()
                if grib is not None:
                    extractedfiles.append(grib)
        date = date + Period(hours=max(dt, 1))  # Avoid infinite loops

    # Open all extracted files with xarray
    if len(extractedfiles) > 0:
        print('Opening grib files with xarray...')
        ds  = xr.open_mfdataset(extractedfiles, concat_dim='time', combine='nested', engine='cfgrib')

        # TODO : set proper variable names, check/set attributes,...
        # ds = ds.drop('time').rename({'valid_time': 'time'})
        ds = ds.assign_coords(longitude=ds.longitude.data.round(2), latitude=ds.latitude.round(2))

        print('Dataset created')
        ds.to_netcdf(outname)
    else:
        if args.test:
            print(f'Request created : {os.path.join(workdir, extraction.rqst)}')
        else:
            print('No extracted file to open')

    clean()


if __name__ == "__main__":
    args = parse_command_line()
    datebegin   = args.datebegin
    dateend     = args.dateend
    parameter   = args.parameter
    level_type  = args.level_type
    model       = args.model
    grid        = args.grid
    domain      = args.domain
    if args.coordinates is not None:
        coordinates = [int(x * 1000) for x in args.coordinates]
    else:
        if domain in known_domains.keys():
            coordinates = [int(x) for x in known_domains[domain]]
        else:
            raise KeyError(f'Domain {domain} unknown, please provide associated coordinates')
    parameter   = args.parameter
    kind       = args.kind
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
    else:
        dt = args.pdt

    if args.workdir is None:
        workdir = os.path.join(os.environ['HOME'], 'workdir', f'extraction_{model.upper()}', domain, parameter)
    else:
        workdir = args.workdir
    print("Workdir = ", workdir)
    goto(workdir)

    if dateend != datebegin:  # The extraction covers a period
        outname = f'{model}_{parameter}_{level_type}_{grid}_{datebegin.ymdh}_{dateend.ymdh}.nc'
    else:  # The extraction is associated to a date
        outname = f'{model}_{parameter}_{level_type}_{grid}_{datebegin.ymdh}.nc'
    if not os.path.exists(outname):
        execute()

    ############################################
    # Everything beyond this point is optional #
    ############################################

    if args.archive:
        import vortex
        from snowtools.scripts.extract.vortex import vortexIO as io

        t = vortex.ticket()

        if kind is None:
            if level_type.startswith('ISO_'):
                kind = level_type
            elif level_type == 'HAUTEUR':
                kind = parameter
            else:
                print('FOOTPRINT ERROR: Missing *kind* footprint (see -k / --king argument)')
                raise NotImplementedError

        # out = io.put_meteo(
        out = toolbox.output(
            kind           = kind,
            vapp           = model_map[model]['vapp'],
            vconf          = model_map[model]['vconf'],
            # source_app     = model_map[model]['vapp'],
            # source_conf    = model_map[model]['vconf'],
            geometry       = domain,
            experiment     = f'oper@{os.environ["USER"]}',
            datebegin      = args.datebegin,
            # TODO : avancer dateend de pdt (?) dans le cas de variables non instantallées (ex: précipitations)
            dateend        = args.dateend,
            date           = '[dateend]',
            filename       = outname,
            block          = model_map[model]['block'],
            namebuild      = 'flat@cen',
            namespace      = 'vortex.multi.fr',
        )
        out[0].quickview()
        print(t.prompt, 'Output location =', out[0].location())
        print()
