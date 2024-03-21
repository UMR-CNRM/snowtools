#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Auteur: Matthieu Vernay
# Date : 20/03/2024

import os
import argparse
from datetime import datetime, timedelta

"""
Outil d'extraction de données de la BDAP.

Exemples :
----------

1. Extraction de l'ISO_TPW de l'analyse ARPEGE entre le 2017073106 et le 2018080106 :
   La grille EURW1S40 est displonible depuis le 05/12/2017, avant cette date il faut extraire la grille
   FRANGP0025.
   Le parametre ISO_WETBT s'appelle ISO_TPW avant le 01/07/2019

>>> p Extraction_BDAP.py -b 2017073106 -e 2018080106 -t 0 -v ISO_TPW -l 27315 27415 -g FRANGP0025 -m PAAROME

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

# Pas en lat/lon de la grille cible en 1/1000 de °
dl = dict(
    FRANXL0025 = ['25', '25'],
    EUROC25    = ['25', '25'],
    GLOB025    = ['25', '25'],
    EURW1S40   = ['25', '25'],
    EURAT01    = ['10', '10'],
    EURW1S100  = ['10', '10'],
    EURAT1S20  = ['05', '05'],
)


def parse_command_line():
    description = "BDAP extraction of NWP (AS-PEAROME) model data."
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument('-b', '--datebegin', help='Begining date of extraction, format YYYYMMDDHH or YYMMDDHH',
                        required=True)
    parser.add_argument('-e', '--dateend', help = 'Final date of extraction (default=datebegin)')
    parser.add_argument('-d', '--domain', help='Domain of the file', choices=coords.keys(), default='GrandesRousses')
    # WARNING : the 'parameters' and 'level-type' arguments depend on each other.
    # TODO : find a way (a very complex dict ?) ton ensure consistency
    parser.add_argument('-p', '--parameter', help='Parameter to extract', default='ALTITUDE',
                        choices=['PRECIP', 'WETBT', 'ALTITUDE'])
    parser.add_argument('-l', '--levels', nargs='+', help='Level(s) of the parameter to extract',
                        default=['27315', '27415', '27465'])
    parser.add_argument('-v', '--level_type', help='Type of level of the parameter to extract', default='ISO_WETBT',
                        choices=['ISO_WETBT', 'SOL', 'HAUTEUR', 'ISOBARE', 'ISO_T', 'ISO_TPW'])
    parser.add_argument('-w', '--workdir', help="Runing directory (soprano's default)",)
    parser.add_argument('-m', '--model', help='NWP model from which the data must be extracted',
                        choices=['PAAROME', 'PAROME', 'PAA'], default='PAA')
    parser.add_argument('-c', '--cutoff', help='NWP model cutoff from which the data must be extracted',
                        choices=['assimilation', 'prevision'], default='assimilation')
    # TODO : documenter les périodes de disponibilités modèle/grille
    parser.add_argument('-g', '--grid', help='BDAP grid name from which to extract data', default='EURAT01',
                        choices=['FRANXL0025', 'EURW1S40', 'EURW1S100', 'EUROC25', 'GLOB025', 'EURAT01', 'EURAT1S20',
                                 'FRANX01', 'GLOB25', 'FRANGP0025'])
    parser.add_argument('-t', '--echeances', help='Echeances à extraire (integer or "first:last" format)')
    parser.add_argument('-H', '--pdt', help="Pas de temps du réseau d'extraction", type=int, default=None)

    args = parser.parse_args()

    return args


def goto(path):
    if not os.path.exists(path):
        os.makedirs(path)
    os.chdir(path)


class ExtractBDAP(object):

    def __init__(self, model, date, ech, parameter, level_type, levels, grid, domain):

        self.model          = model
        self.date           = date
        self.ech            = ech
        self.parameter      = parameter
        self.level_type     = level_type
        self.levels         = levels
        self.grid           = grid.upper()
        self.domain         = domain
        self.gribname       = f'{model}_{date.strftime("%Y%m%d%H")}_{ech}.grib'

        self.run()

    def requete(self):
        self.rqst = 'requete.tmp'
        f = open(self.rqst, "w")
        f.write('#RQST\n')
        f.write(f'#NFIC {self.gribname}\n')  # Output file name
        f.write(f'#MOD {self.model}\n')  # BDAP model name (see doc)
        f.write(f'#PARAM {self.parameter}\n')  # paramater
        f.write(f'#Z_REF {self.grid}\n')  # BDAP grib name (see doc)
        f.write("#Z_EXTR INTERPOLATION\n")  # Interpolation on a user-defined grid
        f.write('#Z_GEO ' + ' '.join(coords[self.domain]) + '\n')  # Coordonées d'extraction (N S W E)
        f.write('#Z_STP 100 100\n')  # Pas en lat/lon (0.1/0.1)
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
                self.extractedfiles.append(self.gribname)
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

        os.environ["DMT_DATE_PIVOT"] = self.date.strftime('%Y%m%d%H%M%S')
        result = self.extract_from_bdap()

        if result:
            return self.gribname
        else:
            return None


if __name__ == "__main__":
    args = parse_command_line()
    datebegin  = datetime.strptime(args.datebegin, '%Y%m%d%H')
    dateend    = datetime.strptime(args.dateend, '%Y%m%d%H')
    parameter  = args.parameter
    levels     = args.levels
    level_type = args.level_type
    model      = args.model
    grid       = args.grid
    domain     = args.domain
    parameter  = args.parameter
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

    extractedfiles = list()
    date = datebegin
    # Loop over dates and lead times
    while date <= dateend:
        for ech in echeances:
            # Launch extraction
            grib = ExtractBDAP(model, date, ech, parameter, level_type, levels, grid, domain)
            if grib is not None:
                extractedfiles.append(grib)
        date = date + timedelta(hours=max(dt, 1))  # Avoid infinite loops
    # Merge all grib files into a single one
    outname = f'{model}_{args.datebegin}_{args.dateend}.grib'
    os.system('cat ' + ' '.join(extractedfiles) + f' > {outname}')
