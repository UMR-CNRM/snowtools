#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Auteur: Matthieu Vernay
# Date : 20/03/2024

import os
import argparse
from datetime import datetime, timedelta
import xarray as xr

"""
Outil d'extraction de données de la BDAP. Les données sont extraites sous forme de multiples fichiers grib (un
par date et échéance) et converties en un unique fichier netcdf par xarray.

Documentation :

- Contenu BDAP : http://intradsi.meteo.fr/spip.php?page=private_access&file=/IMG/pdf/bdap_utilisateurs-249.pdf
- Pour Alpha   : http://intradsi.meteo.fr/spip.php?page=private_access&file=/IMG/pdf/bdap_pprod-3.pdf
- extraction   : http://intradsi.meteo.fr/spip.php?page=private_access&file=/IMG/pdf/doc_utilisateurs_bdap_extr-14.pdf
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

# Pas en lat/lon de chaque grille en 1/1000 de °
dl = dict(
    FRANXL0025 = ['25', '25'],
    FRANGP0025 = ['25', '25'],
    EUROC25    = ['25', '25'],
    GLOB025    = ['25', '25'],
    EURW1S40   = ['25', '25'],
    EURAT01    = ['10', '10'],
    EURW1S100  = ['10', '10'],
    EURAT1S20  = ['05', '05'],
)

model_map = dict(
    PAAROME = dict(vapp='arome', vconf='3dvarfr'),  # Analyse AROME
    PAROME  = dict(vapp='arome', vconf='3dvarfr'),  # prevision AROME
    PAA     = dict(vapp='arpege', vconf='4dvarfr'),  # Analyse ARPEGE
    PA      = dict(vapp='arpege', vconf='4dvarfr'),  # Analyse ARPEGE
)

default_levels = dict(
    ISOBARE   = [100, 125, 150, 175, 200, 225, 250, 275, 300, 350, 400, 450, 500, 550, 600, 650, 700, 750, 800, 850,
                 900, 925, 950, 1000],
    HAUTEUR   = [2, 10, 20, 35, 50, 75, 100, 150, 200, 250, 375, 500, 625, 750, 875, 1000, 1125, 1250, 1375, 1500,
                 1750, 2000, 2250, 2500, 2750, 3000],
    ISO_T     = [25315, 26115, 26315, 27315],
    ISO_TPW   = [27315, 27415],
    ISO_WETBT = [27315, 27415],
)


def parse_command_line():
    description = "BDAP extraction of NWP (AS-PEAROME) model data."
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument('-b', '--datebegin', help='Begining date of extraction, format YYYYMMDDHH or YYMMDDHH',
                        required=True)
    parser.add_argument('-e', '--dateend', help = 'Final date of extraction (default=datebegin)', default=None)
    parser.add_argument('-d', '--domain', help='Domain of the file', choices=coords.keys(), default='GrandesRousses')
    # WARNING : the 'parameters' and 'level-type' arguments depend on each other.
    # TODO : find a way (a very complex dict ?) ton ensure consistency
    parser.add_argument('-p', '--parameter', help='Parameter to extract', default='ALTITUDE',
                        choices=['PRECIP', 'WETBT', 'ALTITUDE', 'TPW'])
    parser.add_argument('-l', '--levels', nargs='+', help='Level(s) of the parameter to extract', default=None)
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
    parser.add_argument('-a', '--archive', help="Archive output file with Vortex", action="store_true", default=False)

    args = parser.parse_args()

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

    def __init__(self, model, date, ech, parameter, level_type, levels, grid, domain):

        self.model          = model
        self.date           = date
        self.ech            = ech
        self.parameter      = parameter
        self.level_type     = level_type
        self.levels         = levels
        self.grid           = grid.upper()
        self.domain         = domain
        self.gribname       = f'{level_type}_{date.strftime("%Y%m%d%H")}_{ech}.grib'

    def requete(self):
        self.rqst = 'requete.tmp'
        f = open(self.rqst, "w")
        f.write('#RQST\n')
        f.write(f'#NFIC {self.gribname}\n')  # Output file name
        f.write(f'#MOD {self.model}\n')  # BDAP model name (see doc)
        f.write(f'#PARAM {self.parameter}\n')  # paramater
        f.write(f'#Z_REF {self.grid}\n')  # BDAP grib name (see doc)
        f.write("#Z_EXTR INTERPOLATION\n")  # Interpolation on a user-defined grid
        f.write('#Z_GEO ' + ' '.join(coords[self.domain]) + '\n')  # Coordonées de la grille de sortie (N S WA E)
        f.write('#Z_STP ' + ' '.join(dl[self.grid]) + '\n')  # Pas en lat/lon de la grille de sortie
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

        os.environ["DMT_DATE_PIVOT"] = self.date.strftime('%Y%m%d%H%M%S')
        result = self.extract_from_bdap()

        if result:
            return self.gribname
        else:
            return None


if __name__ == "__main__":
    args = parse_command_line()
    datebegin  = datetime.strptime(args.datebegin, '%Y%m%d%H')
    if args.dateend is not None:
        dateend = datetime.strptime(args.dateend, '%Y%m%d%H')
    else:
        dateend = datebegin
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
        outname = f'{level_type}_{grid}_{args.datebegin}_{args.dateend}.nc'
    else:  # The extraction is associated to a date
        outname = f'{level_type}_{grid}_{args.datebegin}.nc'
    if not os.path.exists(outname):
        extractedfiles = list()
        date = datebegin
        # Loop over dates and lead times
        while date <= dateend:
            for ech in echeances:
                # Launch extraction
                extractor = ExtractBDAP(model, date, ech, parameter, level_type, levels, grid, domain)
                grib = extractor.run()
                if grib is not None:
                    extractedfiles.append(grib)
            date = date + timedelta(hours=max(dt, 1))  # Avoid infinite loops

        # Open all extracted files with xarray
        print('Opening grib files with xarray...')
        ds  = xr.open_mfdataset(extractedfiles, concat_dim='valid_time', combine='nested', engine='cfgrib')
        print('Dataset created')
        ds.to_netcdf(outname)

    clean()

    ############################################
    # Everything beyond this point is optional #
    ############################################

    if args.archive:
        import cen  # Import necessary to load vortex CEN-specific ressourees
        import vortex
        from vortex import toolbox

        t = vortex.ticket()

        if level_type.startswith('ISO_'):
            kind = level_type
        else:
            print('Kind not yet defined for this extraxction')
            raise NotImplementedError

        out = toolbox.output(
            kind           = kind,
            vapp           = 'edelweiss',
            vconf          = domain,
            source_app     = model_map[model]['vapp'],
            source_conf    = model_map[model]['vconf'],
            model          = '[vapp]',
            cutoff         = 'assimilation',
            filename       = outname,
            experiment     = f'ExtractionBDAP@{os.environ["USER"]}',
            geometry       = grid,
            nativefmt      = 'netcdf',
            namebuild      = 'flat@cen',
            datebegin      = args.datebegin,
            dateend        = args.dateend,
            date           = '[dateend]',
            namespace      = 'vortex.multi.fr',
            block          = 'meteo',
        )
        out[0].quickview()
        print(t.prompt, 'Output location =', out[0].location())
        print()
