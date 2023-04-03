#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import argparse
import sys
import logging

import matplotlib.pyplot as plt
import xarray as xr
import numpy as np

from snowtools.utils.prosimu import prosimu
from snowtools.plots import proReader_mini

logger = logging.getLogger()
logger.setLevel(logging.WARNING)
console_handler = logging.StreamHandler()
console_handler.setFormatter(logging.Formatter('%(levelname)s :: %(message)s'))
console_handler.setLevel(logging.DEBUG)
logger.addHandler(console_handler)


#############################################################
# Le but est de comparer 2 fichiers PRO
# 2 choses apparaissent:
# - un graphique "double" avec les 2 côte à côte
# - une sortie "texte" pour faire des comparaisons globales
#
# Exemple d'appel:
# python3 compare.py PRO1_2010080106_2011080106.nc PRO2_2010080106_2011080106.nc -t1 'BC21' -t2 'C13' -b 2010111206 -e 2011011706
# python3 /home/fructusm/git/snowtools_git/snowtools/plots/compare.py Test_new_tartes/pro/PRO_2010080106_2011080106.nc Test_old_Tartes/pro/PRO_2010080106_2011080106.nc -t1 'New Tartes' -t2 'Old Tartes' -b 2010080106 -e 2011080106 -v TALB_ISBA
#
# Options de lancement:
# -b et -e pour begin et end pour tracer entre ces dates
# -t1 et -t2 pour les titres des graphiques si on veut qque chose de propre
# -v pour le nom de la variable à tracer
# -o pour le nom des fichiers à sauver
# --point pour le choix du point
# (sinon, on le trouve via massif, angle, aspect, slope via les options
#                         -m,     -a,    -as,    -s ) 
#
# Idées des comparaisons globales: 
# - voir métamorphisme (SSA, Type de grain, albedo en surface)
# - voir impact global sur le manteau (htn, densité, snow water equivalent)
# Du coup, dans les comparaisons globales: 
# - htn moyenne sur la période avec DSN_T_ISBA
# - rho moyen avec SNOWRHO
# - snow water equivalent avec WSN_T_ISBA
# - Nb couche fragile avec SNOWTYPE
# - SSA moyenne ? avec SNOWSSA
# - albedo moyen avec TALB_ISBA 
#

def make_double_graph(path_pro1, path_pro2, variable, titre1, titre2, date_begin, date_end, output_name, point, bool_snow_layer, bool_bands=False):
    """Plot 2 PRO files side by side in order to compare them

    :param path_pro1: The path to first PRO file to plot
    :type path_pro1: str
    :param path_pro2: The path to second PRO file to plot
    :type path_pro2: str
    :param variable: The variable to plot (SNOWSSA by default)
    :type variable: str
    :param titre1: Title for first plot in graph (default = 'PRO1')
    :type titre1: str
    :param titre2: Title for second plot in graph (default = 'PRO2')
    :type ittre2: str
    :param date_begin: If you want to zoom the graph between two dates. Format YYYYMMDDHH
    :type date_begin: str
    :param date_end: If you want to zoom the graph between two dates. Format YYYYMMDDHH
    :type date_end: str
    :param output_name: Not mandatory. If a name is given, the plot is saved.
    :type output_name: str
    :param point: If there are several points in the PRO files, the graph is made for choosen point
    :type point: str
    :param bool_snow_layer: If the variable is 1D, this is a 1D plot which is made
    :type bool_snow_layer: boolean

    :returns: the graph (possibly saved if an output name is given)
    """
    fig, (ax1, ax2) = plt.subplots(1, 2, sharey=True)
    fig.suptitle('Compare ' + titre1 + ' and ' + titre2)
    pro1 = proReader_mini.ProReader_standard(ncfile = path_pro1, var = variable, point = int(point))
    pro2 = proReader_mini.ProReader_standard(ncfile = path_pro2, var = variable, point = int(point))
    if bool_snow_layer:
        pro1.plot(ax1, variable, date_begin, date_end, real_layers = True, legend = variable)
        pro2.plot(ax2, variable, date_begin, date_end, real_layers = True, legend = variable)
    elif 'bands' in prosimu(path_pro1).getdimvar(variable):
        pro1.plot1D_bande(ax1, variable, date_begin, legend = variable)
        pro2.plot1D_bande(ax2, variable, date_begin, legend = variable)
    else:
        pro1.plot1D(ax1, variable, date_begin, date_end, legend = variable)
        pro2.plot1D(ax2, variable, date_begin, date_end, legend = variable)
    plt.show()
    if output_name is not None:
        name_save_fig = output_name+'.png'
        plt.savefig(name_save_fig, dpi=200)


def make_text_comparaison(path_pro1, path_pro2, date_begin, date_end, output_name, point):
    """Comparison of 2 PRO files. This is done with global characteristics like mean of height of snow...

    :param path_pro1: The path to first PRO file to compare
    :type path_pro1: str
    :param path_pro2: The path to second PRO file to compare
    :type path_pro2: str
    :param date_begin: If you want to compare the graph between two dates. Format YYYYMMDDHH
    :type date_begin: str
    :param date_end: If you want to compare the graph between two dates. Format YYYYMMDDHH
    :type date_end: str
    :param output_name: Not mandatory. If a name is given, the text is saved.
    :type output_name: str
    :param point: If there are several points in the PRO files, the comparison is made for choosen point
    :type point: str

    :returns: a text with all the comparison (possibly saved if an output name is given)
    """

    # ds.sel(time=slice("2021-01-03", "2021-01-05"))
    if output_name is not None:
        name_save_text = output_name + '.txt'
    else:
        name_save_text = 'Log_texte.log'
    texte = logging.getLogger()
    texte.setLevel(logging.INFO)
    texte_handler = logging.FileHandler(name_save_text)
    texte.addHandler(texte_handler)

    dsPro1 = xr.open_dataset(path_pro1,decode_times=False).sel(Number_of_points=point)
    dsPro2 = xr.open_dataset(path_pro2,decode_times=False).sel(Number_of_points=point)

    def test_moyenne(dsPro1, dsPro2, texte, var, texte_info):
        if var in dsPro1.variables and var in dsPro2.variables:
            var_moy1 = float(dsPro1[var].mean(skipna=True))
            var_moy2 = float(dsPro2[var].mean(skipna=True))
            texte.info(texte_info + ' du manteau neigeux:')
            texte.info(path_pro1 + ' : ' + str(var_moy1))
            texte.info(path_pro2 + ' : ' + str(var_moy2))
            if var_moy1 > 0.01: 
                 texte.info('Ecart de Pro2 par rapport à Pro1 en pourcentage: ' + str( 100*(var_moy2 - var_moy1)/var_moy1 ) + ' % \n' )

    def test_nb_couche_fragile(dsPro1, dsPro2, texte, var, texte_info):
        if var in dsPro1.variables and var in dsPro2.variables:
            ds1 = float(xr.where((dsPro1[var] > 8) & (dsPro1[var] < 12), 1, 0).sum(axis=2).mean())
            ds2 = float(xr.where((dsPro2[var] > 8) & (dsPro2[var] < 12), 1, 0).sum(axis=2).mean())
            texte.info(texte_info + ' du manteau neigeux:')
            texte.info(path_pro1 + ' : ' + str(ds1))
            texte.info(path_pro2 + ' : ' + str(ds2))
            if ds1 > 0.01: 
                 texte.info('Ecart de Pro2 par rapport à Pro1 en pourcentage: ' + str( 100*(ds2 - ds1)/ds1 ) + ' % \n' )

    def test_nb_couche_total(dsPro1, dsPro2, texte, var, texte_info):
        if var in dsPro1.variables and var in dsPro2.variables:
            ds1 = float(xr.where(np.isnan(dsPro1[var]), 0, 1).sum(axis=2).mean())
            ds2 = float(xr.where(np.isnan(dsPro2[var]), 0, 1).sum(axis=2).mean())
            texte.info(texte_info + ' du manteau neigeux:')
            texte.info(path_pro1 + ' : ' + str(ds1))
            texte.info(path_pro2 + ' : ' + str(ds2))
            if ds1 > 0.01: 
                 texte.info('Ecart de Pro2 par rapport à Pro1 en pourcentage: ' + str( 100*(ds2 - ds1)/ds1 ) + ' % \n' )

    test_moyenne(dsPro1, dsPro2, texte, 'DSN_T_ISBA', 'Hauteur moyenne')
    test_moyenne(dsPro1, dsPro2, texte, 'WSN_T_ISBA', 'Equivalent en eau moyen')
    test_moyenne(dsPro1, dsPro2, texte, 'TALB_ISBA', 'Albedo moyen')
    test_moyenne(dsPro1, dsPro2, texte, 'SNOWSSA', 'SSA moyenne')
    test_moyenne(dsPro1, dsPro2, texte, 'SNOWRO', 'densité moyenne')
    test_nb_couche_fragile(dsPro1, dsPro2, texte, 'SNOWTYPE', 'nb moyen de couches fragiles')
    test_nb_couche_total(dsPro1, dsPro2, texte, 'SNOWDZ', 'nb moyen de couches')

    if output_name is None:
        os.remove(name_save_text)


def parseArguments(args):
    """Parsing the arguments when you call the main program.

    :param args: The list of arguments when you call the main program (typically sys.argv[1:] )
    """


    # Create argument parser
    parser = argparse.ArgumentParser()

    # Mandatory argument
    parser.add_argument("pro1", help = "Path to first PRO file", type = str)
    parser.add_argument("pro2", help = "Path to second PRO file", type = str)

    # Optional argument
    parser.add_argument("-v", "--variable", help = "Variable to plot", type = str, default = 'SNOWSSA')
    parser.add_argument("-t1", "--titre1", help = "Title for first graph", type = str, default = 'PRO1')
    parser.add_argument("-t2", "--titre2", help = "Title for second graph", type = str, default = 'PRO2')
    parser.add_argument("-b", "--begin", help = "Start date for graph, format=YYYYMMDDHH, example: -b 2018010106", type = str)
    parser.add_argument("-e", "--end", help = "End date for graph, format=YYYYMMDDHH, example: -e 2018070106", type = str)
    parser.add_argument("-o", "--output", help = "Name for file to save", type = str)

    # Optional for point selection
    parser.add_argument("-a", "--alt", help = "altitude", type = int)
    parser.add_argument("-as", "--aspect", help = "aspect", type = int)
    parser.add_argument("-m", "--massif", help = "massif", type = int)
    parser.add_argument("-s", "--slope", help = "slope for massif graph", type=int)
    parser.add_argument("--point", help = "point number", type = int)

    args = parser.parse_args(args)
    return args


def main(args=None):
    """Main program: parse argument then launch plot and text comparison for the 2 PRO files

    """
    args = args if args is not None else sys.argv[1:]
    if len(sys.argv) > 1: 
        args = parseArguments(args)

        # argument for command line call
        path_pro1 = args.pro1
        path_pro2 = args.pro2
        variable = args.variable
        titre1 = args.titre1
        titre2 = args.titre2
        date_begin = args.begin
        date_end = args.end
        output_name = args.output

        # argument for saving graph without GUI
        altitude = args.alt
        aspect = args.aspect
        massif = args.massif
        slope = args.slope
        point = args.point

        # Check if mandatory path for PRO1 and PRO2 are OK
        if not os.path.isfile(path_pro1):
            logger.critical('Provided path for PRO1 file does not exist ({})'.format(path_pro1))
            sys.exit(1)

        if not os.path.isfile(path_pro2):
            logger.critical('Provided path for PRO2 file does not exist ({})'.format(path_pro2))
            sys.exit(1)

        # Get point of interest
        if (altitude is not None or aspect is not None or slope is not None or massif is not None) and point is None:
            with prosimu(path_pro1) as ff:
                point_pro1 = ChoixPointMassif(ff, altitude, aspect, massif, slope, exitonerror=True)[0]
            with prosimu(path_pro2) as ff:
                point_pro2 = ChoixPointMassif(ff, altitude, aspect, massif, slope, exitonerror=True)[0]
            if point_pro1 != point_pro2:
                logger.critical('Point is not the same for both simulation !! ')
                sys.exit(1)
                 
        if point is None:
            point = 0

        # check variables exist
        if variable is not None:
            with prosimu(path_pro1) as ff1:
                if variable is not None and variable not in ff1.listvar():
                    logger.critical('Variable {} does not exist in {}'.format(variable, path_pro1))
                    sys.exit(3)
            with prosimu(path_pro2) as ff2:
                if variable is not None and variable not in ff2.listvar():
                    logger.critical('Variable {} does not exist in {}'.format(variable, path_pro2))
                    sys.exit(3)

        # Launch the app
        bool_not_snow_layer = 'snow_layer' not in prosimu(path_pro1).getdimvar(variable) and 'snow_layer' not in prosimu(path_pro2).getdimvar(variable)
        bool_not_bands = 'bands' not in prosimu(path_pro1).getdimvar(variable) and 'bands' not in prosimu(path_pro2).getdimvar(variable)
        bool_nothing = bool_not_snow_layer * bool_not_bands
        if variable is not None and bool_nothing:
            make_double_graph(path_pro1, path_pro2, variable, titre1, titre2, date_begin, date_end, output_name, point, False)
        elif variable is not None and bool_not_bands:
            make_double_graph(path_pro1, path_pro2, variable, titre1, titre2, date_begin, date_end, output_name, point, True)
        else:
            make_double_graph(path_pro1, path_pro2, variable, titre1, titre2, date_begin, date_end, output_name, point, False, True)
        make_text_comparaison(path_pro1, path_pro2, date_begin, date_end, output_name, point)

if __name__ == '__main__':
    main()

