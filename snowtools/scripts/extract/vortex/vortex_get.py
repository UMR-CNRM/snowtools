#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import argparse

import cen  # Import necessary to load vortex CEN-specific ressourees
from vortex import toolbox

"""
WORK IN PROGRESS
"""

kind_map = dict(
    FORCING       = 'MeteorologicalForcing',
    PRO           = 'SnowpackSimulation',
    DIAG          = 'SnowpackSimulation',  # TODO : à modifier après ré-organisation des resources Vortex
    # SODA          = ['PART', 'BG_CORR', 'IMASK', 'ALPHA'],
    SODA          = ['PART', 'IMASK', 'ALPHA'],
    Precipitation = 'Precipitation',
    SnowObs       = 'SnowObservations',
)

block_map = dict(
    FORCING               = 'meteo',
    PRO                   = 'pro',
    DIAG                  = 'diag',
    SODA                  = 'soda',
    SnowObservations      = '',
    Precipitation         = '',
    MeteorologicalForcing = 'meteo',
    SnowpackSimulation    = 'pro',
)

model_map = dict(
    DIAG    = 'postproc',
    SODA    = 'soda',
    PRO     = 'surfex',
)

# toolbox.defaults(
default = dict(
    namespace  = 'vortex.multi.fr',
    namebuild  = 'flat@cen',
    nativefmt  = 'netcdf',
    vconf      = '[geometry:tag]',  # CEN-specific norm
    filename   = '[kind]_[datebegin:ymdh]_[dateend:ymdh].nc',
    role       = '[kind]',
    date       = '[dateend]',
    experiment = '[xpid]',
    # TODO : The *model* footprint should (almost ?) always be optionnal for CEN resources
    model      = 's2m',
    # model      = '[vapp]' if '[kind]' != 'DIAG' else 'postproc',
    now        = True,
    dateassim  = None,
    block      = None,
)

namespace_map = dict(
    hendrix = dict(
        namespace = 'vortex.archive.fr',
        storage   = 'hendrix.meteo.fr',
    ),
    soprano = dict(
        namespace = 'vortex.archive.fr',
        storage   = 'sotrtm35-sidev.meteo.fr'
    ),
)


def function_map():
    """
    Returns a dictionary to map function names to function objects
    """
    return {"get": get, "put": put, "clean": clean}


def parse_args():
    parser = argparse.ArgumentParser(description='Manage Vortex resources')

    parser.add_argument("action", choices=['get', 'put', 'clean'],
                        help="What to do with the specified resource(s)")

    parser.add_argument("-b", "--datebegin", dest="datebegin", default=None, type=str,
                        help="Date of begining of the simulation.")

    parser.add_argument("-e", "--dateend", dest="dateend", default=None, type=str,
                        help="Date of end of the simulation.")

    parser.add_argument("-a", "--dateassim", dest="dateassim", default=None, nargs='+',
                        help="Assimilation date(s)")

    parser.add_argument("-d", "--date", dest="date", default=None, type=str,
                        help="Date of the simulation.")

    parser.add_argument("-x", "--xpid", dest="experiment", required=True, type=str,
                        help="XPID of the simulation (format 'xpid@user').")

    parser.add_argument("-g", "--geometry", dest="geometry", required=True, type=str,
                        help="The geometry of the ressource (must be defined in the 'geometries.ini' file !)")

    parser.add_argument("-k", "--kind", dest="kind", required=True, type=str, choices=kind_map.keys(),
                        help="Kind of resource (values such as 'FORCING', 'PRO', 'DIAG',...) are accepted")

    parser.add_argument("-f", "--filename", dest="filename", default=None, type=str,
                        help="local name of the file to get / put.")

    parser.add_argument("-w", "--workdir", dest="workdir", default=None, type=str,
                        help="Local working directory to get / puyt the ressource(s)")

    parser.add_argument("--vapp", dest="vapp", default='s2m', type=str,
                        help="vapp of the simulation (ex: s2m, edelweiss, antilope, arome,...)")

    parser.add_argument("--block", dest="block", default=None, type=str,
                        help="Explore a specific block (for example 'meteo', 'prep', 'pro',...)")

    parser.add_argument("-s", "--server", type=str, default=None,
                        choices=['hendrix', 'soprano'],
                        help="Provider namespace. Use this option to force the target file(s) to be taken"
                        "from a specific server. Use this option to refill a resource that has been modified remotely"
                        "but is already on the local cache")

    parser.add_argument("-m", "--member", dest="member", default=None, type=str,
                        help="Simulation members (format first:last)")

    args = parser.parse_args()

    if args.action == 'clean':
        args.namespace = 'vortex.cache.fr',

    return args


def clean(description):

    target = toolbox.rload(**description)
    for rh in target:
        rh.quickview()
        filename = rh.locate()
        if os.path.exists(filename):
            os.remove(filename)
            print(f'File {filename} has been removed')
        else:
            print(f'File {filename} does not exists')


def get(**description):
    default.update(description)
    description = footprint_kitchen(**default)

    if description['dateassim'] is not None:
        # Verrue pour gérer les ressources issues de l'assimilation (plusieurs fichiers par saison)
        # TODO : Trouver une solution plus propre
        if description["block"] == "soda":
            # TODO : Gérer ce changement de valeur par défaut plus proprement ?
            description["nativefmt"]  = 'ascii'
            description["filename"] = '[kind]_[dateassim:ymdh].txt'
        else:
            # Retrieve files covering sub-periods between each assimilation date
            # TODO : trouver un moyen plus élégant en utilisant un dictionnaire dans les footprints ?
            datesassim = description.pop("dateassim") + [description["dateend"]]
            rh = list()
            for date in datesassim:
                description["dateend"] = date
                rh.append(toolbox.input(**description))
                description["datebegin"] = description["dateend"]
            return rh

    rh = toolbox.input(**description)

    return rh


def put(**description):
    default.update(description)
    description = footprint_kitchen(**default)
    rh = toolbox.output(**description)

    return rh


def footprint_kitchen(**kw):
    """
    This is the method used to set default footprint values
    """

    # if 'xpid' in kw.keys():
    #     # Use proper footprint (optionnal)
    #     kw['experiment'] = kw.pop('xpid')
    if kw['block'] is None:
        kw['block'] = block_map[kw['kind']]

    if 'member' in kw.keys():
        if isinstance(kw['member'], int):
            kw['member'] = [kw['member']]
        elif ':' in kw['member']:
            first_mb, last_mb = kw['member'].split(':')
            kw['member'] = [mb for mb in range(int(first_mb), int(last_mb) + 1)]
        kw['filename'] = f'mb[member]/{kw["filename"]}'

    if "workdir" in kw.keys():
        kw['filename'] = f'{kw["workdir"]}/{kw["filename"]}'
        kw.pop("workdir")

    # TODO : le bloc suivant sera à modifier après ré-organisation de Vortex
    # TODO : The *model* footprint should (almost ?) always be optionnal for CEN resources
    if kw['kind'] in model_map.keys():
        kw['model'] = model_map[kw['kind']]
    else:
        kw['model'] = kw['vapp']

    if kw['kind'] in kind_map.keys():
        kw['kind'] = kind_map[kw['kind']]

    if "server" in kw.keys():
        kw["namespace"] = namespace_map[kw["server"]]["namespace"]
        kw["storage"] = namespace_map[kw["server"]]["storage"]

    if 'date' not in kw.keys():
        if 'dateend' in kw.keys():
            kw['date'] = '[dateend]'
        elif 'dateassim' in kw.keys():
            kw['date'] = '[dateassim]'

    return kw


if __name__ == '__main__':
    args = parse_args()
    user_footprints = vars(args)
    action = user_footprints.pop('action')
    # Remove None values (a default value should be defined)
    actual_user_footprints = {k: v for k, v in user_footprints.items() if v is not None}
    function_map()[action](**actual_user_footprints)
