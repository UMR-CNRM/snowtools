import os
import argparse

import cen  # Import necessary to load vortex CEN-specific ressourees
from vortex import toolbox

"""
WORK IN PROGRESS
"""

kind_map = dict(
    FORCING = 'MeteorologicalForcing',
    PRO     = 'SnowpackSimulation',
    DIAG    = 'SnowpackSimulation',  # TODO : à modifier après ré-organisation des resources Vortex
)

block_map = dict(
    MeteorologicalForcing = 'meteo',
    FORCING               = 'meteo',
    SnowpackSimulation    = 'pro',
    PRO                   = 'pro',
    DIAG                  = 'diag',
    SnowObservations      = '',
    Precipitation         = '',
)

default = dict(
    namespace = 'vortex.multi.fr',
    namebuild = 'flat@cen',
    nativefmt = 'netcdf',
    model     = 'surfex',
    vconf     = '[geometry:tag]',
    now       = True,
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

    parser.add_argument("-d", "--date", dest="date", default=None, type=str,
                        help="Date of the simulation.")

    parser.add_argument("-x", "--xpid", dest="experiment", required=True, type=str,
                        help="XPID of the simulation (format 'xpid@user').")

    parser.add_argument("-g", "--geometry", dest="geometry", required=True, type=str,
                        help="Explore a specific geometry / vconf")

    parser.add_argument("-k", "--kind", dest="kind", required=True, type=str, choices=block_map.keys(),
                        help="Kind of resource (values such as 'FORCING', 'PRO', 'DIAG',...) are accepted")

    parser.add_argument("-f", "--filename", dest="filename", default=None, type=str,
                        help="Local name of the file to get / put.")

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
    rh = toolbox.input(**description)

    return rh


def put(**description):
    default.update(description)
    description = footprint_kitchen(**default)
    rh = toolbox.output(**description)

    return rh


def footprint_kitchen(**kw):

    if 'xpid' in kw.keys():
        kw['experiment'] = kw.pop('xpid')

    if 'vconf' not in kw.keys():
        kw['vconf'] = '[geometry:tag]',

    if 'role' not in kw.keys() or kw['role'] is None:
        kw['role'] = kw['kind']

    if 'filename' not in kw.keys() or kw['filename'] is None:
        kw['filename'] = f'{kw["kind"]}.nc'

    if 'member' in kw.keys() and kw['member'] is not None:
        # first_mb, last_mb = kw['member'].split(':')
        # kw['member'] = [mb for mb in range(int(first_mb), int(last_mb) + 1)]
        kw['filename'] = f'mb[member]/{kw["filename"]}'

    if 'block' not in kw.keys():
        kw['block'] = block_map[kw['kind']]

    if kw['model'] is None:
        kw['model'] = kw['vapp']
    elif kw['kind'] in ['DIAG']:
        kw['model'] = 'postproc'  # TODO : à modifier après ré-organisation de Vortex

    if kw['kind'] in kind_map.keys():
        kw['kind'] = kind_map[kw['kind']]

    if 'date' not in kw.keys() or kw['date'] is None:
        kw['date'] = kw['dateend']

    if "server" in kw.keys():
        if kw["server"] is not None:
            kw["namespace"] = namespace_map[kw["server"]]["namespace"]
            kw["storage"] = namespace_map[kw["server"]]["storage"]
        else:
            kw.pop("server")

    return kw


if __name__ == '__main__':
    args = parse_args()
    user_footprints = vars(args)
    action = user_footprints.pop('action')
    function_map()[action](**user_footprints)
