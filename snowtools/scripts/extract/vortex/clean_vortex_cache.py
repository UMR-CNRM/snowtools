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
)

description = dict(
    namespace = 'vortex.cache.fr',
    namebuild = 'flat@cen',
    nativefmt = 'netcdf',
    model     = 'surfex',
    filename  = 'whatever',
    vconf     = '[geometry:tag]',
)


def parse_args():
    parser = argparse.ArgumentParser(description=
    """
    Clean local vortex cache to ensure that outdated files are not used
    """)

    parser.add_argument("-b", "--datebegin", dest="datebegin", default=None, type=str,
                        help="Date of begining of the simulation.")

    parser.add_argument("-e", "--dateend", dest="dateend", default=None, type=str,
                        help="Date of end of the simulation.")

    parser.add_argument("-d", "--date", dest="date", default=None, type=str,
                        help="Date of the simulation.")

    parser.add_argument("-x", "--xpid", dest="experiment", nargs="+", required=True, type=str,
                        help="XPID of the simulation (format 'xpid@user').")

    parser.add_argument("-g", "--geometry", dest="geometry", required=True, type=str,
                        help="Explore a specific geometry / vconf")

    parser.add_argument("-a", "--vapp", dest="vapp", default='s2m', type=str,
                        choices=['s2m', 'edelweiss', 'safran', 'Pleiades', 'Sentinel2'],
                        help="vapp of the simulation")

    parser.add_argument("--block", dest="block", default=None, type=str,
                        help="Explore a specific block (for example 'meteo', 'prep', 'pro',...)")

    parser.add_argument("--kind", dest="kind", required=True, type=str, choices=block_map.keys(),
                        help="Kind of resource (values such as 'FORCING', 'PRO', 'DIAG',...) are accepted")

    parser.add_argument("-m", "--member", dest="member", default=None, type=str,
            help="Simulation members (format first:last)")

    args = parser.parse_args()

    if args.member is not None:
        first_mb, last_mb = args.member.split(':')
        args.member = [mb for mb in range(int(first_mb), int(last_mb) + 1)]

    args.block = block_map[args.kind]

    if args.kind in ['DIAG']:
        args.model = 'postproc'

    if args.kind in kind_map.keys():
        args.kind = kind_map[args.kind]

    if args.date is None:
        args.date = args.dateend

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


if __name__ == '__main__':
    args = parse_args()
    user_footprints = vars(args)
    description.update(user_footprints)
    clean(description)
