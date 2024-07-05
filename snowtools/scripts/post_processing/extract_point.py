#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''
'''

import os
import numpy as np
import xarray as xr
import argparse
import snowtools.tools.xarray_preprocess as xrp
try:
    # Retrieve input files with Vortex
    from snowtools.scripts.extract.vortex import vortexIO as io
except ImportError:
    print('Vortex not available, input files must be defined by their absolute path')

reference_points = dict(
    # Galibier = dict(xx=968139.97, yy=6446319.56),  # Col
    Galibier = dict(xx=965767.64, yy=6445415.30),  # Nivose
    LacBlanc = dict(xx=944584.42, yy=6452410.74),  # Nivose
    RochillesNivose = dict(xx=972852.5, yy=6448853.87),  # Nivose
    NivometeoHuez = dict(xx=941654.19, yy=6448968.81),  # 1860m
)

members_map = dict(
    RS27_pappus        = [mb for mb in range(17)],
    EnKF36_pappus      = [mb for mb in range(17)],
    PF32_pappus        = [mb for mb in range(17)],
    RS27_sorted_pappus = [mb for mb in range(17)],
    ANTILOPE_pappus    = None,
    SAFRAN_pappus      = None,
)


def parse_command_line():
    description = "Extract a specific simulation point"

    parser = argparse.ArgumentParser(description=description)

    parser.add_argument('-b', '--datebegin', type=str, default=None,
                        help="First date covered by the simulation file, format YYYYMMDDHH.")

    parser.add_argument('-e', '--dateend', type=str, default=None,
                        help="Last date covered by the simulation file, format YYYYMMDDHH.")

    parser.add_argument('-x', '--xpid', type=str, default=None,
                        help="XPID of the simulation format XP_NAME@username")

    parser.add_argument('-g', '--geometry', type=str, default='GrandesRousses250m',
                        help='Geometry of the simulation(s) / observation')

    parser.add_argument('-a', '--vapp', type=str, default='s2m', choices=['s2m', 'edelweiss'],
                        help='vapp of the file')

    # parser.add_argument('-m', '--members', type=int, default=None,
    #                     help="Number of members associated to the experiment")
    parser.add_argument('-m', '--members', action='store_true',
                        help="To activate ensemble simulations")

    parser.add_argument('-w', '--workdir', type=str, default=None,
                        help="Working directory")

    parser.add_argument('-p', '--pro', type=str,
                        help="Absolute path to the pro file."
                             "WARNING : bad practice")

    parser.add_argument('--point', type=str, choices=reference_points.keys(),
                        help="Name of the point to extract")

    args = parser.parse_args()

    return args


def nearest(value, array):
    """ Find the closest element of 'array' to 'value'. """
    return array[np.abs(array - value).argmin()]


def execute(point, member):
    """
    Main method
    """
    proname = 'PRO.nc'
    if member is None or len(member) == 1:
        pro = xr.open_dataset(proname, decode_times=False)
    else:
        pro = xr.open_mfdataset([f'mb{mb:03d}/{proname}' for mb in member],
                concat_dim='member', combine='nested', chunks='auto')
    pro = xrp.preprocess(pro)
    pro = pro.chunk({"xx": 10, 'yy': 10})
    if 'ZS' in pro.keys():
        pro     = pro[['DSN_T_ISBA', 'ZS']]
    else:
        pro = pro.DSN_T_ISBA

    # The "sel" method is far more efficient in this case since it requires to read only 1 chunk
    # But the "reference_point" may not by within the dataset coordinates...
    # The workaround is to find first the nearest point of the domain with the custom "nearest" method,
    # and then use the faster "sel" method
    out = pro.sel({'xx': nearest(reference_points[point]['xx'], pro.xx.data),
        'yy': nearest(reference_points[point]['yy'], pro.yy.data)})
    # The "interp" method needs to read all coordinates to find the nearest one, thus chunking is useless
    # and the computation is very very slow
    # out = pro.interp({'xx': reference_points[point]['xx'], 'yy': reference_points[point]['yy']}, method='nearest')

    outname = f'PRO_{xpid}_{point}.nc'
    if os.path.exists(outname):
        os.remove(outname)
    print('Writing output file...')
    out.to_netcdf(outname)
    return outname


def save_output(outname):

    io.put_pro(datebegin=datebegin, dateend=dateend, xpid=xpid, vconf=point, geometry='SinglePoint',
            namespace='vortex.cache.fr', filename=outname, vapp=vapp)
    os.remove(outname)


if __name__ == '__main__':

    args = parse_command_line()
    datebegin = args.datebegin
    dateend   = args.dateend
    xpid      = args.xpid
    vapp      = args.vapp
    members   = args.members
    workdir   = args.workdir
    pro       = args.pro
    geometry  = args.geometry
    point     = args.point

    # 1. Move in a (clean) working directory
    # --------------------------------------
    if workdir is not None:
        os.chdir(workdir)

    # 2. Fill working directory with input files
    # -------------------------------------------

    if members:
        member = members_map[xpid]
    else:
        if xpid in ['safran_pappus', 'ANTILOPE_pappus', 'SAFRAN_pappus']:
            member = None
        else:
            member = [0]
    try:
        subdir = 'mb[member:03d]' if member is not None else ''
        # Try to get the PRO file with vortexIO
        io.get_pro(datebegin=datebegin, dateend=dateend, xpid=xpid, geometry=geometry, member=member, vapp=vapp)
    except (ImportError, NameError, ModuleNotFoundError):
        # Otherwise a PRO file should be provided
        # TODO : Le code actuel ne gère qu'un unique fichier PRO
        # TODO : il reste à gérer les simulations d'ensemble avec 1 PRO/membre
        # shutil.copyfile(pro, 'PRO.nc')
        os.symlink(pro, 'PRO.nc')

    # 3. Execute the core algorithm and clean up working directory
    # ------------------------------------------------------------
    outname = execute(point, member)
    save_output(outname)
    if member is None or len(member) == 1:
        os.remove('PRO.nc')
    else:
        for mb in member:
            print(f'Member {mb}')
            subdir = f'mb{mb:03d}'
            os.remove(os.path.join(subdir, 'PRO.nc'))
