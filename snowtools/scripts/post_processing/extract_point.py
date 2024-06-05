#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''
'''

import os
import xarray as xr
import argparse
try:
    # Retrieve input files with Vortex
    from snowtools.scripts.extract.vortex import vortexIO as io
except ImportError:
    print('Vortex not available, input files must be defined by their absolute path')

reference_points = dict(
    # Galibier = dict(xx=968139.97, yy=6446319.56),  # Col
    Galibier = dict(xx=965767.64, yy=6445415.30),  # Nivose
    LacBlanc = dict(xx=944584.42, yy=6452410.74),  # Nivose
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

    parser.add_argument('-m', '--members', type=int, default=None,
                        help="Number of members associated to the experiment")

    parser.add_argument('-w', '--workdir', type=str, default=None,
                        help="Working directory")

    parser.add_argument('-p', '--pro', type=str,
                        help="Absolute path to the pro file."
                             "WARNING : bad practice")

    parser.add_argument('--point', type=str, choices=reference_points.keys(),
                        help="Name of the point to extract")

    args = parser.parse_args()

    return args


def execute(subdir, point):
    """
    Main method
    """
    proname = os.path.join(subdir, 'PRO.nc')
    pro     = xr.open_dataset(proname, decode_times=False)
    pro     = xrp.preprocess(pro)
    pro = pro.chunk({"xx": len(pro.xx), 'yy': len(pro.yy)})
    if 'ZS' in pro.keys():
        pro     = pro[['DSN_T_ISBA', 'ZS']]
    else:
        pro = pro.DSN_T_ISBA
    # out = pro.sel({'xx': reference_points[point]['xx'], 'yy': reference_points[point]['yy']})
    out = pro.interp({'xx': reference_points[point]['xx'], 'yy': reference_points[point]['yy']}, method='nearest')

    # Write output file
    outname = os.path.join(subdir, f'PRO_{point}.nc')
    outname = f'PRO_{point}.nc'
    out.to_netcdf(os.path.join(subdir, outname))
    return outname


def save_output(outname):

    io.put_pro(datebegin=datebegin, dateend=dateend, xpid=xpid, vconf=point, geometry='SinglePoint',
            namespace='vortex.cache.fr', filename=outname, vapp=vapp, members=members)
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

    try:
        subdir = 'mb[member:03d]' if members is not None else ''
        # Try to get the PRO file with vortexIO
        io.get_pro(datebegin=datebegin, dateend=dateend, xpid=xpid, geometry=geometry, member=members, vapp=vapp)
    except (ImportError, NameError, ModuleNotFoundError):
        # Otherwise a PRO file should be provided
        # TODO : Le code actuel ne gère qu'un unique fichier PRO
        # TODO : il reste à gérer les simulations d'ensemble avec 1 PRO/membre
        # shutil.copyfile(pro, 'PRO.nc')
        os.symlink(pro, 'PRO.nc')

    # 3. Execute the core algorithm and clean up working directory
    # ------------------------------------------------------------
    if members is None or members == 1:
        subdir = ''
        outname = execute(subdir, point)
        save_output(outname)
        os.remove('PRO.nc')
    else:
        for member in range(members):
            print(f'Member {member}')
            subdir = f'mb{member:03d}'
            outname = execute(subdir, point)
            save_output(outname)
            os.remove(os.path.join(subdir, 'PRO.nc'))
