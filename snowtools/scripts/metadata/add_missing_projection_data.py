#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse
import xarray as xr

from snowtools.utils.S2M_standard_file import LCCProjectionType

parser = argparse.ArgumentParser(description="""
Add missing Lambert93 projection information to netcdf file.
""")

parser.add_argument("inputnc", help="Input NetCDF file")
parser.add_argument("outputnc", help="Output NetCDF file")
parser.add_argument("--xname", default=None, dest='xname', help="variable name containing x coordinate")
parser.add_argument("--yname", default=None, dest='yname', help="variable name containing y coordinate")


if __name__ == "__main__":
    args = parser.parse_args()
    ds = xr.open_dataset(args.inputnc)
    if args.xname:
        ds[args.xname].attrs['standard_name'] = 'projection_x_coordinate'
    if args.yname:
        ds[args.yname].attrs['standard_name'] = 'projection_y_coordinate'
    for var in ds.data_vars:
        x = None
        y = None
        # print(ds[var].attrs)
        # print(ds[var].dims)
        for cordinate in ds[var].coords:
            if ds[cordinate].attrs['standard_name'] == 'projection_x_coordinate':
                x = ds[cordinate].data
            elif ds[cordinate].attrs['standard_name'] == 'projection_y_coordinate':
                y = ds[cordinate].data

        # print(hasattr(ds, ds[var].attrs['grid_mapping']))
        # print(hasattr(ds, var))
        # print('grid_mapping' in ds[var].attrs.keys())
        if 'grid_mapping' in ds[var].attrs.keys():
            projvar = ds[var].attrs['grid_mapping']
            if len(x) > 0 and len(y) > 0:
                if hasattr(ds, projvar):
                    llc = LCCProjectionType(x, y, **ds[projvar].attrs)
                    ds[projvar].attrs = llc.__dict__
                else:
                    llc = LCCProjectionType(x, y)
                    ds[projvar] = xr.DataArray(None, dims=None, coords=None, attrs=llc.__dict__)
                # print(llc.__dict__)
            else:
                raise Exception("variable with grid_mapping found, but no projection_x_coordinate or "
                                "projection_y_coordinate. You might consider specifying xname and yname options.")
        ds.to_netcdf(args.outputnc)


