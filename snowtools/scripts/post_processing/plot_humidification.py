#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import numpy as np
import xarray as xr
import argparse

import matplotlib.pyplot as plt
import matplotlib.dates as mdates

from snowtools.tools import xarray_backend
from snowtools.utils.infomassifs import infomassifs

from bronx.stdtypes.date import Date, Period

from vortex import toolbox
import cen

toolbox.active_now = True


aspect_map = {
    0: {'color': 'k', 'label': 'N'},
    180: {'color': 'b', 'label': 'S'},
}


def parse_command_line():

    description = "Plot Synopsis-like figures for humidification depth"

    parser = argparse.ArgumentParser(description=description)

    parser.add_argument('-b', '--datebegin', type=str, required=True,
                        help="Begin date of the time serie")

    parser.add_argument('-e', '--dateend', type=str, required=True,
                        help="End date of the time serie")

    parser.add_argument('-x', '--xpid', type=str, default='oper',
                        help="XPID(s) of the simulation(s) format XP_NAME@username")

    parser.add_argument('-m', '--massif', nargs='+', required=True, type=int,
                        help="Massif(s) to plot")

    parser.add_argument('-z', '--elevation', nargs='+', required=True, type=float,
                        help="Elevation(s) to plot")

    parser.add_argument('-a', '--aspect', nargs='+', required=True, type=float,
                        help="Aspect(s) to plot")

    parser.add_argument('-s', '--slope', nargs='+', required=True, type=float,
                        help="Slope(s) to plot")

    parser.add_argument('-w', '--workdir', type=str,
                        default=f'/cnrm/cen/users/NO_SAVE/{os.environ["USER"]}/workdir/plot_synopsis',
                        help='Working directory (default value for sxcen)')

    parser.add_argument('-g', '--geometry', type=str, required=True,
                        help='Geometry of the simulation')

    args = parser.parse_args()

    args.datebegin = Date(args.datebegin)
    args.dateend   = Date(args.dateend)
    if not os.path.exists(args.workdir):
        os.makedirs(args.workdir)
    os.chdir(args.workdir)

    return args


def get_data(datebegin, dateend, xpid, geometry):

    date_pivot = dateend.replace(hour=9) - Period(hours=24)
    # On prend le réseau de 9h par défaut
    list_dates = [(datebegin + Period(days=n)).replace(hour=9) for n in range((date_pivot - datebegin).days + 1)]
    if datebegin.hour < 6:
        # TODO
        pass

    toolbox.input(
        kind        = 'SnowpackSimulation',
        local       = 'PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
        vapp        = 's2m',
        vconf       = '[geometry:tag]',
        experiment  = xpid,
        block       = 'pro',
        geometry    = geometry,
        date        = list_dates,
        datebegin   = '[date:ymd6h]/-PT24H',
        dateend     = '[date:ymd6h]',
        member      = 35,
        nativefmt   = 'netcdf',
        model       = 'surfex',
        namespace   = 'vortex.multi.fr',
        cutoff      = 'assimilation',
        fatal       = True,
    )

    toolbox.input(
        kind        = 'SnowpackSimulation',
        local       = 'PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
        vapp        = 's2m',
        vconf       = '[geometry:tag]',
        experiment  = xpid,
        block       = 'pro',
        geometry    = geometry,
        date        = date_pivot,
        datebegin   = '[date:ymd6h]',
        dateend     = '[datebegin]+/PT96H',
        member      = 35,
        nativefmt   = 'netcdf',
        model       = 'surfex',
        namespace   = 'vortex.multi.fr',
        cutoff      = 'production',
        fatal       = True,
    )


def clean():
    import glob
    for f in glob.glob('PRO*.nc'):
        os.remove(f)


def plot(dataset, list_aspects, title, filename):

    fig, ax = plt.subplots(figsize=(20, 6))
    hh = 0
    for aspect in list_aspects:
        color = aspect_map[aspect]['color']
        label = aspect_map[aspect]['label']
        tmp = dataset.where((dataset.aspect == aspect), drop=True)
        htn  = tmp.DSN_T_ISBA * 100
        Phum = tmp.WET_TH_ISBA * 100
        htn.plot(color=color, label=f"Hauteur de neige ({label})")
        for i in range(len(tmp.time)):
            bottom = (htn.data[i] - Phum.data[i])
            if i == 0:
                ax.bar(tmp.time.data[i] + np.timedelta64(hh, 'h'), Phum.data[i], bottom=bottom,
                    color=color, width=0.03, label=f"Profondeur d'humidification ({label})")
            else:
                ax.bar(tmp.time.data[i] + np.timedelta64(hh, 'h'), Phum.data[i], bottom=bottom,
                    color=color, width=0.03)
        hh = hh + 1
    ax.set_ylim([-50, 100 * np.ceil(dataset.DSN_T_ISBA.max() * 2) / 2])
    ax.set_ylabel("Hauteur (cm)")

    # Shrink current axis's height by 10% on the bottom
    box = ax.get_position()
    ax.set_position([box.x0, box.y0 + box.height * 0.1,
                     box.width, box.height * 0.9])

    # Put a legend below current axis
    ax.legend(loc='upper center', bbox_to_anchor=(0.5, -0.05),
              fancybox=True, shadow=True, ncol=len(list_aspects))

    ax.xaxis.set_major_locator(mdates.DayLocator())
    ax.xaxis.set_major_formatter(mdates.DateFormatter('%d. %b'))
    ax.xaxis.set_minor_locator(mdates.HourLocator([12]))
    ax.xaxis.set_minor_formatter(mdates.DateFormatter('%H:00'))
    ax.grid()
    ax.set_title(title)

    fig.savefig(filename, format='pdf')


if __name__ == '__main__':
    args = parse_command_line()

    get_data(args.datebegin, args.dateend, args.xpid, args.geometry)
    massifs_infos = infomassifs()
    for massif in args.massif:
        massif_name = massifs_infos.getMassifName(massif)
        for elevation in args.elevation:
            for slope in args.slope:
                ds = xr.open_mfdataset('PRO*.nc', engine='cen')
                ds = ds.sel(time=slice(args.datebegin.strftime('%Y-%m-%d %H'), args.dateend.strftime('%Y-%m-%d %H')))
                ds = ds.compute()
                # ds = xr.open_mfdataset('PRO*.nc', engine='cen', join='right', concat_dim='time', combine='nested')
                # ds = ds.drop_duplicates('time').sel(time=slice(args.datebegin.ymdh, args.dateend.ymdh)).compute()
                reduced_ds = ds.where((ds.massif_num == massif) & (ds.ZS == elevation) & (ds.slope == slope) &
                        (ds.aspect.isin(args.aspect)), drop=True)
                title = f'Massif: {massif_name} - Elevation: {elevation}m - slope: {slope}°'
                savename = f'Epaisseur_de_neige_humide_{massif}_{elevation}_{slope}_{args.datebegin.ymdh}_' \
                    f'{args.dateend.ymdh}.pdf'
                plot(reduced_ds, args.aspect, title, savename)
    clean()
