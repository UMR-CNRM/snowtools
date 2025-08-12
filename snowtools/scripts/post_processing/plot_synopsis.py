#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import sys
import numpy as np
import xarray as xr
import argparse

import matplotlib.pyplot as plt
import matplotlib.dates as mdates

from snowtools.tools import xarray_backend
from snowtools.utils.infomassifs import infomassifs
from snowtools.plots.stratiprofile.profilPlot import dateProfil

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

    parser.add_argument('-t', '--time', type=str,
                        help="Time of the plot for snapshot plots")

    parser.add_argument('-x', '--xpid', type=str, default='oper',
                        help="XPID(s) of the simulation(s) format XP_NAME@username")

    parser.add_argument('-m', '--massif', nargs='+', required=True, type=int,
                        help="Massif(s) to plot")

    parser.add_argument('-z', '--elevation', nargs='+', required='--humidification' in sys.argv, type=float,
                        default=None, help="Elevation(s) to plot")

    parser.add_argument('-a', '--aspect', nargs='+', required='--humidification' in sys.argv, type=float,
                        default=None, help="Aspect(s) to plot")

    parser.add_argument('-s', '--slope', nargs='+', required='--humidification' in sys.argv, type=float,
                        default=None, help="Slope(s) to plot")

    parser.add_argument('-w', '--workdir', type=str,
                        default=f'/cnrm/cen/users/NO_SAVE/{os.environ["USER"]}/workdir/plot_synopsis',
                        help='Working directory (default value for sxcen)')

    parser.add_argument('-g', '--geometry', type=str, required=True,
                        help='Geometry of the simulation')

    parser.add_argument('--humidification', action='store_true',
                        help='Plot snowpack humidification depth over time')

    parser.add_argument('--meteogram', action='store_true',
                        help='Plot iso-0°, precipitation and rain-sno limit over time')

    parser.add_argument('--profiles', action='store_true',
                        help='Plot vertical snow profiles for all aspects at a given elevation')

    args = parser.parse_args()

    args.datebegin = Date(args.datebegin)
    args.dateend   = Date(args.dateend)
    if args.time is not None:
        args.time = Date(args.time)
    if not os.path.exists(args.workdir):
        os.makedirs(args.workdir)
    os.chdir(args.workdir)

    return args


def get_s2m(datebegin, dateend, xpid, geometry, kind):

    if kind == 'SnowpackSimulation':
        filename = 'PRO'
        block    = 'pro'
        model    = 'surfex'
        date_pivot = dateend.replace(hour=9) - Period(hours=24)
    elif kind == 'MeteorologicalForcing':
        filename = 'FORCING'
        block    = 'meteo'
        model    = 'safran'
        date_pivot = dateend.replace(hour=3) - Period(hours=24)

    # On prend le réseau de 9h par défaut
    list_dates = [(datebegin + Period(days=n)).replace(hour=9) for n in range((date_pivot - datebegin).days + 1)]
    if datebegin.hour < 6:
        # TODO
        pass

    toolbox.input(
        kind        = kind,
        local       = f'{filename}_[datebegin:ymdh]_[dateend:ymdh].nc',
        vapp        = 's2m',
        vconf       = '[geometry:tag]',
        experiment  = xpid,
        block       = block,
        geometry    = geometry,
        date        = list_dates,
        datebegin   = '[date:ymd6h]/-PT24H',
        dateend     = '[date:ymd6h]',
        member      = 35,
        nativefmt   = 'netcdf',
        model       = model,
        namespace   = 'vortex.multi.fr',
        cutoff      = 'assimilation',
        fatal       = True,
    )

    toolbox.input(
        kind        = kind,
        local       = f'{filename}_[datebegin:ymdh]_[dateend:ymdh].nc',
        vapp        = 's2m',
        vconf       = '[geometry:tag]',
        experiment  = xpid,
        block       = block,
        geometry    = geometry,
        date        = date_pivot,
        datebegin   = '[date:ymd6h]' if kind == 'SnowpackSimulation' else '[date:ymd6h]/-PT24H',
        dateend     = '[date:ymd6h]+/PT96H',
        member      = 35,
        nativefmt   = 'netcdf',
        model       = model,
        namespace   = 'vortex.multi.fr',
        cutoff      = 'production',
        fatal       = True,
    )


def get_arpege(datebegin, dateend, massif):

    extraction_dir = f'/home/mrns/vernaym/workdir/extraction_PAA/{massif}/ALTITUDE/'
    iso0 = f'IS0_ARPEGE_{massif}_{datebegin.ymdh}_{dateend.ymdh}.nc'
    iso1wetbt = f'ISO_WBT_ARPEGE_{massif}_{datebegin.ymdh}_{dateend.ymdh}.nc'

    toolbox.input(
        kind        = 'gridpoint',
        local       = iso0,
        hostname    = 'sotrtm35-sidev.meteo.fr',
        username    = 'vernaym',
        tube        = 'ftp',
        remote      = f'{extraction_dir}/PAA_ALTITUDE_ISO_T_EURAT01_{datebegin.ymdh}_{dateend.ymdh}.nc',
        unknown     = True,
    )
    toolbox.input(
        kind        = 'gridpoint',
        local       = iso1wetbt,
        hostname    = 'sotrtm35-sidev.meteo.fr',
        username    = 'vernaym',
        tube        = 'ftp',
        remote      = f'{extraction_dir}/PAA_ALTITUDE_ISO_WETBT_EURAT01_{datebegin.ymdh}_{dateend.ymdh}.nc',
        unknown     = True,
    )

    ds0 = xr.open_dataset(iso0)
    ds1 = xr.open_dataset(iso1wetbt)
    ds = xr.merge([ds0.rename({'h': 'iso0'}), ds1.rename({'h': 'iso1wetbt'})])
    return ds.squeeze()


def clean():
    import glob
    for f in glob.glob('*.nc'):
        os.remove(f)


def plot_humidification(dataset, list_aspects, title, filename):

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

    finalise_fig(fig, ax, title, filename)


def finalise_fig(fig, ax, title, filename):
    ax.xaxis.set_major_locator(mdates.DayLocator())
    ax.xaxis.set_major_formatter(mdates.DateFormatter('%d. %b'))
    ax.xaxis.set_minor_locator(mdates.HourLocator([12]))
    ax.xaxis.set_minor_formatter(mdates.DateFormatter('%H:00'))
    ax.grid()
    ax.set_title(title)

    fig.savefig(filename, format='pdf')


def plot_meteogram(s2m, arpege, title, filename, elevation=1800, aspect=-1):

    tmp = s2m.where((s2m.ZS == elevation) & (s2m.aspect == aspect), drop=True).squeeze()
    precipitation = (tmp.Rainf + tmp.Snowf) * 3600

    fig, ax = plt.subplots(figsize=(20, 6))
    a1, = arpege.iso0.plot(color='k', ax=ax, label='Iso-0°C ARPEGE')
    a2, = arpege.iso1wetbt.plot(color='r', ax=ax, label='Iso-1°C wet-bulb temperature ARPEGE')

    ymin = min([arpege[var].min() for var in list(arpege.keys())])
    ymax = max([arpege[var].max() for var in list(arpege.keys())])
    ax.set_ylim([ymin - 200, ymax + 200])
    ax.set_ylabel("Altitude (m)")

    ax2 = ax.twinx()
    color = 'b'
    for i in range(len(tmp.time)):
        if i == 0:
            b0 = ax2.bar(tmp.time.data[i], precipitation.data[i], color=color, width=0.03, alpha=0.5,
                    label=f"Précipitations S2M {elevation}m")
        else:
            ax2.bar(tmp.time.data[i], precipitation.data[i], color=color, width=0.03, alpha=0.5)
    ax2.set_ylabel("Précipitations (mm)", color=color)
    ax2.tick_params(axis='y', labelcolor=color)
    ax2.set_ylim([0, max(precipitation.max() * 1.2, 1)])

    # Put a common legend below current axis
    # Shrink current axis's height by 10% on the bottom
    box = ax.get_position()
    ax.set_position([box.x0, box.y0 + box.height * 0.1,
                     box.width, box.height * 0.9])
    labels = [a1, a2, b0]
    ax.legend(labels, [label.get_label() for label in labels], loc= 'upper center', bbox_to_anchor=(0.5, -0.05),
            fancybox=True, shadow=True, ncol=3, fontsize=12)

    finalise_fig(fig, ax, title, filename)


def humidification(args):
    get_s2m(args.datebegin, args.dateend, args.xpid, args.geometry, kind='SnowpackSimulation')
    massifs_infos = infomassifs()
    ds = xr.open_mfdataset('PRO*.nc', engine='cen')
    ds = ds.sel(time=slice(args.datebegin.strftime('%Y-%m-%d %H'), args.dateend.strftime('%Y-%m-%d %H')))
    ds = ds.compute()
    for massif in args.massif:
        massif_name = massifs_infos.getMassifName(massif)
        for elevation in args.elevation:
            for slope in args.slope:
                # ds = xr.open_mfdataset('PRO*.nc', engine='cen', join='right', concat_dim='time', combine='nested')
                # ds = ds.drop_duplicates('time').sel(time=slice(args.datebegin.ymdh, args.dateend.ymdh)).compute()
                reduced_ds = ds.where((ds.massif_num == massif) & (ds.ZS == elevation) & (ds.slope == slope) &
                        (ds.aspect.isin(args.aspect)), drop=True)
                title = f'Massif: {massif_name} - Elevation: {elevation}m - slope: {slope}°'
                savename = f'Epaisseur_de_neige_humide_{massif}_{elevation}_{slope}_{args.datebegin.ymdh}_' \
                    f'{args.dateend.ymdh}.pdf'
                plot_humidification(reduced_ds, args.aspect, title, savename)


def meteogram(args):
    get_s2m(args.datebegin, args.dateend, args.xpid, args.geometry, kind='MeteorologicalForcing')
    massifs_infos = infomassifs()
    for massif in args.massif:
        massif_name = massifs_infos.getMassifName(massif)

        iso0 = get_arpege(args.datebegin, args.dateend, massif_name)

        # Open S2M data
        ds = xr.open_mfdataset('FORCING*.nc', engine='cen')
        ds = ds.sel(time=slice(args.datebegin.strftime('%Y-%m-%d %H'), args.dateend.strftime('%Y-%m-%d %H')))
        ds = ds.compute()
        # ds = xr.open_mfdataset('PRO*.nc', engine='cen', join='right', concat_dim='time', combine='nested')
        # ds = ds.drop_duplicates('time').sel(time=slice(args.datebegin.ymdh, args.dateend.ymdh)).compute()
        reduced_ds = ds.where(ds.massif_number == massif, drop=True)

        title = f'Massif: {massif_name}'
        savename = f'Meteogram_{massif}_{args.datebegin.ymdh}_{args.dateend.ymdh}.pdf'
        plot_meteogram(reduced_ds, iso0, title, savename)


def profiles(args):
    get_s2m(args.datebegin, args.dateend, args.xpid, args.geometry, kind='SnowpackSimulation')
    massifs_infos = infomassifs()
    ds = xr.open_mfdataset('PRO*.nc', engine='cen')
    ds = ds.sel(time=args.time)
    ds = ds.compute()
    for massif in args.massif:
        massif_name = massifs_infos.getMassifName(massif)

        if args.elevation is not None:
            # Plot profiles for all aspects for a given elevation
            for elevation in args.elevation:
                reduced_ds = ds.where((ds.massif_num == massif) & (ds.ZS == elevation) & (ds.slope.isin([0, 40])),
                        drop=True)
                title = f'Massif: {massif_name} - Elevation: {elevation}m'
                savename = f'Profiles_{massif}_{elevation}_{args.time.ymdh}.pdf'
                plot_profiles_elevation(reduced_ds, title, savename)

        elif args.aspect is not None:
            # Plot profiles for all elevations for a given aspect
            for aspect in args.aspect:
                if aspect == -1:
                    slope = 0
                else:
                    slope = 40
                reduced_ds = ds.where((ds.massif_num == massif) & (ds.aspect == aspect) & (ds.slope == slope),
                        drop=True)
                title = f'Massif: {massif_name} - Aspect: {aspect}m'
                savename = f'Profiles_{massif}_{aspect}_{args.time.ymdh}.pdf'
                plot_profiles_aspect(reduced_ds, title, savename)


def plot_profiles_elevation(ds, title, savename):

    aspects = {315: 'nord-ouest', 0: 'nord', 45: 'nord-est', 270: 'ouest', -1: 'plat',
            90: 'est', 225: 'sud-ouest', 180: 'sud', 135: 'sud-est'}
    fig, axes = plt.subplots(3, 3, figsize=(16, 12), layout='constrained')
    i = 0
    j = 0
    htnmax = ds.DSN_T_ISBA.max()
    for aspect, orientation in aspects.items():
        tmp = ds.where((ds.aspect == aspect), drop=True)
        tmp.SNOWTEMP.data = tmp.SNOWTEMP.data - 273.16
        tmp = tmp.squeeze().fillna(0)
        ax = axes[i, j]
        plot_profile(tmp, ax, htnmax)
        ax.set_title(orientation, y=1.08, fontweight="bold")
        ax.grid()
        if j == 0:
            ax.set_ylabel('Hauteur de neige (m)')
        j = j + 1
        if j == 3:
            j = 0
            i = i + 1

    fig.savefig(savename, format='pdf')


def plot_profiles_aspect(ds, title, savename):

    fig, axes = plt.subplots(2, 5, figsize=(16, 10), layout='constrained')
    i = 1
    j = 0
    htnmax = ds.DSN_T_ISBA.max()
    for elevation in range(1200, 4200, 300):
        if elevation in ds.ZS.data:
            tmp = ds.where((ds.ZS == elevation), drop=True)
            tmp.SNOWTEMP.data = tmp.SNOWTEMP.data - 273.16
            tmp = tmp.squeeze().fillna(0)
            ax = axes[i, j]
            plot_profile(tmp, ax, htnmax)
            ax.set_title(elevation, y=1.08, fontweight="bold")
            ax.grid()
        if j == 0:
            ax.set_ylabel('Hauteur de neige (m)')
        j = j + 1
        if j == 5:
            j = 0
            i = i - 1

    fig.savefig(savename, format='pdf')


def plot_profile(ds, ax, ylimit):
    ax2 = ax.twiny()
    dateProfil(ax, ax2, ds.SNOWTEMP.data, ds.SNOWDZ.data, value_grain=ds.SNOWTYPE.data,
            value_ram=ds.SNOWRAM.data, legend='Temperature (°C)', xlimit=(-30, 0), ylimit=ylimit * 1.1)
    ax2.set_xlabel('RAM (kg/m²)', loc='right')
    ax2.set_xlim(60, 0)


if __name__ == '__main__':
    args = parse_command_line()

    if args.humidification:
        humidification(args)

    if args.meteogram:
        meteogram(args)

    if args.profiles:
        profiles(args)

    clean()
