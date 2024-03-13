#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''
Created on 8 march 2024

@author: Vernay

Extraction / Backup of commonly used files with Vortex
'''

import os
import vortex
import cen  # Import necessary to load vortex CEN-specific ressourees
from vortex import toolbox
from bronx.stdtypes.date import Date

import footprints

toolbox.active_now = True

t = vortex.ticket()

if 'MTOOLDIR' not in os.environ.keys():
    # Set a default MTOOLDIR for local Vortex cache
    os.environ['MTOOLDIR'] = os.path.join(os.environ['HOME'], 'cache')
    if not os.path.exists(os.environ['MTOOLDIR']):
        os.makedirs(os.environ['MTOOLDIR'])


def get_pro(datebegin, dateend, xpid, geometry, namespace='vortex.multi.fr', filename='PRO.nc',
            members=None, vapp='edelweiss', abspath=None):

    tbpro = toolbox.input(
        local          = f'mb[member]/{filename}' if members is not None else filename,
        experiment     = xpid,
        geometry       = geometry,
        datebegin      = Date(datebegin),
        dateend        = Date(dateend),
        date           = Date(dateend),
        nativefmt      = 'netcdf',
        kind           = 'SnowpackSimulation',
        vapp           = vapp,
        vconf          = '[geometry:tag]',
        model          = 'surfex',
        namespace      = namespace,
        namebuild      = 'flat@cen',
        block          = 'pro',
        member         = None if members is None else footprints.util.rangex(0, members - 1),
        fatal          = True,
    ),
    print(t.prompt, 'PRO input =', tbpro)
    print()


def put_pro(datebegin, dateend, xpid, geometry, namespace='vortex.multi.fr', filename='PRO.nc',
            members=None, vapp='edelweiss', abspath=None):

    tbpro = toolbox.output(
        local          = f'mb[member]/{filename}' if members is not None else filename,
        experiment     = xpid,
        geometry       = geometry,
        datebegin      = Date(datebegin),
        dateend        = Date(dateend),
        date           = Date(dateend),
        nativefmt      = 'netcdf',
        kind           = 'SnowpackSimulation',
        vapp           = vapp,
        vconf          = '[geometry:tag]',
        model          = 'surfex',
        namespace      = namespace,
        namebuild      = 'flat@cen',
        block          = 'pro',
        member         = None if members is None else footprints.util.rangex(0, members - 1),
        fatal          = True,
    ),
    print(t.prompt, 'PRO output =', tbpro)
    print()


def get_diag(datebegin, dateend, xpid, geometry, namespace='vortex.multi.fr', filename='DIAG.nc',
             members=None, vapp='edelweiss', block='', abspath=None):

    tbdiag = toolbox.input(
        local          = f'mb[member]/{filename}' if members is not None else filename,
        experiment     = xpid,
        geometry       = geometry,
        # CEN's convention is to name period footprints 'datebegin' and 'dateend'
        # but for SURFEX diagnostics, we use objects from :
        # common.data.diagnostics.SurfexPeriodDiagnostics
        begindate      = Date(datebegin),
        enddate        = Date(dateend),
        date           = Date(dateend),
        scope          = 'SesonalSnowCoverDiagnostic',
        nativefmt      = 'netcdf',
        kind           = 'diagnostics',
        vapp           = vapp,
        vconf          = '[geometry:tag]',
        model          = 'surfex',
        namespace      = namespace,
        namebuild      = 'flat@cen',
        block          = f'diag/{block}',
        member         = None if members is None else footprints.util.rangex(0, members - 1),
        fatal          = True,
    ),
    print(t.prompt, 'DIAG input =', tbdiag)
    print()


def put_diag(datebegin, dateend, xpid, geometry, namespace='vortex.multi.fr', filename='DIAG.nc',
             members=None, vapp='edelweiss', block='', abspath=None):

    tbdiag = toolbox.output(
        local          = f'mb[member]/{filename}' if members is not None else filename,
        experiment     = xpid,
        geometry       = geometry,
        # CEN's convention is to name period footprints 'datebegin' and 'dateend'
        # but for SURFEX diagnostics, we use objects from :
        # common.data.diagnostics.SurfexPeriodDiagnostics
        begindate      = Date(datebegin),
        enddate        = Date(dateend),
        date           = Date(dateend),
        scope          = 'SesonalSnowCoverDiagnostic',
        nativefmt      = 'netcdf',
        kind           = 'diagnostics',
        vapp           = vapp,
        vconf          = '[geometry:tag]',
        model          = 'surfex',
        namespace      = namespace,
        namebuild      = 'flat@cen',
        block          = f'diag/{block}',
        member         = None if members is None else footprints.util.rangex(0, members - 1),
        fatal          = True,
    ),
    print(t.prompt, 'DIAG ouput =', tbdiag)
    print()


def get_forcing(datebegin, dateend, xpid, geometry, namespace='vortex.multi.fr', filename='FORCING.nc', model='safran',
                members=None, vapp='edelweiss', block='', abspath=None, source_app=None, source_conf=None):

    forcing = toolbox.input(
        role        = 'Forcing file',
        kind        = 'MeteorologicalForcing',
        vapp        = vapp,
        vconf       = '[geometry:tag]',
        cutoff      = 'assimilation',
        source_app  = source_app,
        source_conf = source_conf,
        local       = f'mb[member]/{filename}' if members is not None else filename,
        experiment  = xpid,
        geometry    = geometry,
        nativefmt   = 'netcdf',
        namebuild   = 'flat@cen',
        model       = model,
        date        = dateend,
        datebegin   = datebegin,
        dateend     = dateend,
        namespace   = namespace,
        member      = None if members is None else footprints.util.rangex(0, members - 1),
        block       = 'meteo',
    ),
    print(t.prompt, 'FORCING input =', forcing)
    print()


def put_forcing(datebegin, dateend, xpid, geometry, namespace='vortex.multi.fr', filename='FORCING.nc', model='safran',
                members=None, vapp='edelweiss', block='', abspath=None, source_app=None, source_conf=None):

    forcing = toolbox.output(
        role        = 'Forcing file',
        kind        = 'MeteorologicalForcing',
        vapp        = vapp,
        vconf       = '[geometry:tag]',
        cutoff      = 'assimilation',
        source_app  = source_app,
        source_conf = source_conf,
        local       = f'mb[member]/{filename}' if members is not None else filename,
        experiment  = xpid,
        geometry    = geometry,
        nativefmt   = 'netcdf',
        namebuild   = 'flat@cen',
        model       = model,
        date        = dateend,
        datebegin   = datebegin,
        dateend     = dateend,
        namespace   = namespace,
        member      = None if members is None else footprints.util.rangex(0, members - 1),
        block       = 'meteo',
    ),
    print(t.prompt, 'FORCING input =', forcing)
    print()


def get_precipitation(datebegin, dateend, xpid, geometry, namespace='vortex.multi.fr', filename='PRECIPITATION.nc',
                      members=None, vapp='edelweiss', block='', abspath=None, source_app=None, source_conf=None,
                      model=None):

    precipitation = toolbox.input(
        role        = 'precipitation analysis',
        kind        = 'precipitation',
        vapp        = vapp,
        vconf       = '[geometry:tag]',
        source_app  = source_app,
        source_conf = source_conf,
        cutoff      = 'assimilation',
        local       = f'mb[member]/{filename}' if members is not None else filename,
        experiment  = xpid,
        geometry    = geometry,
        nativefmt   = 'netcdf',
        namebuild   = 'flat@cen',
        model       = model,
        date        = dateend,
        datebegin   = datebegin,
        dateend     = dateend,
        namespace   = namespace,
        member      = None if members is None else footprints.util.rangex(0, members - 1),
        block       = 'meteo',
    )
    print(t.prompt, 'precipitation =', precipitation)
    print()


def put_precipitation(datebegin, dateend, xpid, geometry, namespace='vortex.multi.fr', filename='PRECIPITATION.nc',
                      members=None, vapp='edelweiss', block='', abspath=None, source_app=None, source_conf=None,
                      model=None):

    precipitation = toolbox.output(
        role        = 'precipitation analysis',
        kind        = 'precipitation',
        vapp        = vapp,
        vconf       = '[geometry:tag]',
        source_app  = source_app,
        source_conf = source_conf,
        cutoff      = 'assimilation',
        local       = f'mb[member]/{filename}' if members is not None else filename,
        experiment  = xpid,
        geometry    = geometry,
        nativefmt   = 'netcdf',
        namebuild   = 'flat@cen',
        model       = model,
        date        = dateend,
        datebegin   = datebegin,
        dateend     = dateend,
        namespace   = namespace,
        member      = None if members is None else footprints.util.rangex(0, members - 1),
        block       = 'meteo',
    )
    print(t.prompt, 'precipitation =', precipitation)
    print()


def get_wind(datebegin, dateend, xpid, geometry, namespace='vortex.multi.fr', filename='WIND.nc',
             members=None, vapp='edelweiss', block='', abspath=None, source_app='arome', source_conf='devine',
             model='devine'):

    wind = toolbox.input(
        role        = 'Wind',
        kind        = 'Wind',
        vapp        = vapp,
        vconf       = '[geometry:tag]',
        source_app  = source_app,
        source_conf = source_conf,
        cutoff      = 'assimilation',
        local       = f'mb[member]/{filename}' if members is not None else filename,
        experiment  = xpid,
        geometry    = geometry,
        nativefmt   = 'netcdf',
        namebuild   = 'flat@cen',
        model       = model,
        date        = dateend,
        datebegin   = datebegin,
        dateend     = dateend,
        namespace   = namespace,
        member      = None if members is None else footprints.util.rangex(0, members - 1),
        block       = 'meteo',
    )
    print(t.prompt, 'Wind =', wind)
    print()


def put_wind(datebegin, dateend, xpid, geometry, namespace='vortex.multi.fr', filename='WIND.nc',
             members=None, vapp='edelweiss', block='', abspath=None, source_app='arome', source_conf='devine',
             model='devine'):

    wind = toolbox.output(
        role        = 'Wind',
        kind        = 'Wind',
        vapp        = vapp,
        vconf       = '[geometry:tag]',
        source_app  = source_app,
        source_conf = source_conf,
        cutoff      = 'assimilation',
        local       = f'mb[member]/{filename}' if members is not None else filename,
        experiment  = xpid,
        geometry    = geometry,
        nativefmt   = 'netcdf',
        namebuild   = 'flat@cen',
        model       = model,
        date        = dateend,
        datebegin   = datebegin,
        dateend     = dateend,
        namespace   = namespace,
        member      = None if members is None else footprints.util.rangex(0, members - 1),
        block       = 'meteo',
    )
    print(t.prompt, 'Wind =', wind)
    print()
