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
        vapp           = 'edelweiss',
        vconf          = '[geometry:tag]',
        model          = 'surfex',
        namespace      = namespace,
        namebuild      = 'flat@cen',
        block          = 'pro',
        member         = None if members is None else footprints.util.rangex(1, members),
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
        member         = None if members is None else footprints.util.rangex(1, members),
        fatal          = True,
    ),
    print(t.prompt, 'PRO output =', tbpro)
    print()


def put_diag(datebegin, dateend, xpid, geometry, namespace='vortex.multi.fr', filename='DIAG.nc',
             members=None, vapp='edelweiss', block='', abspath=None):

    tbdiag = toolbox.output(
        local          = f'mb[member]/{filename}' if members is not None else filename,
        experiment     = xpid,
        geometry       = geometry,
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
        member         = None if members is None else footprints.util.rangex(1, members),
        fatal          = True,
    ),
    print(t.prompt, 'DIAG ouput =', tbdiag)
    print()


def get_diag(datebegin, dateend, xpid, geometry, namespace='vortex.multi.fr', filename='DIAG.nc',
             members=None, vapp='edelweiss', block='', abspath=None):

    tbdiag = toolbox.input(
        local          = f'mb[member]/{filename}' if members is not None else filename,
        experiment     = xpid,
        geometry       = geometry,
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
        member         = None if members is None else footprints.util.rangex(1, members),
        fatal          = True,
    ),
    print(t.prompt, 'DIAG input =', tbdiag)
    print()
