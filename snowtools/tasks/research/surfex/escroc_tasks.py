# -*- coding: utf-8 -*-
'''
Created on 7 nov. 2017

@author: lafaysse
'''

from bronx.stdtypes.date import Date
from cen.layout.nodes import S2MTaskMixIn
import footprints
from vortex import toolbox
from vortex.layout.nodes import Driver, Task

from snowtools.utils.dates import get_list_dates_files, get_dic_dateend


def setup(t, **kw):
    return Driver(
        tag='Surfex_Parallel',
        ticket=t,
        nodes=[
            Escroc_Vortex_Task(tag='Escroc_Vortex_Task', ticket=t, **kw),
        ],
        options=kw
    )


class Escroc_Vortex_Task(Task, S2MTaskMixIn):
    '''

    '''

    def process(self):

        t = self.ticket

        if not hasattr(self.conf, "genv"):
            self.conf.genv = 'uenv:cen.10@CONST_CEN'


        # Definition of geometries, safran xpid/block and list of dates from S2MTaskMixIn methods
        list_geometry = self.get_list_geometry(meteo=self.conf.meteo)
        source_safran, block_safran = self.get_source_safran(meteo=self.conf.meteo)
        list_dates_begin_forc, list_dates_end_forc, list_dates_begin_pro, list_dates_end_pro = \
            get_list_dates_files(self.conf.datebegin, self.conf.dateend, self.conf.duration)
        dict_dates_end_forc = get_dic_dateend(list_dates_begin_forc, list_dates_end_forc)
        dict_dates_end_pro = get_dic_dateend(list_dates_begin_pro, list_dates_end_pro)
        dict_source_app_safran, dict_source_conf_safran = self.get_safran_sources(list_dates_begin_forc)

        startmember = int(self.conf.startmember) if hasattr(self.conf, "startmember") else 1
        members = list(range(startmember, int(self.conf.nmembers) + startmember)) \
            if hasattr(self.conf, "nmembers") else list(range(1, 36))

        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            # Try to find a forcing covering the full simulation period
            tb01 = toolbox.input(
                role           = 'Forcing',
                kind           = 'MeteorologicalForcing',
                vapp           = self.conf.meteo,
                vconf          = '[geometry:area]' if source_safran == 'safran' else '[geometry:tag]',
                source_app     = dict_source_app_safran if source_safran == 'safran' else None,
                source_conf    = dict_source_conf_safran if source_safran == 'safran' else None,
                cutoff         = 'assimilation',
                local          = '[geometry::tag]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc' \
                                 if len(list_geometry) > 1 else 'FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                experiment     = self.conf.forcingid,
                block          = block_safran,
                geometry       = list_geometry,
                nativefmt      = 'netcdf',
                model          = 'safran',
                datebegin      = self.conf.datebegin,
                dateend        = self.conf.dateend,
                intent         = 'inout',
                namespace      = 'vortex.multi.fr',
                namebuild      = 'flat@cen',
                fatal          = False,
            ),

            if tb01[0]:
                oneforcing = True
            else:
                oneforcing = False
                # Look for yearly forcing files
                self.sh.title('Toolbox input tb01')
                tb01 = toolbox.input(
                    role           = 'Forcing',
                    kind           = 'MeteorologicalForcing',
                    vapp           = self.conf.meteo,
                    vconf          = '[geometry:area]' if source_safran == 'safran' else '[geometry:tag]',
                    source_app     = dict_source_app_safran if source_safran == 'safran' else None,
                    source_conf    = dict_source_conf_safran if source_safran == 'safran' else None,
                    cutoff         = 'assimilation',
                    local          = '[geometry::tag]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc' \
                                     if len(list_geometry) > 1 else 'FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = self.conf.forcingid,
                    block          = block_safran,
                    geometry       = list_geometry,
                    nativefmt      = 'netcdf',
                    model          = 'safran',
                    datebegin      = list_dates_begin_forc,
                    dateend        = dict_dates_end_forc,
                    intent         = 'inout',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                ),

                print(t.prompt, 'tb01 =', tb01)
                print()

            # Look for a PGD file if already available for this xpid and geometry
            self.sh.title('Toolbox input tb02')
            tb02 = toolbox.input(
                role           = 'SurfexClim',
                kind           = 'pgdnc',
                nativefmt      = 'netcdf',
                local          = 'PGD.nc',
                experiment     = self.conf.xpid,
                geometry       = self.conf.geometry,
                model          = 'surfex',
                namespace      = 'vortex.multi.fr',
                namebuild      = 'flat@cen',
                block          = 'pgd',
                fatal          = False,
            ),
            print(t.prompt, 'tb02_a =', tb02)
            print()

            # Alternate : look for a PGD file if already available for this geometry with the "spinup" xpid
            # If not available, do not fail because the PGD file will be automatically built.
            self.sh.title('Toolbox input tb02a')
            tb02_a = toolbox.input(
                alternate      = 'SurfexClim',
                kind           = 'pgdnc',
                nativefmt      = 'netcdf',
                local          = 'PGD.nc',
                experiment     = 'spinup@' + t.env.getvar("USER"),
                geometry       = self.conf.geometry,
                model          = 'surfex',
                namespace      = 'vortex.multi.fr',
                namebuild      = 'flat@cen',
                block          = 'pgd',
                fatal          = False,
            ),
            print(t.prompt, 'tb02_a =', tb02_a)
            print()

            self.sh.title('Toolbox input tb03')
            tb03 = toolbox.input(
                role           = 'SnowpackInit',
                local          = 'PREP.nc',
                experiment     = self.conf.xpid,
                geometry       = self.conf.geometry,
                date           = self.conf.datespinup,
                intent         = 'inout',
                nativefmt      = 'netcdf',
                kind           = 'PREP',
                model          = 'surfex',
                namespace      = 'vortex.multi.fr',
                namebuild      = 'flat@cen',
                block          = 'prep',
                fatal          = False,
            ),
            print(t.prompt, 'tb03 =', tb03)
            print()

            self.sh.title('Toolbox input tb03_s')
            tb03_s = toolbox.input(
                alternate      = 'SnowpackInit',
                local          = 'PREP.nc',
                experiment     = 'spinup@lafaysse',
                geometry       = self.conf.geometry,
                date           = self.conf.datespinup,
                intent         = 'inout',
                nativefmt      = 'netcdf',
                kind           = 'PREP',
                model          = 'surfex',
                namespace      = 'vortex.multi.fr',
                namebuild      = 'flat@cen',
                block          = 'prep',
                fatal          = True,
            ),
            print(t.prompt, 'tb03_s =', tb03_s)
            print()

            self.sh.title('Toolbox input tb03b')
            tb03b = toolbox.input(
                role           = 'Surfex cover parameters',
                kind           = 'coverparams',
                nativefmt      = 'bin',
                local          = 'ecoclimapI_covers_param.bin',
                geometry       = self.conf.geometry,
                genv           = self.conf.genv,
                source         = 'ecoclimap1',
                model          = 'surfex',
            ),
            print(t.prompt, 'tb03b =', tb03b)
            print()

            self.sh.title('Toolbox input tb03c')
            tb03c = toolbox.input(
                role           = 'Surfex cover parameters',
                kind           = 'coverparams',
                nativefmt      = 'bin',
                local          = 'ecoclimapII_eu_covers_param.bin',
                geometry       = self.conf.geometry,
                genv           = self.conf.genv,
                source         = 'ecoclimap2',
                model          = 'surfex',
            ),
            print(t.prompt, 'tb03c =', tb03c)
            print()

            self.sh.title('Toolbox input tb04')
            tb04 = toolbox.input(
                role            = 'Parameters for F06 metamorphism',
                kind            = 'ssa_params',
                genv            = self.conf.genv,
                nativefmt       = 'netcdf',
                local           = 'drdt_bst_fit_60.nc',
                model           = 'surfex',
            )
            print(t.prompt, 'tb04 =', tb04)
            print()

            self.sh.title('Toolbox input tb05')
            if hasattr(self.conf, "namelist"):
                tb05 = toolbox.input(
                    role            = 'Nam_surfex',
                    remote          = self.conf.namelist,
                    kind            = 'namelist',
                    model           = 'surfex',
                    local           = 'OPTIONS.nam',
                )
            else:
                tb05 = toolbox.input(
                    role            = 'Nam_surfex',
                    source          = 'OPTIONS_default.nam',
                    genv            = self.conf.genv,
                    kind            = 'namelist',
                    model           = 'surfex',
                    local           = 'OPTIONS.nam',
                )

            print(t.prompt, 'tb05 =', tb05)
            print()

            if hasattr(self.conf, "exesurfex"):
                self.sh.title('Toolbox executable tb06= tbx1')
                tb06 = tbx3 = toolbox.executable(
                    role           = 'Binary',
                    kind           = 'offline',
                    local          = 'OFFLINE',
                    model          = 'surfex',
                    remote         = self.conf.exesurfex + "/OFFLINE"
                )

                print(t.prompt, 'tb06 =', tb06)
                print()

                if not (tb02[0] or tb02_a[0]):

                    self.sh.title('Toolbox executable tb07= tbx2')
                    tb07 = tbx1 = toolbox.executable(
                        role           = 'Binary',
                        kind           = 'buildpgd',
                        local          = 'PGD',
                        model          = 'surfex',
                        remote         = self.conf.exesurfex + "/PGD"
                    )

                    print(t.prompt, 'tb07 =', tb07)
                    print()

                if not (tb03[0] or tb03_s[0]):

                    self.sh.title('Toolbox executable tb08= tbx3')
                    tb08 = tbx2 = toolbox.executable(
                        role           = 'Binary',
                        kind           = 'prep',
                        local          = 'PREP',
                        model          = 'surfex',
                        remote         = self.conf.exesurfex + "/PREP"
                    )

                    print(t.prompt, 'tb08 =', tb08)
                    print()

            else:

                self.sh.title('Toolbox executable tb06= tbx1')
                tb06 = tbx3 = toolbox.executable(
                    role           = 'Binary',
                    kind           = 'offline',
                    local          = 'OFFLINE',
                    model          = 'surfex',
                    genv           = self.conf.genv,
                    gvar           = 'master_surfex_offline_nompi',
                )

                print(t.prompt, 'tb06 =', tb06)
                print()

                if not (tb02[0] or tb02_a[0]):

                    self.sh.title('Toolbox executable tb07= tbx2')
                    tb07 = tbx1 = toolbox.executable(
                        role           = 'Binary',
                        kind           = 'buildpgd',
                        local          = 'PGD',
                        model          = 'surfex',
                        genv           = self.conf.genv,
                        gvar           = 'master_pgd_nompi',
                    )

                    print(t.prompt, 'tb07 =', tb07)
                    print()

                if not (tb03[0] or tb03_s[0]):

                    self.sh.title('Toolbox executable tb08= tbx3')
                    tb08 = tbx2 = toolbox.executable(
                        role           = 'Binary',
                        kind           = 'prep',
                        local          = 'PREP',
                        model          = 'surfex',
                        genv           = self.conf.genv,
                        gvar           = 'master_prep_nompi',

                    )

                    print(t.prompt, 'tb08 =', tb08)
                    print()

        if 'compute' in self.steps:

            firstforcing = 'FORCING_' + list_dates_begin_forc[0].strftime("%Y%m%d%H") + "_" + \
                list_dates_end_forc[0].strftime("%Y%m%d%H") + ".nc"
            self.sh.title('Toolbox algo tb09a')
            tb09a = tbalgo1 = toolbox.algo(
                kind         = 'surfex_preprocess',
                datebegin    = self.conf.datebegin,
                dateend      = self.conf.dateend,
                forcingname  = firstforcing,
            )
            print(t.prompt, 'tb09a =', tb09a)
            print()
            tb09a.run()

            # Take care : PGD parallelization will be available in v8.1 --> nproc and ntasks will have to be set to 40
            if not (tb02[0] or tb02_a[0]):
                self.sh.title('Toolbox algo tb09 = PGD')
                tb09 = tbalgo2 = toolbox.algo(
                    kind         = 'pgd_from_forcing',
                    forcingname  = firstforcing,
                )
                print(t.prompt, 'tb09 =', tb09)
                print()
                self.component_runner(tbalgo2, tbx1, mpiopts=dict(nnodes=1, nprocs=1, ntasks=1))

            # Take care : PREP parallelization will be available in v8.1 --> nproc and ntasks will have to be set to 40
            if not (tb03[0] or tb03_s[0]):
                self.sh.title('Toolbox algo tb09 = PREP')
                tb10 = tbalgo3 = toolbox.algo(
                    engine         = 'parallel',
                )
                print(t.prompt, 'tb10 =', tb10)
                print()
                self.component_runner(tbalgo3, tbx2, mpiopts=dict(nnodes=1, nprocs=1, ntasks=1))

            self.sh.title('Toolbox algo tb11 = OFFLINE')

            if self.conf.subensemble == 'Crocus':
                ntasks = 1
            else:
                ntasks = 40 * int(self.conf.nnodes)

            tb11 = tbalgo4 = toolbox.algo(
                kind           = "escroc",
                engine         = 's2m',
                verbose        = True,
                binary         = 'OFFLINE',
                datebegin      = self.conf.datebegin,
                dateend        = self.conf.dateend,
                dateinit       = Date(self.conf.datespinup),
                threshold      = self.conf.threshold,
                members        = footprints.util.rangex(members),
                geometry_in    = [self.conf.geometry.tag],
                geometry_out   = self.conf.geometry.tag,
                subensemble    = self.conf.subensemble if hasattr(self.conf, "subensemble")  else "E2",
                ntasks         = ntasks,
                reprod_info    = self.get_reprod_info,
            )
            print(t.prompt, 'tb11 =', tb11)
            print()
            self.component_runner(tbalgo4, tbx3)

        if 'backup' in self.steps:
            pass

        if 'late-backup' in self.steps:

            self.sh.title('Toolbox output tb19')
            tb19 = toolbox.output(
                local          = 'mb[member%04d]/PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                experiment     = self.conf.xpid,
                geometry       = self.conf.geometry,
                datebegin      = list_dates_begin_pro,
                dateend        = dict_dates_end_pro,
                member         = members,
                nativefmt      = 'netcdf',
                kind           = 'SnowpackSimulation',
                model          = 'surfex',
                namespace      = 'vortex.multi.fr',
                namebuild      = 'flat@cen',
                block          = 'pro',
            ),
            print(t.prompt, 'tb19 =', tb19)
            print()

            if hasattr(self.conf, "writesx"):
                if self.conf.writesx:
                    self.sh.title('Toolbox output tb19bis')
                    tb19bis = toolbox.output(
                        local       = 'mb[member%04d]/PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                        experiment  = self.conf.xpid,
                        geometry    = self.conf.geometry,
                        datebegin   = list_dates_begin_pro,
                        dateend     = dict_dates_end_pro,
                        member      = members,
                        nativefmt   = 'netcdf',
                        kind        = 'SnowpackSimulation',
                        model       = 'surfex',
                        namespace   = 'vortex.archive.fr',
                        storage     = 'sxcen.cnrm.meteo.fr',
                        enforcesync = True, # to forbid asynchronous transfers and not saturate sxcen
                        namebuild   = 'flat@cen',
                        block       = 'pro',
                    ),
                    print(t.prompt, 'tb19bis =', tb19bis)
                    print()

            self.sh.title('Toolbox output tb20')
            tb20 = toolbox.output(
                local          = 'mb[member%04d]/PREP_[date:ymdh].nc',
                role           = 'SnowpackInit',
                experiment     = self.conf.xpid,
                geometry       = self.conf.geometry,
                date           = list_dates_end_pro,
                member         = members,
                nativefmt      = 'netcdf',
                kind           = 'PREP',
                model          = 'surfex',
                namespace      = 'vortex.multi.fr',
                namebuild      = 'flat@cen',
                block          = 'prep',
            ),
            print(t.prompt, 'tb20 =', tb20)
            print()

# The following condition does not work. --> Ask leffe how to do
#                 if not (tb02[0] or tb02_a[0]):
            tb21 = toolbox.output(
                role           = 'SurfexClim',
                kind           = 'pgdnc',
                nativefmt      = 'netcdf',
                local          = 'PGD.nc',
                experiment     = self.conf.xpid,
                geometry       = self.conf.geometry,
                model          = 'surfex',
                namespace      = 'vortex.multi.fr',
                namebuild      = 'flat@cen',
                block          = 'pgd',
            ),
            print(t.prompt, 'tb21 =', tb21)
            print()
