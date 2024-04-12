# -*- coding: utf-8 -*-
"""
Created on 7 nov. 2017

@author: lafaysse
"""
import os

from vortex.layout.nodes import Driver, Task
from cen.layout.nodes import S2MTaskMixIn
from vortex import toolbox
from bronx.stdtypes.date import daterange, Date
import footprints
from vortex.algo.components import DelayedAlgoComponentError
from snowtools.utils.dates import get_list_dates_files, get_dic_dateend

def setup(t, **kw):
    return Driver(
        tag='Surfex_Parallel',
        ticket=t,
        nodes=[
            Ensemble_Surfex_Reforecast(tag='Ensemble_Surfex_Reforecast', ticket=t, **kw),
        ],
        options=kw
    )


class Ensemble_Surfex_Reforecast(S2MTaskMixIn, Task):
    """
    S2M reforecast task
    """

    filter_execution_error = S2MTaskMixIn.reforecast_filter_execution_error

    def process(self):

        if not hasattr(self.conf, "genv"):
            self.conf.genv = 'uenv:cen.10@CONST_CEN'
        t = self.ticket

        if 'reforecast_2023' in self.conf.forcingid  or 'reforecast_2023' in self.conf.xpid:
            # Weird availabilities of reforecasts
            listavaildate = []
            for year in range(2000, 2022, 4):
                listavaildate += daterange(Date(year, 3, 2, 6), Date(year+4, 3, 1, 6), 'P5D')
                listavaildate += daterange(Date(year, 3, 4, 6), Date(year+4, 3, 1, 6), 'P5D')

            listrundate = list(set(daterange(self.conf.datebegin, self.conf.dateend)) & set(listavaildate))
            listrundate.sort()
        else:
            listrundate = list(daterange(self.conf.datebegin, self.conf.dateend))

        # Following is used to produce yearly tar archives
        list_dates_begin_forc, list_dates_end_forc, list_dates_begin_pro, list_dates_end_pro = \
            get_list_dates_files(listrundate[0], listrundate[-1], 'yearly')
        dict_dates_end_pro = get_dic_dateend(list_dates_begin_pro, list_dates_end_pro)

        def tar_hook(t, rh):
            # For only one of the resources list, create the tar archive for the whole list of dates
            sh = t.sh
            prefix = rh.container.basename.split('_')[0]  # FORCING et PRO
            if rh.resource.datebegin == list_dates_begin_pro[0] and rh.provider.member == pearpmembers[0]:
                for date_arch_begin, date_arch_end in zip(list_dates_begin_pro, list_dates_end_pro):
                    list_dates_arch = \
                    [mydate for mydate in listrundate if date_arch_begin <= mydate <= date_arch_end]
                    listdir = sum([sh.glob(date_arch.ymdh + '/mb*/' + prefix + '*') for date_arch in list_dates_arch],
                                  [])
                    tarname = "_".join([prefix, date_arch_begin.ymdh, date_arch_end.ymdh]) + '.tar'
                    sh.tar(tarname, *listdir)

        list_geometry = self.get_list_geometry()
        source_safran, block_safran = self.get_source_safran()
        self.conf.previ = True
        pearpmembers, members = self.get_list_members()

        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            if not pearpmembers: 

                self.sh.title('Toolbox input tb01 deterministe')
                tb01b = toolbox.input(
                    role           = 'Forcing',
                    local          = '[datebegin:ymdh]/[geometry::tag]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc' if len(list_geometry) > 1 else '[datebegin:ymdh]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                    vapp           = self.conf.meteo,
                    vconf          = '[geometry:area]',
                    block          = block_safran,
                    source_app     = 'arpege' if source_safran == 'safran' else None,
                    source_conf    = '4dvarfr',
                    experiment     = self.conf.forcingid  if source_safran == 'safran' else self.conf.xpid,
                    geometry       = list_geometry,
                    datebegin      = listrundate,
                    dateend        = '[datebegin]/+PT96H',
                    nativefmt      = 'netcdf',
                    kind           = 'MeteorologicalForcing',
                    namespace      = 'vortex.multi.fr',
                    model          = source_safran,
                    cutoff         = 'production',
                    namebuild      = 'flat@cen',
                ),
                print(t.prompt, 'tb01b =', tb01b)
                print()

            else:

                self.sh.title('Toolbox input tbO1 pearp')
                tb01b = toolbox.input(
                    role           = 'Forcing',
                    local          = '[datebegin:ymdh]/mb[member]/[geometry::tag]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc' if len(list_geometry) > 1 else '[datebegin:ymdh]/mb[member]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                    vapp           = self.conf.meteo,
                    vconf          = '[geometry:area]',
                    block          = block_safran,
                    source_app     = 'arpege' if source_safran == 'safran' else None,
                    source_conf    = 'pearp' if source_safran == 'safran' else None,
                    experiment     = self.conf.forcingid  if source_safran == 'safran' else self.conf.xpid,
                    geometry       = list_geometry,
                    datebegin      = listrundate,
                    dateend        = '[datebegin]/+PT96H',
                    member         = pearpmembers,
                    nativefmt      = 'netcdf',
                    kind           = 'MeteorologicalForcing',
                    namespace      = 'vortex.multi.fr',
                    model          = source_safran,
                    cutoff         = 'production',
                    namebuild      = 'flat@cen',
                    fatal          = False,
                ),
                print(t.prompt, 'tb01b =', tb01b)
                print()

            self.sh.title('Toolbox input tb02')
            tb02 = toolbox.input(
                role           = 'SurfexClim',
                kind           = 'pgdnc',
                nativefmt      = 'netcdf',
                local          = 'PGD.nc',
                geometry       = self.conf.geometry,
                genv           = self.conf.genv,
                gvar           = 'pgd_[geometry::tag]',
                model          = 'surfex',
                fatal          = False,
            ),
            print(t.prompt, 'tb02 =', tb02)
            print()

            self.sh.title('Toolbox input tb02')
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
                local          = '[datevalidity:ymdh]/PREP.nc',
                block          = 'prep',
                experiment     = self.conf.prep_xpid,
                geometry       = self.conf.geometry,
                datevalidity   = listrundate,
                intent         = 'inout',
                nativefmt      = 'netcdf',
                kind           = 'PREP',
                model          = 'surfex',
                namespace      = 'vortex.multi.fr',
                namebuild      = 'flat@cen',
                fatal          = True,
            ),
            print(t.prompt, 'tb03 =', tb03)
            print()

            self.sh.title('Toolbox input tb04')
            tb04 = toolbox.input(
                role           = 'Surfex cover parameters',
                kind           = 'coverparams',
                nativefmt      = 'bin',
                local          = 'ecoclimapI_covers_param.bin',
                geometry       = self.conf.geometry,
                genv           = self.conf.genv,
                source         = 'ecoclimap1',
                model          = 'surfex',
            ),
            print(t.prompt, 'tb04 =', tb04)
            print()

            self.sh.title('Toolbox input tb05')
            tb05 = toolbox.input(
                role           = 'Surfex cover parameters',
                kind           = 'coverparams',
                nativefmt      = 'bin',
                local          = 'ecoclimapII_eu_covers_param.bin',
                geometry       = self.conf.geometry,
                genv           = self.conf.genv,
                source         = 'ecoclimap2',
                model          = 'surfex',
            ),
            print(t.prompt, 'tb05 =', tb05)
            print()

            self.sh.title('Toolbox input tb06')
            tb06 = toolbox.input(
                role            = 'Parameters for F06 metamorphism',
                kind            = 'ssa_params',
                genv           = self.conf.genv,
                nativefmt       = 'netcdf',
                local           = 'drdt_bst_fit_60.nc',
                model           = 'surfex',
            )
            print(t.prompt, 'tb06 =', tb06)
            print()

            self.sh.title('Toolbox input tb07')
            if self.conf.namelist:
                tb07 = toolbox.input(
                    role            = 'Nam_surfex',
                    remote          = self.conf.namelist,
                    kind            = 'namelist',
                    model           = 'surfex',
                    local           = 'OPTIONS.nam',
                )
            else:
                tb07 = toolbox.input(
                    role            = 'Nam_surfex',
                    source          = 'OPTIONS_default.nam',
                    genv           = self.conf.genv,
                    kind            = 'namelist',
                    model           = 'surfex',
                    local           = 'OPTIONS.nam',
                )

            print(t.prompt, 'tb07 =', tb07)
            print()

            self.sh.title('Toolbox executable tb08= tbx1')
            tb08 = tbx1 = toolbox.executable(
                role           = 'Binary',
                kind           = 'offline',
                local          = 'OFFLINE',
                model          = 'surfex',
                genv           = self.conf.genv,
                gvar           = 'master_surfex_offline_nompi',
            )

            print(t.prompt, 'tb08 =', tb08)
            print()

        if 'compute' in self.steps:

            self.sh.title('Toolbox algo tb09 = OFFLINE')

            tb09 = tbalgo1 = toolbox.algo(
                engine         = 's2m',
                kind           = "ensmeteonodet",
                multidates     = True,
                datebegin      = self.conf.datebegin,
                dateend        = self.conf.dateend,
                threshold      = self.conf.threshold,
                members        = footprints.util.rangex(pearpmembers),
                geometry_in    = list_geometry,
                geometry_out   = self.conf.geometry.tag,
                ntasks         = 40,
                daily          = not self.conf.previ,
                reprod_info    = self.get_reprod_info,
            )
            print(t.prompt, 'tb09 =', tb09)
            print()

            self.component_runner(tbalgo1, tbx1)

        if 'backup' in self.steps:
            if pearpmembers:
                if source_safran != 's2m':
                    self.sh.title('Toolbox output tb10')
                    tb10 = toolbox.output(
                        local='[datebegin:ymdh]/mb[member]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                        experiment=self.conf.xpid,
                        block='meteo',
                        geometry=self.conf.geometry,
                        date='[datebegin]',
                        datebegin=listrundate,
                        dateend='[datebegin]/+PT96H',
                        member=pearpmembers,
                        nativefmt='netcdf',
                        kind='MeteorologicalForcing',
                        model='s2m',
                        namespace='vortex.cache.fr',
                        cutoff='production',
                        fatal=False,
                        hook_autohook1=(tar_hook),
                    ),
                    print(t.prompt, 'tb10 =', tb10)
                    print()

                self.sh.title('Toolbox output tb11')
                tb11 = toolbox.output(
                    local='[datebegin:ymdh]/mb[member]/PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment=self.conf.xpid,
                    block='pro',
                    geometry=self.conf.geometry,
                    date='[datebegin]',
                    datebegin=listrundate,
                    dateend='[datebegin]/+PT96H',
                    member=pearpmembers,
                    nativefmt='netcdf',
                    kind='SnowpackSimulation',
                    model='surfex',
                    namespace='vortex.cache.fr',
                    cutoff='production' if self.conf.previ else 'assimilation',
                    fatal=False,
                    hook_autohook1=(tar_hook),
                ),
                print(t.prompt, 'tb11 =', tb11)
                print()

        if 'late-backup' in self.steps:
            print("source_safran")
            print(source_safran)

            if not pearpmembers:

                if source_safran != 's2m':

                    self.sh.title('Toolbox output tb10')
                    tb10 = toolbox.output(
                        local          = '[datebegin:ymdh]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                        experiment     = self.conf.xpid,
                        block          = 'meteo',
                        geometry       = self.conf.geometry,
                        date           = '[datebegin]',
                        datebegin      = listrundate,
                        dateend        = '[datebegin]/+PT96H',
                        nativefmt      = 'netcdf',
                        kind           = 'MeteorologicalForcing',
                        model          = 's2m',
                        namespace      = 'vortex.multi.fr',
                        cutoff         = 'production',
                        member         = 35,
                        fatal          = False
                    ),
                    print(t.prompt, 'tb10 =', tb10)
                    print()

                self.sh.title('Toolbox output tb11')
                tb11 = toolbox.output(
                    local          = '[datebegin:ymdh]/PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = self.conf.xpid,
                    block          = 'pro',
                    geometry       = self.conf.geometry,
                    date           = '[datebegin]',
                    datebegin      = listrundate,
                    dateend        = '[datebegin]/+PT96H',
                    nativefmt      = 'netcdf',
                    kind           = 'SnowpackSimulation',
                    model          = 'surfex',
                    namespace      = 'vortex.multi.fr',
                    cutoff         = 'production' if self.conf.previ else 'assimilation',
                    member         = 35,
                    fatal          = False
                ),
                print(t.prompt, 'tb11 =', tb11)
                print()

                self.sh.title('Toolbox output tb12')
                tb12 = toolbox.output(
                    local          = '[date:ymdh]/PREP_[datevalidity:ymdh].nc',
                    role           = 'SnowpackInit',
                    experiment     = self.conf.xpid,
                    block          = 'prep',
                    geometry       = self.conf.geometry,
                    date           = listrundate,
                    datevalidity   = '[date]/+PT96H',
                    nativefmt      = 'netcdf',
                    kind           = 'PREP',
                    model          = 'surfex',
                    namespace      = 'vortex.multi.fr',
                    cutoff         = 'production' if self.conf.previ else 'assimilation',
                    member         = 35,
                    fatal          = False
                ),
                print(t.prompt, 'tb12 =', tb12)
                print()

            else:

                if source_safran != 's2m':
                    self.sh.title('Toolbox output tb10_tar')
                    tb10_tar = toolbox.output(
                        local          = 'FORCING_[datebegin:ymdh]_[dateend:ymdh].tar',
                        experiment     = self.conf.xpid,
                        block          = 'meteo',
                        geometry       = self.conf.geometry,
                        datebegin      = list_dates_begin_pro,
                        dateend        = dict_dates_end_pro,
                        nativefmt      = 'tar',
                        kind           = 'FORCING',
                        model          = 's2m',
                        namebuild      = 'flat@cen',
                        namespace      = 'vortex.archive.fr',
                        fatal          = True,
                    ),
                    print(t.prompt, 'tb10_tar =', tb10_tar)
                    print()

                self.sh.title('Toolbox output tb11_tar')
                tb11_tar = toolbox.output(
                    local='PRO_[datebegin:ymdh]_[dateend:ymdh].tar',
                    experiment=self.conf.xpid,
                    block='pro',
                    geometry=self.conf.geometry,
                    datebegin=list_dates_begin_pro,
                    dateend=dict_dates_end_pro,
                    nativefmt='tar',
                    kind='PRO',
                    model='surfex',
                    namebuild='flat@cen',
                    namespace='vortex.archive.fr',
                    fatal=True,
                ),
                print(t.prompt, 'tb11_tar =', tb11_tar)
                print()
