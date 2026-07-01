# -*- coding: utf-8 -*-
"""
"""

from mkjob.nodes import Task
from vortex_cen.tasks.oper_research_mixin import CENTaskMixIn
from vortex_cen.tools.monitoring import InputReportContext, OutputReportContext
import vortex
from bronx.stdtypes.date import daterange, yesterday, tomorrow
import footprints


class Ensemble_Surfex_Task(CENTaskMixIn, Task):
    """
    **Task : Ensemble_Surfex_Task**

    Task for operational ensemble SURFEX simulation used in the Drivers of
    ensemble_surfex_tasks_analysis.py and in ensemble_surfex_tasks_forecast.py
    Ensemble member include FORCING files derived from ARPEGE and PEARP, plus a specific "SYTRON" member.

    **Input:**

    - ensemble of FORCING files in a "slopes geometry"
        * [ALTERNATE] ensemble of FORCING files in a "flat geometry"
    - PGD.nc (Ground physiography)
    - PREP.nc (initial conditions) from the last run of the "deterministic" member (except for the SYTRON member)
    - OPTIONS.nam ready-to-use SURFEX namelist (coming from an execution of a "Preprocess_Task")
    - ecoclimapI_covers_param.bin and ecoclimapII_eu_covers_param.bin (binaries for vegetation generation)
    - drdt_bst_fit_60.nc (Crocus metamorphism parameters)
    - OFFLINE_NOMPI binary

    **Output:**

    - PRO.nc : Snowpack simulation output
    - [OPTIONAL] FORCING.nc in a "slopes geometry"
    - PREP.nc : Snowpack state at the end of the simulation

    """

    MANDATORY_CONFIGURATION_VARIABLES = [
        "xpid",
        "geometry",
        "rundate+help=Date of run;choices=YYYYMMDD[03 06 09 12];type=str or Date",  # used in the "get_period" method
        "previ+help=Activate forecast mode;type=bool",  # used in the "get_period" method
        "namespace_in+help=Where to look for nwp files;type=str",
        "namespace_out+help=Where to store output guess files;type=str",
        "prepid+help=*xpid* of the PREP.nc file",
        "focringid+help=*xpid* of the FORCING files",
        "cycle+help=Alias for uenv;type=str",
        "threshold+help=Threshold to apply to the snow water equivalent (in kg/m2);type=int",
    ]
    OPTIONAL_CONFIGURATION_VARIABLES = [
        "prep_vortex1+help=If the 'warmstart' target PREP.nc file was produced with vortex1,type=bool",
    ]
    # Filter of errors to be applied in both oper and dev cases
    filter_execution_error = CENTaskMixIn.s2moper_filter_execution_error
    # only in dev for CEN, to be defined for IGA
    report_execution_warning = CENTaskMixIn.s2moper_report_execution_warning
    # only in dev for CEN, keep IGA method for oper
    report_execution_error = CENTaskMixIn.s2moper_report_execution_error

    def process(self):

        t = self.ticket

        datebegin, dateend = self.get_period()
        rundate_forcing = self.get_rundate_forcing()
        rundate_prep, alternate_rundate_prep = self.get_rundate_prep()

        list_geometry = self.get_list_geometry()
        source_safran, block_safran = self.get_source_safran()
        alternate_safran, alternate_block, alternate_geometry = self.get_alternate_safran()
        exceptional_save_forcing = False

        pearpmembers, members = self.get_list_members()

        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            with InputReportContext(self, t):

                self.sh.title('Toolbox input tb01')
                tb01 = vortex.input(
                    role           = 'Forcing_Deterministic',
                    local          = 'mb035/[geometry::tag]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc'
                    if len(list_geometry) > 1 else 'mb035/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                    vapp           = self.conf.vapp,
                    vconf          = '[geometry:domain]',
                    block          = block_safran,
                    member         = 35 if source_safran == 's2m' else None,
                    source_app     = 'arpege' if source_safran == 'safran' else None,
                    source_conf    = '4dvarfr' if source_safran == 'safran' else None,
                    experiment     = self.conf.forcingid if source_safran == 'safran' else self.conf.xpid,
                    geometry       = list_geometry,
                    date           = rundate_forcing,
                    datebegin      = datebegin if source_safran == 'safran' else yesterday(base=datebegin),
                    dateend        = dateend,
                    nativefmt      = 'netcdf',
                    kind           = 'MeteorologicalForcing',
                    namespace      = 'vortex.multi.fr',
                    model          = source_safran,
                    cutoff         = 'production' if self.conf.previ else 'assimilation',
                    fatal          = False
                ),
                print(t.prompt, 'tb01 =', tb01)
                print()

                if not any(tb01) and source_safran == "s2m":  # alternate case if forcing not available in s2m task

                    self.sh.title('Toolbox input tb01a')
                    tb01a = vortex.input(
                        alternate      = 'Forcing_Deterministic',
                        local          = 'mb035/[geometry::tag]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc'
                        if len(list_geometry) > 1 else 'mb035/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                        vapp           = self.conf.vapp,
                        vconf          = '[geometry:domain]',
                        block          = alternate_block,
                        member         = None,
                        source_app     = 'arpege',
                        source_conf    = '4dvarfr',
                        experiment     = self.conf.forcingid,
                        geometry       = alternate_geometry,
                        date           = rundate_forcing,
                        datebegin      = yesterday(base=datebegin),
                        dateend        = dateend,
                        nativefmt      = 'netcdf',
                        kind           = 'MeteorologicalForcing',
                        namespace      = self.conf.namespace_in,
                        model          = alternate_safran,
                        cutoff         = 'production' if self.conf.previ else 'assimilation',
                        fatal          = False
                    ),
                    print(t.prompt, 'tb01a =', tb01a)
                    print()

                self.sh.title('Toolbox input tb01b')
                tb01b = vortex.input(
                    role           = 'Forcing',
                    local          = 'mb[member]/[geometry::tag]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc'
                    if len(list_geometry) > 1 else 'mb[member]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                    vapp           = self.conf.vapp,
                    vconf          = '[geometry:domain]',
                    block          = block_safran,
                    source_app     = 'arpege' if source_safran == 'safran' else None,
                    source_conf    = 'pearp' if source_safran == 'safran' else None,
                    experiment     = self.conf.forcingid  if source_safran == 'safran' else self.conf.xpid,
                    geometry       = list_geometry,
                    date           = rundate_forcing,
                    datebegin      = datebegin if source_safran == 'safran' else yesterday(base=datebegin),
                    dateend        = dateend,
                    member         = pearpmembers,
                    nativefmt      = 'netcdf',
                    kind           = 'MeteorologicalForcing',
                    namespace      = self.conf.namespace_in,
                    model          = source_safran,
                    cutoff         = 'production' if self.conf.previ else 'assimilation',
                    fatal          = False
                ),
                print(t.prompt, 'tb01b =', tb01b)
                print()

                if not any(tb01b) and source_safran == "s2m":  # alternate case if forcing not available in s2m task

                    self.sh.title('Toolbox input tb01c')
                    tb01c = vortex.input(
                        alternate           = 'Forcing',
                        local          = 'mb[member]/[geometry::tag]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc'
                        if len(list_geometry) > 1 else 'mb[member]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                        vapp           = self.conf.vapp,
                        vconf          = '[geometry:domain]',
                        block          = alternate_block,
                        source_app     = 'arpege',
                        source_conf    = 'pearp',
                        experiment     = self.conf.forcingid,
                        geometry       = alternate_geometry,
                        date           = rundate_forcing,
                        datebegin      = yesterday(base=datebegin),
                        dateend        = dateend,
                        member         = pearpmembers,
                        nativefmt      = 'netcdf',
                        kind           = 'MeteorologicalForcing',
                        namespace      = self.conf.namespace_in,
                        model          = alternate_safran,
                        cutoff         = 'production' if self.conf.previ else 'assimilation',
                        fatal          = False
                    ),
                    print(t.prompt, 'tb01c =', tb01c)
                    print()

                print(any(tb01), any(tb01b))

                if not any(tb01) and not any(tb01b) and source_safran == 's2m':
                    print('MODE SECOURS')
                    print(any(tb01a), any(tb01c))

                    if any(tb01a) or any(tb01c):
                        print("EXCEPTIONAL SAVE FORCING")
                        exceptional_save_forcing = True
                        list_geometry = alternate_geometry[:]
                        print(list_geometry)

                self.sh.title('Toolbox input tb02')
                tb02 = vortex.input(
                    role           = 'SurfexClim',
                    kind           = 'pgdnc',
                    nativefmt      = 'netcdf',
                    local          = 'PGD.nc',
                    geometry       = self.conf.geometry,
                    genv           = self.conf.cycle,
                    gvar           = 'pgd_[geometry::tag]',
                    model          = 'surfex',
                    fatal          = True,
                ),
                print(t.prompt, 'tb02 =', tb02)
                print()

                if self.conf.previ:
                    # Forecasts are all initialized by the deterministic analysis (member=35)
                    self.sh.title('Toolbox input tb03c')
                    tb03 = vortex.input(
                        role           = 'SnowpackInit',
                        local          = 'PREP.nc',
                        block          = 'prep',
                        experiment     = self.conf.prepid,
                        geometry       = self.conf.geometry,
                        datevalidity   = datebegin,
                        date           = rundate_prep,
                        member         = 35,
                        intent         = 'inout',
                        nativefmt      = 'netcdf',
                        kind           = 'PREP',
                        model          = 'surfex',
                        namespace      = self.conf.namespace_in,
                        vortex1        = self.conf.get('prep_vortex1', False),
                        fatal          = False,
                        cutoff         = 'assimilation'
                    ),
                    print(t.prompt, 'tb03 =', tb03)
                    print()

                    # Previous runs can replace if the expected run is missing
                    for i, alternate_prep in enumerate(alternate_rundate_prep):
                        self.sh.title('Toolbox input tb03c')
                        tb03b = vortex.input(
                            alternate      = 'SnowpackInit',
                            local          = 'PREP.nc',
                            block          = 'prep',
                            experiment     = self.conf.prepid,
                            geometry       = self.conf.geometry,
                            datevalidity   = datebegin,
                            date           = alternate_prep[0],
                            member         = 35,
                            intent         = 'inout',
                            nativefmt      = 'netcdf',
                            kind           = 'PREP',
                            model          = 'surfex',
                            namespace      = self.conf.namespace_in,
                            vortex1        = self.conf.get('prep_vortex1', False),
                            fatal          = False,
                            cutoff         = alternate_prep[1]
                        ),
                        print(t.prompt, 'tb03b =', tb03b)
                        print()

                    if not self.conf.geometry.area == "postes":
                        # SYTRON forecasts are initialized by the SYTRON analysis (member=36)
                        self.sh.title('Toolbox input tb03c')
                        tb03 = vortex.input(
                            role           = 'SnowpackInit',
                            local          = 'mb[member]/PREP.nc',
                            block          = 'prep',
                            experiment     = self.conf.prepid,
                            geometry       = self.conf.geometry,
                            datevalidity   = datebegin,
                            date           = rundate_prep,
                            member         = 36,
                            intent         = 'inout',
                            nativefmt      = 'netcdf',
                            kind           = 'PREP',
                            model          = 'surfex',
                            namespace      = self.conf.namespace_in,
                            vortex1        = self.conf.get('prep_vortex1', False),
                            fatal          = False,
                            cutoff         = 'assimilation'
                        ),
                        print(t.prompt, 'tb03 =', tb03)
                        print()

                        # Previous runs can replace if the expected run is missing
                        for i, alternate_prep in enumerate(alternate_rundate_prep):
                            self.sh.title('Toolbox input tb03c')
                            tb03b = vortex.input(
                                alternate      = 'SnowpackInit',
                                local          = 'mb[member]/PREP.nc',
                                block          = 'prep',
                                experiment     = self.conf.prepid,
                                geometry       = self.conf.geometry,
                                datevalidity   = datebegin,
                                date           = alternate_prep[0],
                                member         = 36,
                                intent         = 'inout',
                                nativefmt      = 'netcdf',
                                kind           = 'PREP',
                                model          = 'surfex',
                                namespace      = self.conf.namespace_in,
                                vortex1        = self.conf.get('prep_vortex1', False),
                                fatal          = False,
                                cutoff         = alternate_prep[1]
                            ),
                            print(t.prompt, 'tb03b =', tb03b)
                            print()

                else:
                    # Analyses are initialized by the corresponding members of the previous run
                    self.sh.title('Toolbox input tb03')
                    tb03 = vortex.input(
                        role           = 'SnowpackInit',
                        local          = 'mb[member]/PREP.nc',
                        block          = 'prep',
                        experiment     = self.conf.prepid,
                        geometry       = self.conf.geometry,
                        datevalidity   = datebegin,
                        date           = rundate_prep,
                        member         = members,
                        intent         = 'inout',
                        nativefmt      = 'netcdf',
                        kind           = 'PREP',
                        model          = 'surfex',
                        namespace      = self.conf.namespace_in,
                        vortex1        = self.conf.get('prep_vortex1', False),
                        fatal          = False,
                        cutoff         = 'assimilation'
                    ),
                    print(t.prompt, 'tb03 =', tb03)
                    print()

                    # Previous runs can replace if the expected run is missing
                    for i, alternate_prep in enumerate(alternate_rundate_prep):

                        # fatal = i == len(alternate_rundate_prep) - 1

                        self.sh.title('Toolbox input tb03b')
                        tb03b = vortex.input(
                            alternate      = 'SnowpackInit',
                            local          = 'mb[member]/PREP.nc',
                            block          = 'prep',
                            experiment     = self.conf.prepid,
                            geometry       = self.conf.geometry,
                            datevalidity   = datebegin,
                            date           = alternate_prep[0],
                            member         = members,
                            intent         = 'inout',
                            nativefmt      = 'netcdf',
                            kind           = 'PREP',
                            model          = 'surfex',
                            namespace      = self.conf.namespace_in,
                            vortex1        = self.conf.get('prep_vortex1', False),
                            fatal          = False,
                            cutoff         = alternate_prep[1]
                        ),
                        print(t.prompt, 'tb03b =', tb03b)
                        print()

                        # We also get the SnowPackInitSecours resource in case some members are still missing:
                        # It will only be used by the members without any initial condition from recent runs.
                        # First we try the deterministic run:
                        self.sh.title('Toolbox input tb03c')
                        tb03c = vortex.input(
                            role           = 'SnowpackInitSecours',
                            local          = 'PREP.nc',
                            block          = 'prep',
                            experiment     = self.conf.prepid,
                            geometry       = self.conf.geometry,
                            datevalidity   = datebegin,
                            date           = rundate_prep,
                            member         = 35,
                            intent         = 'inout',
                            nativefmt      = 'netcdf',
                            kind           = 'PREP',
                            model          = 'surfex',
                            namespace      = self.conf.namespace_in,
                            vortex1        = self.conf.get('prep_vortex1', False),
                            fatal          = False,
                            cutoff         = 'assimilation'
                        ),
                        print(t.prompt, 'tb03c =', tb03c)
                        print()

                        # Last chance is the reanalysis if even the deterministic run is stopped:
			# This can allow a quick restart from a file on hendrix
			# produced by CEN after a long interruption.
                        self.sh.title('Toolbox input tb03e')
                        tb03d = vortex.input(
                            alternate      = 'SnowpackInitSecours',
                            local          = 'PREP.nc',
                            experiment     = self.ref_reanalysis,
                            geometry       = self.conf.geometry,
                            vconf          = self.conf.geometry.tag,
                            date           = datebegin,
                            intent         = 'inout',
                            nativefmt      = 'netcdf',
                            kind           = 'PREP',
                            model          = 'surfex',
                            namespace      = 'vortex.multi.fr',  # IGA can keep that: this is only for last chance rescue mode
                            vortex1        = self.conf.get('prep_vortex1', False),
                            namebuild      = 'flat@cen',
                            block          = 'prep',
                            fatal          = False,
                        )

                        print(t.prompt, 'tb03d =', tb03d)
                        print()

                self.sh.title('Toolbox input tb04')
                tb04 = vortex.input(
                    role           = 'Surfex cover parameters',
                    kind           = 'coverparams',
                    nativefmt      = 'bin',
                    local          = 'ecoclimapI_covers_param.bin',
                    geometry       = self.conf.geometry,
                    genv           = self.conf.cycle,
                    source         = 'ecoclimap1',
                    model          = 'surfex',
                ),
                print(t.prompt, 'tb04 =', tb04)
                print()

                self.sh.title('Toolbox input tb05')
                tb05 = vortex.input(
                    role           = 'Surfex cover parameters',
                    kind           = 'coverparams',
                    nativefmt      = 'bin',
                    local          = 'ecoclimapII_eu_covers_param.bin',
                    geometry       = self.conf.geometry,
                    genv            = self.conf.cycle,
                    source         = 'ecoclimap2',
                    model          = 'surfex',
                ),
                print(t.prompt, 'tb05 =', tb05)
                print()

                self.sh.title('Toolbox input tb06')
                tb06 = vortex.input(
                    role            = 'Parameters for F06 metamorphism',
                    kind            = 'ssa_params',
                    genv            = self.conf.cycle,
                    nativefmt       = 'netcdf',
                    local           = 'drdt_bst_fit_60.nc',
                    model          = 'surfex',
                )
                print(t.prompt, 'tb06 =', tb06)
                print()

                self.sh.title('Toolbox input tb07')

                tb07a = vortex.input(
                    role            = 'Nam_surfex',
                    source          = 'OPTIONS_default.nam',
                    genv            = self.conf.cycle,
                    kind            = 'namelist',
                    model           = 'surfex',
                    local           = 'OPTIONS.nam',
                )

                self.sh.title('Toolbox input tb07a')

                tb07 = vortex.input(
                    role            = 'Nam_surfex',
                    source          = 'OPTIONS_sytron.nam',
                    genv            = self.conf.cycle,
                    kind            = 'namelist',
                    model           = 'surfex',
                    local           = 'OPTIONS_sytron.nam',
                )

                print(t.prompt, 'tb07 =', tb07)
                print()

                self.sh.title('Toolbox executable tb08= tbx1')
                tb08 = tbx1 = vortex.executable(
                    role           = 'Binary',
                    kind           = 'offline',
                    local          = 'OFFLINE',
                    model          = 'surfex',
                    genv           = self.conf.cycle,
                    gvar           = 'master_offline_nompi',
                )

                print(t.prompt, 'tb08 =', tb08)
                print()

        if 'compute' in self.steps:

            self.sh.title('Toolbox algo tb09 = OFFLINE')

            tb09 = tbalgo1 = vortex.task(
                engine         = 's2m',
                kind           = "ensmeteo" if self.conf.geometry.area == 'postes' else "ensmeteo+sytron",
                datebegin      = datebegin,
                dateend        = dateend,
                dateinit       = datebegin,
                threshold      = self.conf.threshold,
                members        = footprints.util.rangex(members),
                geometry_in    = list_geometry if any(tb01) or source_safran != 's2m' else alternate_geometry,
                geometry_out   = self.conf.geometry.tag,
                ntasks         = 6 if self.conf.rundate.hour == self.monthly_analysis_time else 40,
                daily          = not self.conf.previ,
                taskset        = "numapacked_taskset",
                verbose        = True,
                reprod_info    = self.get_reprod_info,
            )
            print(t.prompt, 'tb09 =', tb09)
            print()

            self.component_runner(tbalgo1, tbx1)

        if 'backup' in self.steps or 'late-backup' in self.steps:

            with OutputReportContext(self, t):

                self.sh.title('Toolbox output tb11')
                tb11 = vortex.output(
                    local       = 'mb[member]/PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment  = self.conf.xpid,
                    block       = 'pro',
                    geometry    = self.conf.geometry,
                    date        = self.conf.rundate,
                    datebegin   = datebegin if self.conf.previ else '[dateend]/-PT24H',
                    dateend     = dateend if self.conf.previ else list(daterange(tomorrow(base=datebegin), dateend)),
                    member      = members,
                    nativefmt   = 'netcdf',
                    kind        = 'SnowpackSimulation',
                    model       = 'surfex',
                    namespace   = self.conf.namespace_out,
                    cutoff      = 'production' if self.conf.previ else 'assimilation',
                    fatal       = False
                ),
                print(t.prompt, 'tb11 =', tb11)
                print()

                if source_safran != 's2m' or exceptional_save_forcing:
                    self.sh.title('Toolbox output tb10')
                    tb10 = vortex.output(
                        local          = 'mb[member]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                        experiment     = self.conf.xpid,
                        block          = 'meteo',
                        geometry       = self.conf.geometry,
                        date           = self.conf.rundate,
                        datebegin      = datebegin,
                        dateend        = dateend,
                        member         = members,
                        nativefmt      = 'netcdf',
                        kind           = 'MeteorologicalForcing',
                        model          = 's2m',
                        namespace      = self.conf.namespace_out, 
                        cutoff         = 'production' if self.conf.previ else 'assimilation',
                        fatal          = False
                    ),
                    print(t.prompt, 'tb10 =', tb10)
                    print()

                self.sh.title('Toolbox output tb12')
                tb12 = vortex.output(
                    local          = 'mb[member]/PREP_[datevalidity:ymdh].nc',
                    role           = 'SnowpackInit',
                    experiment     = self.conf.xpid,
                    block          = 'prep',
                    geometry       = self.conf.geometry,
                    datevalidity   = dateend if self.conf.previ else list(daterange(tomorrow(base=datebegin), dateend)),
                    date           = self.conf.rundate,
                    member         = members,
                    nativefmt      = 'netcdf',
                    kind           = 'PREP',
                    model          = 'surfex',
                    namespace      = self.conf.namespace_out,
                    cutoff         = 'production' if self.conf.previ else 'assimilation',
                    fatal          = False
                ),
                print(t.prompt, 'tb12 =', tb12)
                print()
