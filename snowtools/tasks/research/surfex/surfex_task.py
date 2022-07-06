# -*- coding: utf-8 -*-
'''
Created on 7 nov. 2017

@author: lafaysse
'''

from vortex.layout.nodes import Driver, Task
from vortex import toolbox
from snowtools.utils.dates import get_list_dates_files, get_dic_dateend
from bronx.stdtypes.date import Date, daterange, tomorrow
from cen.layout.nodes import S2MTaskMixIn


def setup(t, **kw):
    return Driver(
        tag='Surfex_Parallel',
        ticket=t,
        nodes=[
            Surfex_Vortex_Task(tag='Surfex_Vortex_Task', ticket=t, **kw),
        ],
        options=kw
    )


class Surfex_Vortex_Task(Task, S2MTaskMixIn):
    '''
    Generic task for a MPI Surfex/Crocus reanalysis
    '''

    def process(self):

        t = self.ticket

        if not hasattr(self.conf, "genv"):
            self.conf.genv = 'uenv:cen.06@CONST_CEN'

        # Definition of geometries, safran xpid/block and list of dates from S2MTaskMixIn methods
        list_geometry = self.get_list_geometry(meteo=self.conf.meteo)
        source_safran, block_safran = self.get_source_safran(meteo=self.conf.meteo)
        list_dates_begin_forc, list_dates_end_forc, list_dates_begin_pro, list_dates_end_pro = \
            get_list_dates_files(self.conf.datebegin, self.conf.dateend, self.conf.duration)
        dict_dates_end_forc = get_dic_dateend(list_dates_begin_forc, list_dates_end_forc)
        dict_dates_end_pro = get_dic_dateend(list_dates_begin_pro, list_dates_end_pro)
        dict_source_app_safran, dict_source_conf_safran = self.get_safran_sources(list_dates_begin_forc)

        # Logicals to activate optional parts of the task
        if not hasattr(self.conf, "interpol"):
            self.conf.interpol = False
        if not hasattr(self.conf, "climground"):
            self.conf.climground = False
        if not hasattr(self.conf, "dailyprep"):
            self.conf.dailyprep = False
        if not hasattr(self.conf, "simu2D"):
            self.conf.simu2D = False
        if hasattr(self.conf, "simu2D"):
            self.conf.genv2D = 'uenv:pgd.002@SURFEX_CEN'

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

            # Look for a PREP file if already available for this xpid, geometry, and initial date
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

            # 1st alternate : look for a PREP file if already available for this geometry,
            # and initial date for the "spinup" xpid
            self.sh.title('Toolbox input tb03')
            tb03a = toolbox.input(
                alternate      = 'SnowpackInit',
                local          = 'PREP.nc',
                experiment     = 'spinup@' + t.env.getvar("USER"),
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
            print(t.prompt, 'tb03a =', tb03a)
            print()

            # 2nd alternate : look for a PREP file if already available for this geometry,
            # and initial date for the "reanalysis" xpid
            # If not available, do not fail because the PREP file will be automatically built.
            self.sh.title('Toolbox input tb03')
            tb03a2 = toolbox.input(
                alternate      = 'SnowpackInit',
                local          = 'PREP.nc',
                experiment     = self.ref_reanalysis,
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
            print(t.prompt, 'tb03a2 =', tb03a2)
            print()

            if not tb03[0] and not tb03a[0] and not tb03a2[0] and not self.conf.climground:
                # not self.conf.climground means that the user does not allow the climatological file
                # to be built by the system (-g option of the s2m command is not activated)
                # If no prep file has been found, look for a climatological file to initialize the ground in the uenv
                tbi = toolbox.input(
                    role           = 'initial values of ground temperature',
                    kind           = 'climTG',
                    nativefmt      = 'netcdf',
                    local          = 'init_TG.nc',
                    geometry       = self.conf.geometry,
                    genv           = self.conf.genv,
                    gvar           = 'climtg_[geometry::tag]',
                    model          = 'surfex',
                    fatal          = False
                ),
                print(t.prompt, 'tbi =', tbi)
                print()

                # Alternate : look for the climatological file if already available for this xpid and geometry
                # Fail if not available because mandatory to build a prep file !
                tbi_a = toolbox.input(
                    alternate      = 'initial values of ground temperature',
                    kind           = 'climTG',
                    nativefmt      = 'netcdf',
                    local          = 'init_TG.nc',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    model          = 'surfex',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                    block          = 'prep',
                ),

                print(t.prompt, 'tbi_a =', tbi_a)
                print()

            # Binary ECOCLIMAP I files are mandatory to run SURFEX and taken from the uenv
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

            # Binary ECOCLIMAP II files are mandatory to run SURFEX and taken from the uenv
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

            # Crocus metamorphism parameters mandatory to run SURFEX and taken from the uenv
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

            # For the 2d-simulation on Belenos: avoiding the PGD copy
            if not (tb02[0] or tb02_a[0]) and self.conf.simu2D:
                # If no PGD file has been found, look for the PGD binary
                # Binary Sand files are mandatory to run SURFEX for PGD construction in simu2D
                self.sh.title('Toolbox input tb04b')
                tb04b = toolbox.input(
                    role           = 'SandDB',
                    format         = 'dir/hdr',
                    genv           = self.conf.genv2D,
                    model          = 'surfex',
                    kind           = 'sand', #'database'
                    local          = 'sand_DB.02.tgz',
                    source         = 'sand_DB',
                    gvar           = 'sand_DB',
                )
                print(t.prompt, 'tb04b =', tb04b)
                print()

                # Binary Clay files are mandatory to run SURFEX for PGD construction in simu2D
                self.sh.title('Toolbox input tb04c')
                tb04c = toolbox.input(
                    role           = 'ClayDB',
                    format         = 'dir/hdr',
                    genv           = self.conf.genv2D,
                    model          = 'surfex',
                    kind           = 'clay',
                    local          = 'clay_DB.02.tgz',
                    source         = 'clay_DB',
                    gvar           = 'clay_DB',
                )
                print(t.prompt, 'tb04c =', tb04c)
                print()

                # EcoclimapII_europ files are mandatory to run SURFEX for PGD construction in simu2D
                self.sh.title('Toolbox input tb04d')
                tb04d = toolbox.input(
                    role           = 'EcoclimapIIEurop',
                    format         = 'dir/hdr',
                    genv           = self.conf.genv2D,
                    model          = 'surfex',
                    kind           = 'coverparams',
                    local          = 'ECOCLIMAP_II_EUROP.02.tgz',
                    source         = 'ecoclimap2',
                    gvar           = 'ECOCLIMAP_II_EUROP',
                )
                print(t.prompt, 'tb04d =', tb04d)
                print()

            # Use the path provided in the configuration file for the SURFEX namelist
            self.sh.title('Toolbox input tb05')
            if self.conf.namelist:
                tb05 = toolbox.input(
                    role            = 'Nam_surfex',
                    remote          = self.conf.namelist,
                    kind            = 'namelist',
                    model           = 'surfex',
                    local           = 'OPTIONS.nam',
                )
            else:
                # If not provided, standard namelist taken from the uenv
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

            # Target grid file if an interpolation is required before the run
            # and the path must be provided in the configuration file
            if self.conf.interpol:
                tbgrid = toolbox.input(
                    role   = 'gridout',
                    remote = self.conf.gridout,
                    kind   = 'interpolgrid',
                    model  = 'surfex',
                    local  = 'GRID.nc',
                )
                print(t.prompt, 'tbgrid =', tbgrid)
            else:
                print(t.prompt, 'tbgrid = undefined')
            print()

            # SURFEX binaries are taken from the path in the configuration file is provided
            if self.conf.exesurfex:
                # OFFLINE binary is always required
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
                    # If no PGD file has been found, look for the PGD binary
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

                if not tb03[0] and not tb03a[0] and not tb03a2[0]:
                    # If no PREP file has been found, look for the PREP binary
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
                # If not provided in the configuration file, binaries are taken from the uenv
                # OFFLINE binary is always required
                self.sh.title('Toolbox executable tb06= tbx1')
                tb06 = tbx3 = toolbox.executable(
                    role           = 'Binary',
                    kind           = 'offline',
                    local          = 'OFFLINE',
                    model          = 'surfex',
                    genv           = self.conf.genv,
                    gvar           = 'master_surfex_offline_mpi',
                )

                print(t.prompt, 'tb06 =', tb06)
                print()

                if not (tb02[0] or tb02_a[0]):
                    # If no PGD file has been found, look for the PGD binary
                    self.sh.title('Toolbox executable tb07= tbx2')
                    tb07 = tbx1 = toolbox.executable(
                        role           = 'Binary',
                        kind           = 'buildpgd',
                        local          = 'PGD',
                        model          = 'surfex',
                        genv           = self.conf.genv,
                        gvar           = 'master_pgd_mpi',
                    )

                    print(t.prompt, 'tb07 =', tb07)
                    print()

                if not tb03[0] and not tb03a[0] and not tb03a2[0]:
                    # If no PREP file has been found, look for the PREP binary
                    self.sh.title('Toolbox executable tb08= tbx3')
                    tb08 = tbx2 = toolbox.executable(
                        role           = 'Binary',
                        kind           = 'prep',
                        local          = 'PREP',
                        model          = 'surfex',
                        genv           = self.conf.genv,
                        gvar           = 'master_prep_mpi',
                    )

                    print(t.prompt, 'tb08 =', tb08)
                    print()

            # If the forcing must be interpolated, take the interpolation binary from the uenv
            if self.conf.interpol:
                tbI = tbxi = toolbox.executable(
                    role           = 'Binary',
                    kind           = 'offline',
                    local          = 'INTERPOL',
                    model          = 'surfex',
                    genv           = self.conf.genv,
                    gvar           = 'master_interpol_mpi',
                )

                print(t.prompt, 'tbI =', tbI)
                print()

        if 'compute' in self.steps:

            print (self.conf.meteo, self.conf.interpol, self.conf.addmask)

            if self.conf.meteo == "safran":
                # Forcing files need to be converted from flat to slopes geometry
                # Parallelization on the years but limited for memory issues on alp_allslopes and pyr_allslopes domains
                if self.conf.geometry.tag in ["alp_allslopes", "pyr_allslopes", "alp27_allslopes", "pyr23_allslopes"]:
                    ntasks = min(5, len(list_dates_begin_forc))
                else:
                    ntasks = min(40, len(list_dates_begin_forc))

                # Algo component converting the forcing files geometry
                # (duplication of points and projection of solar radiations)
                self.sh.title('Toolbox algo tb09a')
                tb09a = tbalgo1 = toolbox.algo(
                    engine       = 's2m',
                    kind         = 'prepareforcing',
                    datebegin    = list_dates_begin_forc if not oneforcing else [self.conf.datebegin],
                    dateend      = list_dates_end_forc if not oneforcing else [self.conf.dateend],
                    ntasks       = ntasks,
                    geometry_in  = list_geometry,
                    geometry_out = self.conf.geometry.tag
                )
                print(t.prompt, 'tb09a =', tb09a)
                print()
                tb09a.run()

            elif self.conf.interpol:
                # Algo component for interpolation of the forcing on a regular grid
                self.sh.title('Toolbox algo tb09a')
                tb09a = tbalgo1 = toolbox.algo(
                    engine         = 'parallel',
                    binary         = 'INTERPOL',
                    kind           = 'deterministic'
                )
                print(t.prompt, 'tb09a =', tb09a)
                print()

                self.component_runner(tbalgo1, tbxi)

                # Algo component for shadows
                if self.conf.addmask:
                    self.sh.title('Toolbox algo tb09abis')
                    tb09abis = tbalgo_shadows = toolbox.algo(
                        engine       = 's2m',
                        kind         = 'shadowsforcing',
                        datebegin    = list_dates_begin_forc if not oneforcing else [self.conf.datebegin],
                        dateend      = list_dates_end_forc if not oneforcing else [self.conf.dateend],
                        ntasks       = min(40, len(list_dates_begin_forc))
                    )
                    print(t.prompt, 'tb09abis =', tb09abis)
                    print()

                    tb09abis.run()

            if self.conf.climground:
                # Algo component to build the climatological file if allowed by the user (-g option)
                self.sh.title('Toolbox algo tb09a')
                tb09b = tbalgo1 = toolbox.algo(
                    engine       = 's2m',
                    kind         = 'clim',
                )
                print(t.prompt, 'tb09b =', tb09b)
                print()
                tb09b.run()

            if oneforcing:
                firstforcing = 'FORCING_' + self.conf.datebegin.strftime("%Y%m%d%H") + \
                    "_" + self.conf.dateend.strftime("%Y%m%d%H") + ".nc"
            else:
                firstforcing = 'FORCING_' + list_dates_begin_forc[0].strftime("%Y%m%d%H") + \
                    "_" + list_dates_end_forc[0].strftime("%Y%m%d%H") + ".nc"

            # Algo component to preprocess the namelist (adjust dates, etc.)
            self.sh.title('Toolbox algo tb09a')
            tb09a = tbalgo1 = toolbox.algo(
                kind         = 'surfex_preprocess',
                datebegin    = self.conf.datebegin,
                dateend      = self.conf.dateend,
                forcingname  = firstforcing
            )
            print(t.prompt, 'tb09a =', tb09a)
            print()
            tb09a.run()

            # Algo component to produce the PGD file if not found in the inputs
            # Take care : PGD parallelization will be available in v8.1 --> nproc and ntasks will have to be set to 40
            if not (tb02[0] or tb02_a[0]):
                self.sh.title('Toolbox algo tb09 = PGD')
                tb09 = tbalgo2 = toolbox.algo(
                    kind         = 'pgd_from_forcing',
                    forcingname  = firstforcing,
                )
                print(t.prompt, 'tb09 =', tb09)
                print()
                self.component_runner(tbalgo2, tbx1, mpiopts = dict(nnodes=1, nprocs=1, ntasks=1))

            # Algo component to produce the PREP file if not found in the inputs
            # Take care : PREP parallelization will be available in v8.1 --> nproc and ntasks will have to be set to 40
            if not tb03[0] and not tb03a[0] and not tb03a2[0]:
                self.sh.title('Toolbox algo tb09 = PREP')
                tb10 = tbalgo3 = toolbox.algo(
                    engine     = 'parallel',
                )
                print(t.prompt, 'tb10 =', tb10)
                print()
                self.component_runner(tbalgo3, tbx2, mpiopts = dict(nnodes=1, nprocs=2, ntasks=2))

            # Algo component to produce to run the SURFEX OFFLINE simulation (MPI parallelization)
            self.sh.title('Toolbox algo tb11 = OFFLINE')
            tb11 = tbalgo4 = toolbox.algo(
                engine         = 'parallel',
                binary         = 'OFFLINE',
                kind           = 'deterministic',
                datebegin      = self.conf.datebegin,
                dateend        = self.conf.dateend,
                dateinit       = Date(self.conf.datespinup),
                threshold      = self.conf.threshold,
                daily          = self.conf.dailyprep
            )
            print(t.prompt, 'tb11 =', tb11)
            print()

            if self.conf.geometry.tag in ["cor_flat"]:
                # Specific number of threads must be provided for domains
                # with a number of points lower than the number of MPI threads
                self.component_runner(tbalgo4, tbx3, mpiopts=dict(nnodes=1, nprocs=18, ntasks=18))
            else:
                self.component_runner(tbalgo4, tbx3)

        if 'backup' in self.steps:
            pass

        if 'late-backup' in self.steps:

            # First we try to save a PRO file covering the whole simulation period if present
            datebegin = self.conf.datebegin
            dateend = self.conf.dateend
            self.sh.title('Toolbox output tb19')
            tb19 = toolbox.output(
                local          = 'PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                experiment     = self.conf.xpid,
                geometry       = self.conf.geometry,
                datebegin      = datebegin if not self.conf.dailyprep else '[dateend]/-PT24H',
                dateend        = dateend if not self.conf.dailyprep else list(daterange(tomorrow(base=datebegin),
                                                                                        dateend)),
                nativefmt      = 'netcdf',
                kind           = 'SnowpackSimulation',
                model          = 'surfex',
                namespace      = 'vortex.multi.fr',
                namebuild      = 'flat@cen',
                block          = 'pro',
                fatal          = False
            ),
            print(t.prompt, 'tb19 =', tb19)
            print()

            if hasattr(self.conf, "writesx"):
                if self.conf.writesx:
                    self.sh.title('Toolbox output tb19bis')
                    tb19bis = toolbox.output(
                        local       = 'PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                        experiment  = self.conf.xpid,
                        geometry    = self.conf.geometry,
                        datebegin   = datebegin if not self.conf.dailyprep else '[dateend]/-PT24H',
                        dateend     = dateend if not self.conf.dailyprep else list(daterange(tomorrow(base=datebegin),
                                                                                             dateend)),
                        nativefmt   ='netcdf',
                        kind        = 'SnowpackSimulation',
                        model       = 'surfex',
                        namespace   = 'vortex.archive.fr',
                        storage     = 'sxcen.cnrm.meteo.fr',
                        enforcesync = True, # to forbid asynchronous transfers and not saturate sxcen
                        namebuild   = 'flat@cen',
                        block       = 'pro',
                        fatal       = False
                    ),
                    print(t.prompt, 'tb19bis =', tb19bis)
                    print()


            if tb19[0]:
                # Only one pro file for the whole simulation period
                # Save only one cumul and diag file covering the whole simulation
                # Save only one prep at the end of the simulation

                self.sh.title('Toolbox output tb19')
                tb19bis = toolbox.output(
                    local          = 'CUMUL_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    datebegin      = datebegin if not self.conf.dailyprep else '[dateend]/-PT24H',
                    dateend        = dateend if not self.conf.dailyprep else list(daterange(tomorrow(base=datebegin),
                                                                                            dateend)),
                    nativefmt      = 'netcdf',
                    kind           = 'SnowpackSimulation',
                    model          = 'surfex',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                    block          = 'cumul',
                    fatal          = False
                ),
                print(t.prompt, 'tb19bis =', tb19bis)
                print()

                tb19ter = toolbox.output(
                    local          = 'DIAG_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    datebegin      = datebegin if not self.conf.dailyprep else '[dateend]/-PT24H',
                    dateend        = dateend if not self.conf.dailyprep else list(daterange(tomorrow(base=datebegin),
                                                                                            dateend)),
                    nativefmt      = 'netcdf',
                    kind           = 'SnowpackSimulation',
                    model          = 'surfex',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                    block          = 'diag',
                    fatal          = False
                ),
                print(t.prompt, 'tb19 =', tb19ter)
                print()

                self.sh.title('Toolbox output tb20')
                tb20 = toolbox.output(
                    local          = 'PREP_[date:ymdh].nc',
                    role           = 'SnowpackInit',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    date           = dateend if not self.conf.dailyprep else list(daterange(tomorrow(base=datebegin),
                                                                                            dateend)),
                    nativefmt      = 'netcdf',
                    kind           = 'PREP',
                    model          = 'surfex',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                    block          = 'prep',
                ),
                print(t.prompt, 'tb20 =', tb20)
                print()

            else:
                # PRO not available for the whole simulation period: try to save yearly PRO, DIAG, CUMUL 
                # files + 1 PREP file per year
                self.sh.title('Toolbox output tb19')
                tb19 = toolbox.output(
                    local          = 'PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    datebegin      = list_dates_begin_pro if not self.conf.dailyprep else '[dateend]/-PT24H',
                    dateend        = dict_dates_end_pro if not self.conf.dailyprep else \
                                     list(daterange(tomorrow(base=datebegin), dateend)),
                    nativefmt      = 'netcdf',
                    kind           = 'SnowpackSimulation',
                    model          = 'surfex',
                    namespace      ='vortex.multi.fr',
                    namebuild      = 'flat@cen',
                    block          = 'pro',
                ),
                print(t.prompt, 'tb19 =', tb19)
                print()

                if hasattr(self.conf, "writesx"):
                    if self.conf.writesx:
                        self.sh.title('Toolbox output tb19bis')
                        tb19bis = toolbox.output(
                            local       = 'PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                            experiment  = self.conf.xpid,
                            geometry    = self.conf.geometry,
                            datebegin   = list_dates_begin_pro if not self.conf.dailyprep else '[dateend]/-PT24H',
                            dateend     = dict_dates_end_pro if not self.conf.dailyprep else \
                                          list(daterange(tomorrow(base=datebegin), dateend)),
                            nativefmt   = 'netcdf',
                            kind        = 'SnowpackSimulation',
                            model       = 'surfex',
                            namespace   ='vortex.archive.fr',
                            storage     = 'sxcen.cnrm.meteo.fr',
                            enforcesync = True,  # to forbid asynchronous transfers and not saturate sxcen
                            namebuild   = 'flat@cen',
                            block       = 'pro',
                        ),
                        print(t.prompt, 'tb19bis =', tb19bis)
                        print()

                tb19b = toolbox.output(
                    local          = 'DIAG_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    datebegin      = list_dates_begin_pro if not self.conf.dailyprep else '[dateend]/-PT24H',
                    dateend        = dict_dates_end_pro if not self.conf.dailyprep else \
                                     list(daterange(tomorrow(base=datebegin), dateend)),
                    nativefmt      = 'netcdf',
                    kind           = 'SnowpackSimulation',
                    model          = 'surfex',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                    block          = 'diag',
                    fatal          = False,
                ),
                print(t.prompt, 'tb19b =', tb19b)
                print()

                tb19c = toolbox.output(
                    local          = 'CUMUL_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    datebegin      = list_dates_begin_pro if not self.conf.dailyprep else '[dateend]/-PT24H',
                    dateend        = dict_dates_end_pro if not self.conf.dailyprep else \
                                     list(daterange(tomorrow(base=datebegin), dateend)),
                    nativefmt      = 'netcdf',
                    kind           = 'SnowpackSimulation',
                    model          = 'surfex',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                    block          = 'cumul',
                    fatal          = False,
                ),
                print(t.prompt, 'tb19c =', tb19c)
                print()

                self.sh.title('Toolbox output tb20')
                tb20 = toolbox.output(
                    local          = 'PREP_[date:ymdh].nc',
                    role           = 'SnowpackInit',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    date           = list_dates_end_pro if not self.conf.dailyprep else \
                                     list(daterange(tomorrow(base=datebegin), dateend)),
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
            # Save the PGD file for this xpid and geometry
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
                block          = 'pgd'),
            print(t.prompt, 'tb21 =', tb21)
            print()

            # Save the climatological file for this xpid and geometry if built during this task
            if self.conf.climground:
                tb22 = toolbox.output(
                    role           = 'initial values of ground temperature',
                    kind           = 'climTG',
                    nativefmt      = 'netcdf',
                    local          = 'init_TG.nc',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    model          = 'surfex',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                    block          = 'prep'),
                print(t.prompt, 'tb22 =', tb22)
                print()

            # Save the forcing files only if they have been converted towards a new geometry.
            if self.conf.meteo == 'safran' or self.conf.interpol:
                self.sh.title('Toolbox output tb19')
                tb19 = toolbox.output(
                    local          = 'FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    datebegin      = list_dates_begin_forc,
                    dateend        = dict_dates_end_forc,
                    nativefmt      = 'netcdf',
                    kind           = 'MeteorologicalForcing',
                    model          = 's2m',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                    block          = 'meteo',
                ),
                print(t.prompt, 'tb19 =', tb19)
                print()
