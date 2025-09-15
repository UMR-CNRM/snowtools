# -*- coding: utf-8 -*-
'''
Created on 7 mars 2024
@author: Vernay.M
'''

from vortex import toolbox
from snowtools.tasks.vortex_task_base import _VortexTask
from snowtools.utils.dates import get_list_dates_files, get_dic_dateend
from bronx.stdtypes.date import daterange, tomorrow, Period


class OfflineTask(_VortexTask):
    """
    Base class for SURFEX/OFFLINE executions
    """

    def get_remote_inputs(self):
        """
        Main method to fetch all necessary input files for an OFFLINE execution
        """
        t = self.ticket()

        ##########################################################################
        # 1. FORCING
        ##########################################################################

        # Definition of geometries, safran xpid/block and list of dates from S2MTaskMixIn methods
        list_geometry = self.get_list_geometry(meteo=self.conf.meteo)
        source_safran, block_safran = self.get_source_safran(meteo=self.conf.meteo)
        list_dates_begin_forc, list_dates_end_forc, list_dates_begin_pro, list_dates_end_pro = \
            get_list_dates_files(self.conf.datebegin, self.conf.dateend, self.conf.duration)
        dict_dates_end_forc = get_dic_dateend(list_dates_begin_forc, list_dates_end_forc)

        if not hasattr(self.conf, "era5"):
            self.era5 = 'era5' in self.conf.forcingid
        dict_source_app_safran, dict_source_conf_safran = self.get_safran_sources(list_dates_begin_forc, era5=self.era5)

        # Try to find a forcing covering the full simulation period
        self.sh.title('Forcing over the full simulation period')
        tb01a = toolbox.input(
            role           = 'Forcing',
            kind           = 'MeteorologicalForcing',
            vapp           = self.conf.meteo,
            vconf          = '[geometry:area]' if source_safran == 'safran' or 'oper' in self.conf.forcingid
                             else '[geometry:tag]',
            source_app     = dict_source_app_safran if source_safran == 'safran' else None,
            source_conf    = dict_source_conf_safran if source_safran == 'safran' else None,
            cutoff         = 'assimilation',
            local          = '[geometry::tag]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc'
                             if len(list_geometry) > 1 else 'FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
            experiment     = self.conf.forcingid,
            member         = self.conf.member if hasattr(self.conf, 'member') else None,
            block          = block_safran,
            geometry       = list_geometry,
            nativefmt      = 'netcdf',
            model          = 'safran',
            datebegin      = self.conf.datebegin,
            dateend        = self.conf.dateend,
            date           = self.conf.dateend.replace(hour=12) + Period(days=4),  # for monthly reanalysis only
            intent         = 'in',
            namespace      = 'vortex.multi.fr',
            namebuild      = 'flat@cen' if 'oper' not in self.conf.forcingid else None,
            fatal          = False if 'oper' not in self.conf.forcingid else True,
        ),
        print(t.prompt, 'tb01a =', tb01a)
        print()

        if not tb01a[0]:

            self.sh.title('Several (yearly) forcing files')
            tb01b = toolbox.input(
                role           = 'Forcing',
                kind           = 'MeteorologicalForcing',
                vapp           = self.conf.meteo,
                vconf          = '[geometry:area]' if source_safran == 'safran' else '[geometry:tag]',
                source_app     = dict_source_app_safran if source_safran == 'safran' else None,
                source_conf    = dict_source_conf_safran if source_safran == 'safran' else None,
                cutoff         = 'assimilation',
                local          = '[geometry::tag]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc'
                                 if len(list_geometry) > 1 else 'FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                experiment     = self.conf.forcingid,
                member         = self.conf.member if hasattr(self.conf, 'member') else None,
                block          = block_safran,
                geometry       = list_geometry,
                nativefmt      = 'netcdf',
                model          = 'safran',
                datebegin      = list_dates_begin_forc,
                dateend        = dict_dates_end_forc,
                intent         = 'in',
                namespace      = 'vortex.multi.fr',
                namebuild      = 'flat@cen',
            ),
            print(t.prompt, 'tb01b =', tb01b)
            print()

        ##########################################################################
        # 2. PGD
        ##########################################################################

        # Look for a PGD file if already available for this xpid and geometry
        self.sh.title('PGD from the current experiment')
        tb02a = toolbox.input(
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
        print(t.prompt, 'tb02a =', tb02a)
        print()

        # Alternate : look for a PGD file if already available for this geometry with the "spinup" xpid
        # If not available, do not fail because the PGD file will be automatically built.
        self.sh.title('PGD from a spinup experiment')
        tb02b = toolbox.input(
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
        print(t.prompt, 'tb02b =', tb02b)
        print()

        ##########################################################################
        # 3. PREP
        ##########################################################################

        self.sh.title('PREP from the current experiment')
        prep_a = toolbox.input(
            role           = 'SnowpackInit',
            local          = 'PREP.nc',
            experiment     = self.conf.xpid if not hasattr(self.conf, 'prep_xpid') else self.conf.prep_xpid,
            geometry       = self.conf.geometry,
            date           = self.conf.datespinup,
            intent         = 'inout',
            nativefmt      = 'netcdf',
            kind           = 'PREP',
            model          = 'surfex',
            namespace      = 'vortex.multi.fr',
            namebuild      = 'flat@cen',
            block          = 'prep',
            fatal          = False if not hasattr(self.conf, 'prep_xpid') else True,
        ),
        print(t.prompt, 'PREP (a) =', prep_a)
        print()

        # 1st alternate : look for a PREP file if already available for this geometry,
        # and initial date for the "spinup" xpid
        self.sh.title('PREP from a spinup experiment')
        prep_b = toolbox.input(
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
        print(t.prompt, 'PREP (b) =', prep_b)
        print()

        # 2nd alternate : look for a PREP file if already available for this geometry,
        # and initial date for the "reanalysis" xpid
        # If not available, do not fail because the PREP file will be automatically built.
        self.sh.title('PREP from a reanalysis experiment')
        prep_c = toolbox.input(
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
        print(t.prompt, 'PREP (c) =', prep_c)
        print()

        ##########################################################################
        # 4. ECOCLIMAP
        ##########################################################################

        self.sh.title('Toolbox input ecoclimap1')
        ecmap1 = toolbox.input(
            role           = 'Surfex cover parameters',
            kind           = 'coverparams',
            nativefmt      = 'bin',
            local          = 'ecoclimapI_covers_param.bin',
            geometry       = self.conf.geometry,
            genv           = self.conf.genv,
            source         = 'ecoclimap1',
            model          = 'surfex',
        ),
        print(t.prompt, 'ecoclimap1 =', ecmap1)
        print()

        self.sh.title('Toolbox input ecoclimap2')
        ecmap2 = toolbox.input(
            role           = 'Surfex cover parameters',
            kind           = 'coverparams',
            nativefmt      = 'bin',
            local          = 'ecoclimapII_eu_covers_param.bin',
            geometry       = self.conf.geometry,
            genv           = self.conf.genv,
            source         = 'ecoclimap2',
            model          = 'surfex',
        ),
        print(t.prompt, 'ecoclimap2 =', ecmap2)
        print()

        ##########################################################################
        # 5. SSA parameters
        ##########################################################################

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

        ##########################################################################
        # 6. Nameliste
        ##########################################################################

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
        elif hasattr(self.conf, "uenv") and 'OPTIONS.nam' in self.conf.udata.items():
            # The OPTION.nam namelist will be retrived by the 'tbuenv' toolbox
            pass
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

        ##########################################################################
        # 7. Executable
        ##########################################################################

        self.sh.title('Toolbox executable tb06= tbx1')
        self.offline = toolbox.executable(
            role           = 'Binary',
            kind           = 'offline',
            local          = 'OFFLINE',
            model          = 'surfex',
            remote         = self.conf.exesurfex + "/OFFLINE"
        )
        print(t.prompt, 'Exe OFFLINE =', self.offline)
        print()

    def algo(self):
        """
        Launch a single OFFLINE execution with MPI parallelisation
        """

        t = self.ticket
        # Algo component to produce to run the SURFEX OFFLINE simulation (MPI parallelization)
        self.sh.title('ALGO : OFFLINE MPI')
        offline = toolbox.algo(
            engine         = 'parallel',  # TODO : *engine* should be 'blind'
            binary         = 'OFFLINE',
            kind           = 'deterministic',
            datebegin      = self.conf.datebegin,
            dateend        = self.conf.dateend,
            dateinit       = self.conf.date_prep,
            ntasks         = self.conf.ntasks,
            verbose        = True,
        )
        print(t.prompt, 'Algo =', offline)
        print()
        self.component_runner(offline, self.offline, mpiopts=dict(nprocs=self.conf.nprocs, ntasks=self.conf.ntasks))

    def put_remote_outputs(self):
        """
        Main method to save an OFFLINE execution output (PRO file)
        """
        t = self.ticket()
        source_safran, block_safran = self.get_source_safran(meteo=self.conf.meteo)
        list_dates_begin_forc, list_dates_end_forc, list_dates_begin_pro, list_dates_end_pro = \
            get_list_dates_files(self.conf.datebegin, self.conf.dateend, self.conf.duration)
        dict_dates_end_pro = get_dic_dateend(list_dates_begin_pro, list_dates_end_pro)

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
            namespace      = self.conf.namespace,
            namebuild      = 'flat@cen',
            block          = 'pro',
            member         = self.conf.member if hasattr(self.conf, 'member') else None,
            fatal          = False
        ),
        print(t.prompt, 'tb19 =', tb19)
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
                member         = self.conf.member if hasattr(self.conf, 'member') else None,
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
                block          = 'diag',
                member         = self.conf.member if hasattr(self.conf, 'member') else None,
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
                block          = 'prep',
                member         = self.conf.member if hasattr(self.conf, 'member') else None,
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
                dateend        = dict_dates_end_pro if not self.conf.dailyprep else
                                 list(daterange(tomorrow(base=datebegin), dateend)),
                nativefmt      = 'netcdf',
                kind           = 'SnowpackSimulation',
                model          = 'surfex',
                block          = 'pro',
                member         = self.conf.member if hasattr(self.conf, 'member') else None,
            ),
            print(t.prompt, 'tb19 =', tb19)
            print()

            tb19b = toolbox.output(
                local          = 'DIAG_[datebegin:ymdh]_[dateend:ymdh].nc',
                experiment     = self.conf.xpid,
                geometry       = self.conf.geometry,
                datebegin      = list_dates_begin_pro if not self.conf.dailyprep else '[dateend]/-PT24H',
                dateend        = dict_dates_end_pro if not self.conf.dailyprep else
                                 list(daterange(tomorrow(base=datebegin), dateend)),
                nativefmt      = 'netcdf',
                kind           = 'SnowpackSimulation',
                model          = 'surfex',
                namespace      = 'vortex.multi.fr',
                namebuild      = 'flat@cen',
                block          = 'diag',
                member         = self.conf.member if hasattr(self.conf, 'member') else None,
                fatal          = False,
            ),
            print(t.prompt, 'tb19b =', tb19b)
            print()

            tb19c = toolbox.output(
                local          = 'CUMUL_[datebegin:ymdh]_[dateend:ymdh].nc',
                experiment     = self.conf.xpid,
                geometry       = self.conf.geometry,
                datebegin      = list_dates_begin_pro if not self.conf.dailyprep else '[dateend]/-PT24H',
                dateend        = dict_dates_end_pro if not self.conf.dailyprep else
                                 list(daterange(tomorrow(base=datebegin), dateend)),
                nativefmt      = 'netcdf',
                kind           = 'SnowpackSimulation',
                model          = 'surfex',
                namespace      = 'vortex.multi.fr',
                namebuild      = 'flat@cen',
                block          = 'cumul',
                member         = self.conf.member if hasattr(self.conf, 'member') else None,
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
                date           = list_dates_end_pro if not self.conf.dailyprep else
                                 list(daterange(tomorrow(base=datebegin), dateend)),
                nativefmt      = 'netcdf',
                kind           = 'PREP',
                model          = 'surfex',
                namespace      = 'vortex.multi.fr',
                namebuild      = 'flat@cen',
                block          = 'prep',
                member         = self.conf.member if hasattr(self.conf, 'member') else None,
            ),
            print(t.prompt, 'tb20 =', tb20)
            print()

# The following condition does not work. --> Ask leffe how to do
#                 if not (tb02a[0] or tb02b[0]):
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
            block          = 'pgd',
            member         = self.conf.member if hasattr(self.conf, 'member') else None,
        ),
        print(t.prompt, 'tb21 =', tb21)
        print()


class OfflineResearchTask(OfflineTask):
    """
    Base class for SURFEX/OFFLINE executions
    """

    def get_remote_inputs(self):

        t = self.ticket()

        super().get_remote_inputs()

        self.sh.title('New input 1')
        tb_new1 = toolbox.input(
            role            = 'LAI',
            kind            = 'const',
            model           = 'surfex',
            local           = 'lai_forest.txt',
            genv            = self.conf.uenv,
            gvar            = self.conf.gvar_lai,
            format          = 'ascii',
        )
        print(t.prompt, 'LAI =', tb_new1)
        print()

        self.sh.title('New input 2')
        tb_new2 = toolbox.input(
            kind            = 'const',
            model           = 'surfex',
            local           = 'htree_forest_cap.txt',
            genv            = self.conf.uenv,
            gvar            = self.conf.gvar_htree,
            format          = 'ascii',
        )
        print(t.prompt, 'HTree =', tb_new2)
        print()
