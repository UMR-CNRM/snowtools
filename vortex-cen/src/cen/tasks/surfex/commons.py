# -*- coding: utf-8 -*-

import vortex
from vortex.layout.dataflow import SectionFatalError


class SurfexCommonsMixin:
    """
    Mixin class that provides methods to get common SURFEX inputs.
    """

    def get_ecoclimap(self):
        """
        Get ecoclimapI_covers_param.bin, ecoclimapII_eu_covers_param.bin,
        Binary ECOCLIMAP I files are mandatory to run OFFLINE and taken from the uenv
        Binary ECOCLIMAP II files are mandatory to run OFFLINE and taken from the uenv
        """
        # Binary ECOCLIMAP I files are mandatory to run OFFLINE and taken from the uenv
        self.sh.title('Input ecoclimap1')
        ecoclimap1_tbi = vortex.input(
            role           = 'Surfex cover parameters',
            kind           = 'coverparams',
            nativefmt      = 'bin',
            local          = 'ecoclimapI_covers_param.bin',
            geometry       = self.conf.geometry,
            genv           = self.conf.get('consts_SURFEX', self.conf.genv),
            source         = 'ecoclimap1',
            model          = 'surfex',
        ),
        print(self.ticket.prompt, 'ecoclimap1 =', ecoclimap1_tbi)
        print()

        # Binary ECOCLIMAP II files are mandatory to run OFFLINE and taken from the uenv
        self.sh.title('Input ecoclimap2')
        ecoclimap2_tbi = vortex.input(
            role           = 'Surfex cover parameters',
            kind           = 'coverparams',
            nativefmt      = 'bin',
            local          = 'ecoclimapII_eu_covers_param.bin',
            geometry       = self.conf.geometry,
            genv           = self.conf.get('consts_SURFEX', self.conf.genv),
            source         = 'ecoclimap2',
            model          = 'surfex',
        ),
        print(self.ticket.prompt, 'ecoclimap2 =', ecoclimap2_tbi)
        print()

    def get_drdt_bst_fit(self):
        """
        Get drdt_bst_fit_60.nc from uenv
        Crocus metamorphism parameters mandatory to run OFFLINE, PREP or PGD
        """
        self.sh.title('Input drdt_bst_fit_60')
        drdt_bst_fit_tbi = vortex.input(
            role            = 'Parameters for F06 metamorphism',
            kind            = 'ssa_params',
            genv           = self.conf.get('consts_SURFEX', self.conf.genv),
            nativefmt       = 'netcdf',
            local           = 'drdt_bst_fit_60.nc',
            model           = 'surfex',
        )
        print(self.ticket.prompt, 'drdt_bst_fit_60 =', drdt_bst_fit_tbi)
        print()

    def get_pgd_from_cache(self):
        """
        In the general research case, the PGD comes from the vortex cache.
        For "stable" configurations such as the reanalysis, it comes from a UEnv/GEnv.
        """
        try:
            self.sh.title('Input PGD from cache')
            pgd = vortex.input(
                local         = 'PGD.nc',
                role          = 'SurfexClim',
                experiment    = self.conf.get('pgd_xpid', self.conf.xpid),
                vapp          = self.conf.get('pgd_vapp', self.conf.vapp),
                vconf         = self.conf.get('pgd_vconf', self.conf.vconf),
                geometry      = self.conf.geometry,
                nativefmt     = 'netcdf',
                kind          = 'pgdnc',
                model         = 'surfex',
                namespace     = 'vortex.cache.fr',
                namebuild     = 'flat@cen',  # TODO : passer en variable de configuration
                block         = 'pgd',
                vortex1       = self.conf.get('pgd_vortex1', False),
                fatal         = True,
            ),
            print(self.ticket.prompt, 'PGD =', pgd)
            print()
        except SectionFatalError as e:
            print('Unable to get PGD.nc from cache. Make sure that your driver '
                  'has a node corresponding to the GetPgd1D task '
                  'before executing the Prep task and that the pgd_xpid values in the '
                  'corresponding configuration sections match. '
                  'Or that the Pgd_Uenv_Pgd or Pgd_Local_Pgd task '
                  'has been run recently for the given experiment (pgd_xpid).')
            raise e

    def get_pgd_from_cache_or_archive(self, fatal=True):
        self.sh.title('Input PGD from cache or archive')
        pgd = vortex.input(
            role       = 'SurfexClim',
            kind       = 'pgdnc',
            nativefmt  = 'netcdf',
            local      = 'PGD.nc',
            vapp       = self.conf.get('pgd_vapp', self.conf.vapp),
            vconf      = self.conf.get('pgd_vconf', self.conf.vconf),
            experiment = self.conf.get('pgd_xpid', self.conf.xpid),
            username   = self.conf.get('pgd_user', None),
            geometry   = self.conf.get('pgd_geometry', self.conf.geometry),
            model      = 'surfex',
            namespace  = 'vortex.multi.fr',
            namebuild  = 'flat@cen',  # TODO : passer en variable de configuration
            block      = 'pgd',
            vortex1    = self.conf.get('pgd_vortex1', None),
            fatal      = fatal,
        ),
        print(self.ticket.prompt, 'PGD =', pgd)
        print()
        return pgd

    def get_pgd_from_uenv(self, fatal=True):
        """
        Get PGD.nc file from UEnv
        """
        self.sh.title('Input PGD from UEnv')
        pgd = vortex.input(
            role      = 'SurfexClim',
            genv      = self.conf.get('pgd_genv', self.conf.genv),
            gvar      = self.conf.get('pgd_gvar', 'pgd_[geometry::tag]'),
            kind      = 'pgdnc',
            model     = 'surfex',
            geometry  = self.conf.get('pgd_geometry', self.conf.geometry),
            local     = 'PGD.nc',
            nativefmt = 'netcdf',
            fatal     = fatal,
        )
        print(self.ticket.prompt, 'PGD =', pgd)
        print()

    def get_namelist_from_cache(self):
        """
        OPTIONS.nam always comes from the local cache because it comes from
        a previous execution of a "pre_process" task.
        """
        # Namelist mandatory to run OFFLINE and taken from the cache
        self.sh.title('Input SURFEX-ready namelist')
        namelist_tbi = vortex.input(
            role         = 'Nam_surfex',
            kind         = 'namelist',
            model        = 'surfex',
            local        = 'OPTIONS.nam',
            experiment   = self.conf.xpid,
            namespace    = 'vortex.cache.fr',
            block        = 'namelist',
            nativefmt    = 'nam',
        ),
        print(self.ticket.prompt, 'namelist =', namelist_tbi)
        print()

    def get_namelist_from_uenv(self):
        """
        Get namelist from UEnv
        """
        self.sh.title('Input Namelist')
        namelist_tbi = vortex.input(
            role     = 'Nam_surfex',
            # Dans un UEnv, plusieurs namelistes peuvent être stockées dans une archive ".tar",
            # le footprint *source* permet de définir le nom exact de la nameliste à récupérer.
            source   = self.conf.namelist_source,  # ex : OPTIONS_default.nam
            genv     = self.conf.get('namelist_genv', self.conf.genv),
            kind     = 'namelist',
            model    = 'surfex',
            local    = 'OPTIONS.nam',
            # la nameliste va être modifiée, il faut s'assurer du droit d'écriture (<==> intent='inout')
            intent   = 'inout',
        )
        print(self.ticket.prompt, 'namelist_tbi =', namelist_tbi)
        print()

    def get_namelist_from_path(self):
        """
        Get namelist from user-defined local path
        """
        self.sh.title('Input Namelist')
        namelist_tbi = vortex.input(
            role     = 'Nam_surfex',
            remote   = self.conf.namelist_path,
            kind     = 'namelist',
            model    = 'surfex',
            local    = 'OPTIONS.nam',
            # la nameliste va être modifiée, il faut s'assurer du droit d'écriture (<==> intent='inout')
            intent   = 'inout',
        )
        print(self.ticket.prompt, 'namelist_tbi =', namelist_tbi)
        print()

    def get_prep(self, fatal=True):
        """
        Standard method to get a PREP file.
        """
        self.sh.title('Input PREP')
        prep_tbi = vortex.input(
            local          = 'PREP.nc',
            role           = 'SnowpackInit',
            # MV : pour permettre de récupérer le PREP depuis une expérience indépendante
            # --> possibilité de renseigner 'prep_xpid' dans le fichier de conf
            experiment     = self.conf.get('prep_xpid', self.conf.xpid),
            username       = self.conf.get('prep_user', None),
            # MV : il faut définir la date de validité du fichier PREP qui par défaut
            # est la *datebegin* de simulation mais peut être arbitraire si 'date_prep' est renseigné
            date           = self.conf.get('prep_date', self.conf.datebegin),
            # MV : Pour prévoir les cas où le PREP vient d'un vapp / vconf différent
            # de ceux de la tâche
            vapp           = self.conf.get('prep_vapp', self.conf.vapp),
            vconf          = self.conf.get('prep_vconf', self.conf.vconf),
            geometry       = self.conf.geometry,
            nativefmt      = 'netcdf',
            kind           = 'PREP',
            model          = 'surfex',
            namespace      = 'vortex.multi.fr',
            vortex1        = self.conf.get('prep_vortex1', False),
            namebuild  = self.conf.get('prep_namebuild', 'flat@cen'),
            block          = self.conf.get('prep_block', 'prep'),
            cutoff     = self.conf.get('prep_cutoff', None),
            # MV : La notion de "membre" pour le PREP est particulière dans le cas déterministe
            # - dans le cas général, le PREP n'est associé à aucun *membre*
            # - dans une simulation avec assimilation: la première initialisation est faite
            #   avec un unique fichier PREP pour tous les membres de simulation et les initialisations
            #   suivantes dépendent des membres sélectionnés par SODA.
            # Le cas ensembliste (parralélisation sur les membres, 1 PREP / membre)
            # doit être traité dans une tâche spécifique
            member         = self.conf.get('prep_member', self.conf.get('member', None)),
            intent         = 'inout',
            fatal          = fatal,
        ),
        print(self.ticket.prompt, 'prep_tbi =', prep_tbi)
        print()
        return prep_tbi

    def get_init_TG_from_cache(self):

        try:
            self.sh.title('Input init_TG from Cache')
            init_tg = vortex.input(
                role       = "InitialValuesOfGroundTemperature",
                kind       = 'climTG',
                nativefmt  = 'netcdf',
                local      = 'init_TG.nc',
                vapp       = self.conf.get('tg_vapp', self.conf.vapp),
                vconf      = self.conf.get('tg_vconf', self.conf.vconf),
                experiment = self.conf.get('tg_xpid', self.conf.xpid),
                username   = self.conf.get('tg_user', None),
                geometry   = self.conf.get('tg_geometry', self.conf.geometry),
                model      = 'surfex',
                namespace  = 'vortex.cache.fr',
                namebuild  = 'flat@cen',  # TODO : passer en variable de configuration
                block      = 'prep',
                fatal      = True,
            ),
            print(self.ticket.prompt, 'initTG =', init_tg)
            print()
        except SectionFatalError as e:
            print('Unable to get init_TG.nc from cache. Make sure that your driver '
                  'has a node corresponding to the GetClimGroundTemperature task '
                  'before executing the Prep task and that the tg_xpid values in the '
                  'corresponding configuration sections match. '
                  'Or that the InitClimGroundTemperature task '
                  'has been run recently for the given experiment (tg_xpid).')
            raise e

    def get_init_TG_from_cache_or_archive(self, fatal=True):

        self.sh.title('Input init_TG from cache or archive')
        init_tg = vortex.input(
            role       = "InitialValuesOfGroundTemperature",
            kind       = 'climTG',
            nativefmt  = 'netcdf',
            local      = 'init_TG.nc',
            vapp       = self.conf.get('tg_vapp', self.conf.vapp),
            vconf      = self.conf.get('tg_vconf', self.conf.vconf),
            experiment = self.conf.get('tg_xpid', self.conf.xpid),
            username   = self.conf.get('tg_user', None),
            geometry   = self.conf.get('tg_geometry', self.conf.geometry),
            model      = 'surfex',
            namespace  = 'vortex.multi.fr',
            namebuild  = 'flat@cen',  # TODO : passer en variable de configuration
            block      = 'prep',
            fatal      = fatal,
        ),
        print(self.ticket.prompt, 'initTG =', init_tg)
        print()
        return init_tg

    def get_init_TG_from_uenv(self, fatal=True):
        self.sh.title('Input init_TG from uenv')
        init_tg = vortex.input(
            role         = "InitialValuesOfGroundTemperature",
            kind         = 'climTG',
            nativefmt    = 'netcdf',
            local        = 'init_TG.nc',
            geometry     = self.conf.get('tg_geometry', self.conf.geometry),
            genv         = self.conf.get('tg_genv', self.conf.genv),
            gvar         = self.conf.get('tg_gvar', 'climtg_[geometry::area]'),
            model        = 'surfex',
            fatal        = fatal,
        ),
        print(self.ticket.prompt, 'initTG =', init_tg)
        print()
        return init_tg
