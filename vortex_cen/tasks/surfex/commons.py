# -*- coding: utf-8 -*-
"""
commons.py
----------

MixIn input common to all SURFEX tasks.

.. autoclass:: SurfexCommonsMixin
   :members:
   :show-inheritance:

"""

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

        **Configuration Variables used:**

        * ``surfex_uenv`` or if not present ``uenv`` User Environment in which the following resources are to be retrieved :
          - ecoclimapI_covers_param.bin
          - ecoclimapII_eu_covers_param.bin

          Format : uenv:{uenv_name}@{user}
        * ``geometry`` *geometry* of the forcing file(s)
          type: str, footprints.stdtypes.FPList

        """
        # Binary ECOCLIMAP I files are mandatory to run OFFLINE and taken from the uenv
        self.sh.title('Input ecoclimap1')
        ecoclimap1_tbi = vortex.input(
            role           = 'Surfex cover parameters',
            kind           = 'coverparams',
            nativefmt      = 'bin',
            local          = 'ecoclimapI_covers_param.bin',
            geometry       = self.conf.geometry,
            genv           = self.conf.get('consts_surfex_uenv', self.conf.uenv),
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
            genv           = self.conf.get('consts_surfex_uenv', self.conf.uenv),
            source         = 'ecoclimap2',
            model          = 'surfex',
        ),
        print(self.ticket.prompt, 'ecoclimap2 =', ecoclimap2_tbi)
        print()

    def get_drdt_bst_fit(self):
        """
        Get drdt_bst_fit_60.nc from uenv
        Crocus metamorphism parameters mandatory to run OFFLINE, PREP or PGD

        **Configuration Variables used:**

        * ``surfex_uenv`` or if not present ``uenv`` User Environment in which the following resources are to be retrieved :
          - ecoclimapI_covers_param.bin
          - ecoclimapII_eu_covers_param.bin

          Format : uenv:{uenv_name}@{user}

        """
        self.sh.title('Input drdt_bst_fit_60')
        drdt_bst_fit_tbi = vortex.input(
            role            = 'Parameters for F06 metamorphism',
            kind            = 'ssa_params',
            genv           = self.conf.get('consts_surfex_uenv', self.conf.uenv),
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

        **Configuration Variables used:**

        * ``pgd_xpid`` Experiment Identifier of the PGD file, if different from the task's XPID. defaults to ``xpid``.
          type: str
        * ``pgd_vapp`` *vapp* of the PGD file, if different from the task's *vapp*. defaults to ``vapp``.
          type: str
        * ``pgd_vconf`` *vconf* of the PGD file, if different from the task's *vconf*. defaults to ``vconf``.
          type: str
        * ``pgd_vortex1`` True if the pgd file was produced with vortex1 and uses vortex1 naming conventions.
          default: False
          type: bool
        * ``geometry`` *geometry* of the forcing file(s)
          type: str, footprints.stdtypes.FPList
        """
        try:
            self.sh.title('Input PGD File from cache')
            pgd = vortex.input(
                local         = 'PGD.nc',
                role          = 'SurfexClim',
                experiment    = self.conf.get('pgd_xpid', self.conf.xpid),
                username      = self.conf.get('pgd_user', None),
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
                  'has a node corresponding to a FetchPgd* or MakePgd* task '
                  'before executing the Prep task and that the pgd_xpid values in the '
                  'corresponding configuration sections match. '
                  'Or that MakePgd*  task '
                  'has been run recently for the given experiment (pgd_xpid).')
            raise e

    def get_pgd_file_from_cache_or_archive(self, fatal=True):
        """
        Method to be used in tasks that fetch or try to fetch the pgd file from the cache or archive.

        :param fatal: If True, the method raises a fatal error if the file could not be fetched. Default: True.
            Should be False only in tasks that implement a second option for fetching a pgd file, for example from an uenv.
        :type fatal: bool
        :return: pgd toolbox

        **Configuration Variables used:**

        * ``pgd_xpid`` Experiment Identifier of the PGD file, if different from the task's XPID. defaults to ``xpid``.
          type: str
        * ``pgd_vapp`` *vapp* of the PGD file, if different from the task's *vapp*. defaults to ``vapp``.
          type: str
        * ``pgd_vconf`` *vconf* of the PGD file, if different from the task's *vconf*. defaults to ``vconf``.
          type: str
        * ``pgd_vortex1`` True if the pgd file was produced with vortex1 and uses vortex1 naming conventions.
          default: False
          type: bool
        * ``pgd_user`` Name of the user who produced the PGD file. Default: ``None``.
          type: str
        * ``pgd_geometry`` *geometry* of the pgd file. Default: ``geometry``.
          type: str, footprints.stdtypes.FPList
        """
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

    def get_pgd_file_from_uenv(self, fatal=True):
        """
        Get PGD.nc file from UEnv

        :param fatal: If *True*, the method raises a fatal error if the file could not be fetched. Default: *True*.
            Should be False only in tasks that implement a second option for fetching a pgd file, for example one
            archived from a previous experiment.
        :return: pgd toolbox

        **Configuration Variables used:**

        * ``surfex_uenv`` or if not present ``uenv`` User Environment from which the PGD.nc file should be fetched.
                 Format : uenv:{uenv_name}@{user}
        * ``pgdnc_gvar`` variable name of the pgd file in the uenv. Default: *pgd_[geometry::tag]*.
        * ``pgd_geometry`` *geometry* of the pgd file. Default: ``geometry``.
          type: str, footprints.stdtypes.FPList
        """
        self.sh.title('Input PGD File from UEnv')
        pgd = vortex.input(
            role      = 'SurfexClim',
            genv      = self.conf.get('consts_surfex_uenv', self.conf.uenv),
            gvar      = self.conf.get('pgdnc_gvar', 'pgd_[geometry:area]'),
            kind      = 'pgdnc',
            model     = 'surfex',
            geometry  = self.conf.get('pgd_geometry', self.conf.geometry),
            local     = 'PGD.nc',
            nativefmt = 'netcdf',
            fatal     = fatal,
        )
        print(self.ticket.prompt, 'PGD =', pgd)
        print()
        return pgd

    def get_namelist_from_cache(self):
        """
        get OPTIONS.nam from the local cache usually produced by
        a previous execution of a "pre_process" task.

        **Configuration Variables used:**

        * ``xpid`` experiment identifier
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
            intent = 'inout', # needed for dailyprep
        ),
        print(self.ticket.prompt, 'namelist =', namelist_tbi)
        print()

    def get_namelist_from_uenv(self):
        """
        Get namelist from UEnv. To be used typically by the preprocess_namelist task.

        **Configuration Variables used:**

        * ``surfex_uenv`` or if not present ``uenv`` User Environment from which the namelist file should be fetched.
                 Format : uenv:{uenv_name}@{user}
        * ``namelist_source`` In an UEnv, several namelistes can be present in an *.tar* archive,
          the *source*  footprint allows to define the exact name of the nameliste to fetch.
          For example, *OPTIONS_default.nam*.
        """
        self.sh.title('Input Namelist')
        namelist_tbi = vortex.input(
            role     = 'Nam_surfex',
            # Dans un UEnv, plusieurs namelistes peuvent être stockées dans une archive ".tar",
            # le footprint *source* permet de définir le nom exact de la nameliste à récupérer.
            source   = self.conf.namelist_source,  # ex : OPTIONS_default.nam
            genv     = self.conf.get('surfex_uenv', self.conf.uenv),
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
        Get namelist from a user-defined local path.
        A quick and dirty solution while experimenting
        with namelist parameters without properly archiving the experiments.
        Be aware that using uenvs with versioning is considered a better practice leading to more reproducible results.

        **Configuration Variables used:**

        * ``namelist_path`` absolute path to the namelist file
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

    def get_prep_file_from_cache_or_archive(self, fatal=True, cache_only=False, local="PREP.nc"):
        """
        Standard method to get a PREP file from the cache or archive. The PREP.nc file contains the initial conditions
        of the snowpack at the beginning of the simulation.


        :param fatal: If *True*, the method raises a fatal error if the file could not be fetched. Default: *True*.
            Should be False only in tasks that implement a second option for fetching a pgd file, for example from an uenv.
        :type fatal: bool
        :param cache_only: If *True*, the method gets the PREP file from the cache only. Default: *False*.
            *cache_only=False* should be used in tasks that are supposed to fetch a PREP file as a remote input,
            or check if such a file already exists for the given experiment.
            *cache_only=True* should be used in tasks where the prep file is considered a local input,
            that means that it has been either fetched or produced by a previous task in the driver.
        :type cache_only: bool

        :return: a toolbox input with the PREP file.

        **Configuration Variables used:**

        * ``prep_xpid`` or ``xpid`` Experiment id the prep file should be searched for or put in cache.
        * ``prep_user`` name of the user who produced the PREP file. Default: None.
        * ``prep_date`` or ``datebegin`` Validity date of the prep file. Default is ``datebegin`` but can be any date.
        * ``prep_vapp`` or ``vapp`` Application name to search the PREP.nc file.
        * ``prep_vconf`` or ``vconf`` Configuration name to search the PREP.nc file.
        * ``prep_vortex1`` type: bool. *True* if the requested PREP.nc file was produced with vortex 1 and thus uses
          vortex 1 naming conventions. Default is *False*.
        * ``prep_geometry`` or ``geometry`` *geometry* of the PREP file.
        * ``prep_namebuild`` Default: *flat@cen*
        * ``prep_block`` block part of the data tree to search for the PREP.nc file. Default is ``prep``.
        * ``prep_member`` or ``member`` If the PREP.nc file comes from an ensemble, a member can be chosen.
           Default is ``None``.
        * ``prep_cutoff`` Can be used to select a PREP file coming from an operational forecast (*forecast*) or
           analysis (*assimilation*). Default is *None*. Might be useful for reforecasts.

        """
        if cache_only:
            namespace = 'vortex.cache.fr'
        else:
            namespace = 'vortex.multi.fr'

        self.sh.title('Input PREP file')
        prep_tbi = vortex.input(
            local          = local,
            role           = 'SnowpackInit',
            # MV : pour permettre de récupérer le PREP depuis une expérience indépendante
            # --> possibilité de renseigner 'prep_xpid' dans le fichier de conf
            experiment     = self.conf.get('prep_xpid', self.conf.xpid),
            username       = self.conf.get('prep_user', None),
            # MV : il faut définir la date de validité du fichier PREP qui par défaut
            # est la *datebegin* de simulation mais peut être arbitraire si 'date_prep' est renseigné
            datevalidity   = self.conf.get('prep_date', self.conf.datebegin),
            # MV : Pour prévoir les cas où le PREP vient d'un vapp / vconf différent
            # de ceux de la tâche
            vapp           = self.conf.get('prep_vapp', self.conf.vapp),
            vconf          = self.conf.get('prep_vconf', self.conf.vconf),
            geometry       = self.conf.get('prep_geometry', self.conf.geometry),
            nativefmt      = 'netcdf',
            kind           = 'PREP',
            model          = 'surfex',
            namespace      = namespace,
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

    def get_init_TG_from_cache_or_archive(self, fatal=True, cache_only=False):
        """
        Method to get an init_tg from the vortex cache or the archive.

        :param fatal: If True, the method raises a fatal error if the file could not be fetched. Default: True.
            Should be False only in tasks that implement a second option for fetching an init_tg file,
            for example from an uenv.
        :type fatal: bool
        :param cache_only: If *True*, the method gets the file from the cache only. Default: *False*.
            *cache_only=False* should be used in tasks that are supposed to fetch an init_tg file as a remote input,
            or check if such a file already exists for the given experiment.
            *cache_only=True* should be used in tasks where the init_tg file is considered a local input,
            that means that it has been either fetched or produced by a previous task in the driver.
        :type cache_only: bool
        :return: init_tg toolbox

        **Configuration Variables used:**

    * ``tg_xpid`` or ``xpid`` experiment id the init_TG.nc file should be fetched from.
    * ``tg_user`` name of the user that produced the target the init_TG.nc file. Default: *None*
    * ``tg_geometry`` or ``geometry`` geometry of the init_TG. Logically the same as for the rest of the simulation
    * ``tg_vapp`` or ``vapp`` Application name to search the init_TG.nc file.
    * ``tg_vconf`` or ``vconf`` Configuration name to search the init_TG.nc file.
    * ``tg_block`` Block name to search the init_TG.nc file. Default: *prep*

        """
        if cache_only:
            namespace = 'vortex.cache.fr'
        else:
            namespace = 'vortex.multi.fr'

        try:
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
                namespace  = namespace,
                namebuild  = 'flat@cen',  # TODO : passer en variable de configuration
                block      = self.conf.get("tg_block", "prep"),
                fatal      = fatal,
            ),
            print(self.ticket.prompt, 'initTG =', init_tg)
            print()
        except SectionFatalError as e:
            print('Unable to get init_TG.nc from cache or archive. Make sure that your driver '
                  'has a node corresponding to a task that fetches or produces an init_TG.nc file.'
                  'Make sure that the tg_xpid values in the '
                  'corresponding configuration sections match. ')
            raise e
        return init_tg



    def get_init_TG_from_uenv(self, fatal=True):
        """
        Fetch an init_TG.nc file from an  uenv

        :param fatal: If True, the method raises a fatal error if the file could not be fetched. Default: True.
            Should be False only in tasks that implement a second option for fetching an init_tg file,
            for example from the cache or archive.
        :return: init_tg toolbox

        **Configuration Variables used:**

        * ``tg_geometry`` or ``geometry`` geometry of the init_TG. Logically the same as for the rest of the simulation
        * ``surfex_uenv`` or if not present ``uenv`` User Environment from which the init_TG.nc file should be fetched.
                 Format : uenv:{uenv_name}@{user}
        * ``tg_gvar`` key to look up the init_TG.nc file in the uenv the file should come from.
          Default: *climtg_[geometry::area]*

        """
        self.sh.title('Input init_TG from uenv')
        init_tg = vortex.input(
            role         = "InitialValuesOfGroundTemperature",
            kind         = 'climTG',
            nativefmt    = 'netcdf',
            local        = 'init_TG.nc',
            geometry     = self.conf.get('tg_geometry', self.conf.geometry),
            genv         = self.conf.get('consts_surfex_uenv', self.conf.uenv),
            gvar         = self.conf.get('tg_gvar', 'climtg_[geometry::area]'),
            model        = 'surfex',
            fatal        = fatal,
        ),
        print(self.ticket.prompt, 'initTG =', init_tg)
        print()
        return init_tg
