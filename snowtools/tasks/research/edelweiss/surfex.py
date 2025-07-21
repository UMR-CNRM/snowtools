# -*- coding: utf-8 -*-
'''
Created on 7 mars 2024
@author: Vernay.M
'''

import vortex
from vortex import toolbox
from snowtools.tasks.vortex_task_base import _VortexTask
from snowtools.scripts.extract.vortex import vortexIO as io


class _SURFEXTask(_VortexTask):
    """
    Abstract class common to all SURFEX executions

    Sequence of methods called :
    ----------------------------
    1. get_remote_inputs   --> _Offline
    2. get_local_inputs    --> _VortexTask
    3. algo                --> _Offline
    4. put_local_outputs   --> _VortexTask
    5. put_remote_outputs  --> _Offline

    """

    def get_remote_inputs(self):
        """
        Main method to fetch all necessary input files for an OFFLINE execution
        """
        self.get_forcing()
        self.get_surfex_namelist()
        self.get_prep()
        self.get_pgd()
        self.get_const_surfex()
        self.get_additionnal_inputs()
        self.get_executable()

    def put_remote_outputs(self):
        """
        Main method to save an OFFLINE execution output (PRO file)
        """
        self.sh.title('Output PRO')
        self.pro = toolbox.output(
            kind           = 'SnowpackSimulation',
            local          = 'PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
            model          = 'surfex',
            block          = 'pro',
            member         = self.conf.member,
            datebegin      = self.conf.datebegin,
            dateend        = self.conf.dateend,
        )

        self.sh.title('Output PREP')
        self.prep_out = toolbox.output(
            role           = 'SnowpackInit',
            kind           = 'PREP',
            local          = 'PREP_[date:ymdh].nc',
            date           = self.conf.dateend,
            model          = 'surfex',
            block          = self.conf.block_prep_out,
            member         = self.conf.member,
        ),
        print()

    def get_forcing(self):
        """
        Main method to fetch a single or an ensemble of FORCING files
        """
        kw = self.common_kw.copy()  # Create a copy to set resource-specific entries
        # Update default vapp with specific conf values
        # Verrue pour gérer les FORCINGs 2021/2022 qui commencent à 7h !
        kw.update(dict(vapp=self.conf.vapp_forcing, filename=f'FORCING_{self.conf.datebegin}_[dateend:ymdh].nc',
        # kw.update(dict(vapp=self.conf.vapp_forcing, filename='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
            datebegin=self.conf.datebegin_forcing, dateend=self.conf.dateend_forcing, member=self.conf.member,
            xpid=self.conf.xpid_forcing, geometry=self.conf.geometry_forcing))
        self.sh.title('FORCING')
        self.forcing = io.get_forcing(**kw)

    def get_surfex_namelist(self, source='OPTIONS_OFFLINE.nam'):
        """
        Main method to get an OPTIONS.nam namelist
        The namelist file will be modifier by the pre-processing step, so a copy must be made
        instead of a symbolic link (--> intent='inout' by default in vortexIO).
        """
        self.sh.title('NAMELIST')
        options = toolbox.input(
            role         = 'namelist',
            kind         = 'namelist',
            genv         = self.conf.uenv,
            gvar         = 'NAMELIST_SURFEX',
            local        = 'OPTIONS.nam',
            source       = source,
            model        = 'surfex',
            intent       = 'inout',
        )
        print('OPTIONS =', options)
        print()

    def get_prep(self):
        """
        Main method to get a PREP file
        Parsing an *alternate_xpid* to vortexIO.get_prep implies that alternate resources may
        be used if not PREP file already exists for this geometry/xpid.
        The PREP file will be modified during the execution so a copy must be made
        instead of a symbolic link (--> intent='inout').
        """
        if 'member_prep_in' in self.conf:
            member = self.conf.member_prep_in
        else:
            member = self.conf.member
        self.sh.title('PREP')
        self.prep = toolbox.input(
            role         = "SnowpackInit",
            kind         = 'PREP',
            experiment   = self.conf.xpid_prep_in,
            local        = 'PREP.nc',
            geometry     = self.conf.geometry_prep_in,
            date         = self.conf.date_prep_in,
            intent       = 'inout',
            vapp         = self.conf.vapp_prep_in,
            block        = self.conf.block_prep_in,
            model        = 'surfex',
            member       = member,
        )
        print()

        #kw = self.common_kw.copy()  # Create a copy to set resource-specific entries
        # Update default vapp with specific conf values
        #kw.update(dict(xpid=self.conf.xpid_prep, geometry=self.conf.geometry_prep, date=self.conf.date_prep,
        #    alternate_xpid=self.ref_reanalysis, intent='inout', vapp=self.conf.vapp_prep))
        #self.prep = io.get_prep(**kw)

    def get_pgd(self):
        """
        Main method to get a PGD file.

        TODO : this should be included in the 'get_const_surfex' method
        """

        t = vortex.ticket()

        self.sh.title('PGD')
        # 1. Look for a PGD file if already available for this xpid and geometry
        pgd_a = toolbox.input(
            role      = 'SurfexClim',
            kind      = 'pgdnc',  # TODO : à modifier, la classe common.data.surfex.PGDNC est obsolète !
            model     = 'surfex',
            filename  = 'PGD.nc',
            block     = 'pgd',
            fatal     = False,  # Several possibilities, do not crash imediatly !
        )
        print('PGD (a) = ', pgd_a)
        print()
        # 2. Alternate : look for a PGD file if already available for this geometry with the "spinup" xpid
        pgd_b = toolbox.input(
            alternate  = 'SurfexClim',
            kind       = 'pgdnc',  # TODO : à modifier, la classe common.data.surfex.PGDNC est obsolète !
            model      = 'surfex',
            filename   = 'PGD.nc',
            block      = 'pgd',
            experiment = 'spinup@' + t.env.getvar("USER"),
            fatal      = True,
        )
        print('PGD (a) = ', pgd_b)
        print()
        # self.pgd = io.get_pgd(**self.common_kw)

    def get_const_surfex(self):
        """
        Main method to get all other SURFEX static resources
        """
        self.sh.title('STATIC SURFEX INPUTS')
        io.get_const_offline(self.conf.geometry, self.conf.uenv, alternate_uenv=self.conf.default_uenv)

    def get_additionnal_inputs(self):
        """
        Method to fetch any optional input that can be used for an OFFLINE execution
        """
        pass

    def get_executable(self):
        """
        Main method to get the OFFLINE executable
        This method must be overwritten depending on the type of execution (MPI or not)
        """
        raise NotImplementedError()

    def algo(self):
        """
       Before the launch of a single instance of any OFFLINE executable, the namelist must be pre-processed.

       -->  Launch *Surfex_PreProcess* (vortex/src/cen/algo/deterministic.py) algo component to
            preprocess the namelist (adjust dates, etc.)

        TODO : this piece of algo should be included the the OFFLINE algo component !
        """

        # Launch *Surfex_PreProcess* (vortex/src/cen/algo/deterministic.py) algo component to
        # preprocess the namelist (adjust dates, etc.)
        # TODO : this piece of algo should be included the the OFFLINE algo component !
        t = self.ticket
        self.sh.title('ALGO : NAMELIST pre-processing')
        preprocess = toolbox.algo(
            kind         = 'surfex_preprocess',
            datebegin    = self.conf.datebegin,
            dateend      = self.conf.dateend,
            forcingname  = self.forcing[0].container.basename,
        )
        print(t.prompt, 'Pre-process algo =', preprocess)
        print()
        preprocess.run()


class OfflineMPI(_SURFEXTask):
    """
    Launch an OFFLINE executable with MPI parallelisation

    Sequence of methods called :
    ----------------------------
    1. get_remote_inputs           --> _Offline
        a) get_forcing             --> _Offline
        b) get_surfex_namelist     --> _Offline
        c) get_prep                --> _Offline
        d) get_pgd                 --> _Offline
        e) get_const_offline       --> _Offline
        f) get_additionnal_inputs  --> _Offline
        g) get_offline             --> OfflineMPI

    2. algo                        --> OfflineMPI

    3. put_remote_outputs          --> _Offline
    """

    def get_executable(self):
        """
        Get OFFLINE MPI executable
        """
        self.offline = io.get_offline_mpi(self.conf.uenv, alternate_uenv=self.conf.default_uenv)

    def algo(self):
        """
        Launch a single OFFLINE execution with MPI parallelisation
        """

        super().algo()  # Call the common "pre-processing" required before OFFLINE executions

        t = self.ticket
        # Algo component to produce to run the SURFEX OFFLINE simulation (MPI parallelization)
        self.sh.title('ALGO : OFFLINE MPI')
        offline = toolbox.algo(
            engine         = 'parallel',  # TODO : *engine* should be 'blind'
            binary         = 'OFFLINE',
            kind           = 'deterministic',
            datebegin      = self.conf.datebegin,
            dateend        = self.conf.dateend,
            dateinit       = self.conf.date_prep_in,
            ntasks         = self.conf.ntasks,
            verbose        = True,
        )
        print(t.prompt, 'Algo =', offline)
        print()
        self.component_runner(offline, self.offline, mpiopts=dict(nprocs=self.conf.nprocs, ntasks=self.conf.ntasks))


class OfflineEnsemble(_SURFEXTask):
    """
    Launch several instances of an OFFLINE executable (without an MPI parallelisation) in parallel.

    Sequence of methods called :
    ----------------------------
    1. get_remote_inputs           --> _Offline
        a) get_forcing             --> _Offline
        b) get_surfex_namelist     --> _Offline
        c) get_prep                --> _Offline
        d) get_pgd                 --> _Offline
        e) get_const_offline       --> _Offline
        f) get_additionnal_inputs  --> _Offline
        g) get_offline             --> OfflineEnsemble

    2. algo                        --> OfflineEnsemble

    3. put_remote_outputs          --> _Offline
    """

    def get_executable(self):
        """
        Get OFFLINE MPI executable
        TODO : use a VortexIO like tool
        """
        t = self.ticket
        self.sh.title('EXEC OFFLINE NO MPI')
        self.offline = toolbox.executable(
            role           = 'Binary',
            kind           = 'offline',
            local          = 'OFFLINE',
            model          = 'surfex',
            genv           = self.conf.genv,
            gvar           = 'master_surfex_offline_nompi',
        )
        print(t.prompt, 'Executable =', self.offline)
        print()

    def algo(self):
        """
        Launch several instances of the OFFLINE executable in parallel

        # TODO : réorganiser l'algo `SurfexComponent` en plusieurs algos indépendants ( 1 algo / *kind*)

        """
        super().algo()  # Call the common "pre-processing" required before OFFLINE executions

        t = self.ticket
        self.sh.title('ALGO OFFLINE NO MPI')

        # TODO : réorganiser l'algo en plusieurs algos indépendants ( 1 algo / *kind*)

        algo = toolbox.algo(
            engine         = 's2m',
            kind           = "ensmeteonodet",
            multidates     = True,
            datebegin      = self.conf.datebegin,
            dateend        = self.conf.dateend,
            threshold      = self.conf.threshold,
            members        = self.conf.member,
            ntasks         = 40,
            daily          = not self.conf.previ,
        )
        print(t.prompt, 'Algo =', algo)
        print()
        self.component_runner(algo, self.offline)


class SodaTask(_SURFEXTask):
    '''
    Task for 1 assimilation cycle
    '''

    def get_executable(self):
        """
        """
        self.sh.title('Executable SODA NOMPI')
        self.soda = toolbox.executable(
            role           = 'Binary',
            kind           = 'soda',
            local          = 'SODA',
            model          = 'surfex',
            genv           = self.conf.uenv,
            gvar           = 'master_surfex_soda_nompi',
        )
        print('SODA =', self.soda)
        print()

    def get_remote_inputs(self):

        # TODO : Ensure that the observation is in the simulation's geometry !
        # TODO : Put the Observation input in the common input to crash immediately
        # if the observation file is missing instead of waiting for soda to be called
        self.sh.title('Toolbox input tobs (obs)')
        self.obs = toolbox.input(
            filename   = 'OBSERVATIONS_[date:ymdHh].nc',
            date       = self.conf.date,
            vapp       = 'Pleiades',
            experiment = 'CesarDB@vernaym',
            geometry   = self.conf.geometry,
            kind       = 'SnowObservations',
            model      = 'surfex',
            block      = '',
            namespace  = 'vortex.multi.fr',
            namebuild  = 'flat@cen',
            fatal      = True,
        )
        print('Observation = ', self.obs)
        print()

        self.sh.title('Background PREP')
        prep = toolbox.input(
            role           = 'SnowpackInit',
            member         = self.conf.members,
            local          = 'mb[member%04d]/PREP_[date:ymdh].nc',
            experiment     = self.conf.xpid,
            geometry       = self.conf.geometry,
            date           = self.conf.date,
            nativefmt      = 'netcdf',
            kind           = 'PREP',
            model          = 'surfex',
            namespace      = 'vortex.cache.fr',  # get it on the cache from last loop
            namebuild      = 'flat@cen',
            block          = 'prep/bg',          # get it on cache @mb****/bg
            fatal          = True,
        ),
        print('PREP =', prep)
        print()

        self.get_pgd()

        self.get_const_surfex()

        self.get_executable()

    def get_local_inputs(self):

        self.sh.title('Soda-preprocessed namelist)')
        nam_soda = toolbox.input(
            role            = 'Nam_surfex',
            kind            = 'namelist',
            model           = 'surfex',
            local           = 'OPTIONS.nam',
            experiment      = self.conf.xpid,
            namespace       = 'vortex.cache.fr',
            block           = 'namelist',
            nativefmt       = 'nam',
        )
        print('preprocessed namelist =', nam_soda)
        print()

    def algo(self):

        self.sh.title('Algo SODA')

        algo = toolbox.algo(
            engine         = 'parallel',
            binary         = 'SODA',
            kind           = "s2m_soda",
            dateassim      = self.conf.date,
        )
        print('Algo=', algo)
        print()
        self.component_runner(algo, self.soda, mpiopts=dict(nnodes=1, nprocs=1, ntasks=1))

    def put_local_outputs(self):

        self.sh.title('Output PREP')
        prep_out = toolbox.output(
            local          = 'mb[member%04d]/PREP_[date:ymdh].nc',
            role           = 'SnowpackInit',
            experiment     = self.conf.xpid,
            geometry       = self.conf.geometry,
            date           = self.conf.date,
            member         = self.conf.members,  # BC 21/03/19 probably replace by mbids
            nativefmt      = 'netcdf',
            kind           = 'PREP',
            model          = 'surfex',
            namespace      = 'vortex.multi.fr',
            # storage        = storage,
            # enforcesync    = enforcesync,
            namebuild      = 'flat@cen',
            block          = 'prep/an',
            fatal          = True
        ),
        print('Output PREP =', prep_out)
        print()

        diags_kind = [prefix for prefix in ('PART', 'BG_CORR', 'IMASK', 'ALPHA')
                      if self.sh.path.exists(f'{prefix}_{self.conf.date}.txt')]
        if 'PART' not in diags_kind:
            print(f'file "PART_{self.conf.date}.txt" does not exist')

        self.sh.title('Diagnostics')
        diags = toolbox.output(
            kind           = diags_kind,
            model          = 'soda',
            block          = 'soda',
            namebuild      = 'flat@cen',
            namespace      = 'vortex.multi.fr',
            # storage        = storage,
            # enforcesync    = enforcesync,
            fatal          = True,
            dateassim      = self.conf.date,
            experiment     = self.conf.xpid,
            local          = '[kind]_[dateassim:ymdh].txt',
        )
        print('Diags =', diags)
        print()


class PreprocessSodaNamelist(_SURFEXTask):

    def get_remote_inputs(self):

        self.get_surfex_namelist(source='OPTIONS_SODA.nam')

    def algo(self):
        self.sh.title('SODA preprocess algo)')
        algo = toolbox.algo(
            kind         = 'soda_preprocess',
            members      = self.conf.members,
        )
        print('Namelist soda preprocess =', algo)
        print()
        algo.run()

    def put_local_outputs(self):

        self.sh.title('Soda-preprocessed namelist)')
        nam_out = toolbox.output(
            role            = 'Nam_surfex',
            kind            = 'namelist',
            model           = 'surfex',
            local           = 'OPTIONS.nam',
            experiment      = self.conf.xpid,
            namespace       = 'vortex.cache.fr',
            block           = 'namelist',
            nativefmt       = 'nam',
        )
        print('preprocessed namelist =', nam_out)
        print()
