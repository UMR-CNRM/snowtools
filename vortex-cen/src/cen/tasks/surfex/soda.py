# -*- coding: utf-8 -*-
'''
'''

from vortex import toolbox
from vortex_cen.tasks.research_task_base import _CenResearchTask
import footprints


class Soda(_CenResearchTask):
    '''
    SODA Particle Filter assimilation task.
    TODO : ref Cluzet

    Inputs:
    -------
    - SODA namelist (OPTIONS.nam)
    - Ensemble of snowpack initial conditions ("PREP.nc") refered to as "background"
    - ecoclimapI_covers_param.bin and ecoclimapII_eu_covers_param.bin (binaries for vegetation generation)
    - drdt_bst_fit_60.nc (Crocus metamorphism parameters)
    - PGD.nc (Ground physiography)

    Outputs:
    --------
    - Modified ensemble of snowpack initial conditions ("PREP.nc") refered to as "analysis"

    Mandatory configuration variables
    ---------------------------------
    :param date: *date* of the analysis
    :type date: str, Date
    :param geometry: *geometry* of the PREP files
    :type geometry: str, footprints.stdtypes.FPList
    :param xpid: Experiment identifier (format "{experiment_name}@{user}")
    :type xpid: str
    :param observation_xpid: Experiment identifier (format "{experiment_name}@{user}") of the observation file
    :type observation_xpid: str
    :param genv: User Environment in which the following resources are to be retrieved :
                 Format : uenv:{uenv_name}@{user}
    :type genv: str
    :param sensor: Sensor used for the observation (ex: MODIS, PLEIADES, VIIRS)
    :type sensor: str
    :param members: Ensemble members
    :type members: footprints.stdtypes.FPList

    Optionnal configuration variables
    ---------------------------------
    :param scope: Scope of the observation
    :type scope: str
    :param observation_xpid: Experiment identifier of the observation file
    :type observation_xpid: str
    :param observation_user: User who produced / owns the observation file
    :type observation_user: str
    :parm observation_vapp: *vapp* of the observation file
    :type observation_vapp: str
    :parm observation_vconf: *vconf* of the observation file
    :type observation_vconf: str
    :parm prep_vapp: *vapp* of the PREP files
    :type prep_vapp: str
    :parm prep_vconf: *vconf* of the PREP files
    :type prep_vconf: str
    :parm prep_xpid: Exepriment Identifier of the PREP files
    :type prep_xpid: str
    :parm prep_user: User who produced the PREP files
    :type prep_user: str
    :parm prep_vortex1: Whether or not the PREP files have been produced with Vortex-v1
    :type prep_vortex1: bool
    :parm pgd_vapp: *vapp* of the PGD file
    :type pgd_vapp: str
    :parm pgd_vconf: *vconf* of the PGD file
    :type pgd_vconf: str
    :parm pgd_xpid: Exepriment Identifier of the PGD file
    :type pgd_xpid: str
    :parm pgd_user: User who produced the PGD file
    :type pgd_user: str
    :parm pgd_vortex1: Whether or not the PGD files have been produced with Vortex-v1
    :type pgd_vortex1: bool
    '''

    def get_remote_inputs(self):

        t = self.ticket

        self.sh.title('Input PGD')
        pgd = toolbox.input(
            alternate  = 'SurfexClim',
            kind       = 'pgdnc',
            nativefmt  = 'netcdf',
            local      = 'PGD.nc',
            vapp       = self.conf.get('pgd_vapp', self.conf.vapp),
            vconf      = self.conf.get('pgd_vconf', self.conf.vconf),
            experiment = self.conf.get('pgd_xpid', self.conf.xpid),
            username   = self.conf.get('pgd_user', None),
            geometry   = self.conf.geometry,
            model      = 'surfex',
            namespace  = 'vortex.multi.fr',
            namebuild  = 'flat@cen',
            block      = 'pgd',
            vortex1    = self.conf.get('pgd_vortex1', None),
            fatal      = True,
        ),
        print(t.prompt, 'PGD =', pgd)
        print()

        self.sh.title('Input ecoclimapI')
        ecoclimapI = toolbox.input(
            role       = 'Surfex cover parameters',
            kind       = 'coverparams',
            nativefmt  = 'bin',
            local      = 'ecoclimapI_covers_param.bin',
            geometry   = self.conf.geometry,
            genv       = self.conf.genv,
            source     = 'ecoclimap1',
            model      = 'surfex',
        ),
        print(t.prompt, 'ecoclimapI =', ecoclimapI)
        print()

        self.sh.title('Input ecoclimapII')
        ecoclimapII = toolbox.input(
            role       = 'Surfex cover parameters',
            kind       = 'coverparams',
            nativefmt  = 'bin',
            local      = 'ecoclimapII_eu_covers_param.bin',
            geometry   = self.conf.geometry,
            genv       = self.conf.genv,
            source     = 'ecoclimap2',
            model      = 'surfex',
        ),
        print(t.prompt, 'ecoclimapII =', ecoclimapII)
        print()

        self.sh.title('Input drdt_bst_fit_60.nc (Parameters F06 metamorphism)')
        ssa  = toolbox.input(
            kind       = 'ssa_params',
            genv       = self.conf.genv,
            nativefmt  = 'netcdf',
            local      = 'drdt_bst_fit_60.nc',
            model      = 'surfex',
        )
        print(t.prompt, 'SSA parameters =', ssa)
        print()

        self.sh.title('Input Observation')
        obs = toolbox.input(
            kind            = 'SnowObservations',
            geometry        = self.conf.geometry,
            model           = 'surfex',
            nativefmt       = 'netcdf',
            vapp            = self.conf.get('observation_vapp', self.conf.vapp),
            vconf           = self.conf.get('observation_vconf', self.conf.vconf),
            datevalidity    = self.conf.date,  # TODO : autoriser une date =/= ? de la date de run ?
            block           = self.conf.get('sensor', None),
            scope           = self.conf.get('scope', None),
            namespace       = 'vortex.multi.fr',
            namebuild       = 'flat@cen',
            experiment      = self.conf.observation_xpid,
            username        = self.conf.get('observation_user', None),
            local           = 'OBSERVATIONS_[datevalidity:ymdHh].nc',
            fatal           = True
        )
        print(t.prompt, 'Observation =', obs)
        print()

        self.sh.title('Input SODA executable')
        soda = toolbox.executable(
            role           = 'Binary',
            kind           = 'soda',
            local          = 'SODA',
            model          = 'surfex',
            genv           = self.conf.genv,
            gvar           = 'master_surfex_soda_nompi',
        )
        print(self.ticket.prompt, 'SODA =', soda)
        print()

        self.sh.title('Input SODA background PREPs')
        prep = toolbox.input(
            role           = 'SnowpackInit',
            member         = footprints.util.rangex(self.conf.members),
            vapp           = self.conf.get('prep_vapp', self.conf.vapp),
            vconf          = self.conf.get('prep_vconf', self.conf.vconf),
            local          = 'mb[member]/PREP_[date:ymdh].nc',
            experiment     = self.conf.get('prep_xpid', self.conf.xpid),
            username       = self.conf.get('prep_user', None),
            geometry       = self.conf.geometry,
            date           = self.conf.date,
            nativefmt      = 'netcdf',
            kind           = 'PREP',
            model          = 'surfex',
            namespace      = self.conf.get('prep_namespace', 'vortex.multi.fr'),
            namebuild      = 'flat@cen',
            block          = 'prep/bg',
            vortex1        = self.conf.get('prep_vortex1', None),
            fatal          = True,
        ),
        print(t.prompt, 'Background PREP =', prep)
        print()

    def get_local_inputs(self):

        self.sh.title('Input SODA namelist')
        namelist = toolbox.input(
            role       = 'Namelist_soda',
            kind       = 'namelist',
            model      = 'surfex',
            local      = 'OPTIONS.nam',
            experiment = self.conf.xpid,
            namespace  = 'vortex.cache.fr',
            nativefmt  = 'nam',
            block      = 'namelist',
            intent     = 'inout',
        )
        print(self.ticket.prompt, 'namelist =', namelist)
        print()

    def algo(self):

        self.sh.title('Toolbox algo (SODA)')
        algo = toolbox.algo(
            engine         = 'parallel',
            binary         = 'SODA',
            kind           = "s2m_soda",
            dateassim      = self.conf.date,
        )
        print(self.ticket.prompt, 'Algo =', algo)
        print()

        return algo

    def launch_algo(self, algo):

        executable = [tbx.rh for tbx in self.ticket.context.sequence.executables()]
        self.component_runner(algo, executable,
            mpiopts=dict(
                nnodes=self.conf.get('nnodes', 1),
                nprocs=self.conf.get('nprocs', 1),
                ntasks=self.conf.get('ntasks', 1)
            )
        )

    def put_remote_outputs(self):

        t = self.ticket

        self.sh.title('Toolbox output PREP (analysis)')
        prep = toolbox.output(
            local          = 'mb[member]/PREP_[date:ymdh].nc',
            role           = 'SnowpackInit',
            experiment     = self.conf.xpid,
            geometry       = self.conf.geometry,
            date           = self.conf.date,
            member         = footprints.util.rangex(self.conf.members),
            nativefmt      = 'netcdf',
            kind           = 'PREP',
            model          = 'surfex',
            namespace      = 'vortex.multi.fr',
            namebuild      = 'flat@cen',
            block          = 'prep/an',
            fatal          = True
        ),
        print(t.prompt, 'SODA analysis =', prep)
        print()

        self.sh.title('Toolbox output SODA diagnostics')
        diags = toolbox.output(
            kind           = ['PART', 'BG_CORR', 'IMASK', 'ALPHA'],
            model          = 'soda',
            block          = 'soda',
            namebuild      = 'flat@cen',
            namespace      = 'vortex.multi.fr',
            dateassim      = self.conf.date,  # TODO : Use *date* footprint instead
            experiment     = self.conf.xpid,
            local          = '[kind]_[dateassim:ymdh].txt',
            fatal          = False,
        )
        print(t.prompt, 'SODA diags =', diags)
        print()
