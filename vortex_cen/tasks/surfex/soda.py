# -*- coding: utf-8 -*-
'''
'''

import vortex
from vortex_cen.tasks.research_task_base import _CenResearchTask
from vortex_cen.tasks.surfex.commons import SurfexCommonsMixin


class SodaCommonsMixin(SurfexCommonsMixin):

    def get_snow_observation(self):

        self.sh.title('Input Observation')
        obs = vortex.input(
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
            experiment      = self.conf.get('observation_xpid', self.conf.xpid),
            username        = self.conf.get('observation_user', None),
            local           = 'OBSERVATIONS_[datevalidity:ymdHh].nc',
            fatal           = True
        )
        print(self.ticket.prompt, 'Observation =', obs)
        print()

    def get_soda_exe_from_uenv(self, mpi=False, fatal=True):

        if mpi:
            default_gvar = 'master_soda_mpi'
        else:
            default_gvar = 'master_soda_nompi'

        self.sh.title('Input SODA executable')
        soda = vortex.executable(
            role           = 'Binary',
            kind           = 'soda',
            local          = 'SODA',
            model          = 'surfex',
            genv           = self.conf.get('surfex_uenv', self.conf.uenv),
            gvar           = self.conf.get('soda_gvar', default_gvar),
            fatal          = fatal,
        )
        print(self.ticket.prompt, 'SODA =', soda)
        print()

    def get_background(self):

        self.sh.title('Input SODA background PREPs')
        prep = vortex.input(
            role           = 'SnowpackInit',
            member         = self.get_list_members(),
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
        print(self.ticket.prompt, 'Background PREP =', prep)
        print()


class Soda(SodaCommonsMixin, _CenResearchTask):
    '''
    SODA Particle Filter assimilation task.
    Reference : Cluzet et al. (2021): https://gmd.copernicus.org/articles/14/1595/2021/

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

    '''
    def __init__(self, **kw):

        super().__init__(**kw)
        MANDATORY_CONFIGURATION_VARIABLES = [
            "datebegin",
            "dateend",
            "xpid",
            "geometry",
            "uenv|surfex_uenv",
            "members",
        ]

        OPTIONAL_CONFIGURATION_VARIABLES = [
            "observation_vapp+help=*vapp* of the snow observation to assimilate;type=str;default=*vapp*",
            "observation_vconf+help=*vconf* of the snow observation to assimilate;type=str;default=*vconf*",
            "observation_xpid+help=Experiment identifier of the snow observation to assimilate;type=str;default=*xpid*",
            "observation_user+help=Name of the user who owns the snow observation file to assimilate;" +
            "type=str;default=$USER",
            "sensor+help=Sensor used for the snow observation to assimilate (ex: MODIS, PLEIADES, VIIRS);type=str",
            "scope+help=Scope of the snow observation to assimilate observation (ex: MODIS, PLEIADES, VIIRS);type=str",
            "soda_gvar",
            "prep",
            "prep_namespace+help=Where to look for the PREP files ('vortex.cache.fr' if part of the 'assim' task');" +
            "type=str;choices=vortex.cache.fr (local cache), vortex.archive.fr (Hendrix), " +
            "vortex.multi.fr (Hendrix + local cache)",
            "pgd",
            "diff_xpid",
            "diff_user",
        ]

        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def get_remote_inputs(self):

        self.get_pgd_from_cache_or_archive()
        self.get_ecoclimap()
        self.get_drdt_bst_fit()
        self.get_snow_observation()
        self.get_soda_exe_from_uenv()
        self.get_background()

    def get_local_inputs(self):

        self.get_namelist_from_cache()

    def algo(self):

        self.sh.title('Algo SODA')
        algo = vortex.task(
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

    def put_outputs(self):

        t = self.ticket

        self.sh.title('Output PREP (analysis)')
        prep = vortex.output(
            local          = 'mb[member]/PREP_[date:ymdh].nc',
            role           = 'SnowpackInit',
            experiment     = self.conf.xpid,
            geometry       = self.conf.geometry,
            date           = self.conf.date,
            member         = self.get_list_members(),
            nativefmt      = 'netcdf',
            kind           = 'PREP',
            model          = 'surfex',
            namespace      = 'vortex.multi.fr',
            namebuild      = 'flat@cen',
            block          = 'soda/analysis',
            fatal          = True
        ),
        print(t.prompt, 'SODA analysis =', prep)
        print()

        self.sh.title('Output SODA diagnostics')
        diags = vortex.output(
            kind           = ['PART', 'BG_CORR', 'IMASK', 'ALPHA', 'SNOWLINE'],
            model          = 'soda',
            block          = 'soda',
            namebuild      = 'flat@cen',
            geometry       = self.conf.geometry,
            namespace      = 'vortex.multi.fr',
            dateassim      = self.conf.date,
            experiment     = self.conf.xpid,
            local          = '[kind]',
            fatal          = False,  # TODO : cela pourrait dépendre du "kind" pour plus de felxibilité
        )
        print(t.prompt, 'SODA diags =', diags)
        print()

    def diff(self):
        """
        Test output reproductibility [OPTIONAL]
        """
        self.sh.title("Reproductibility check : PREP")
        diff = vortex.diff(
            local          = 'mb[member]/PREP_[date:ymdh].nc',
            role           = 'SnowpackInit',
            experiment     = self.conf.diff_xpid,
            username       = self.conf.get('diff_user', None),
            geometry       = self.conf.geometry,
            date           = self.conf.date,
            member         = self.get_list_members(),
            nativefmt      = 'netcdf',
            kind           = 'PREP',
            model          = 'surfex',
            namespace      = 'vortex.multi.fr',
            namebuild      = 'flat@cen',
            block          = 'soda/analysis',
            fatal          = True
        ),
        print(self.ticket.prompt, 'diff =', diff)
        print()
