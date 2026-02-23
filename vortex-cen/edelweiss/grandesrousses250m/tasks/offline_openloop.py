# -*- coding:Utf-8 -*-

from mkjob.nodes import Driver
import vortex
from vortex_cen.tasks.surfex.pre_process import Preprocess_Uenv_Namelist
from vortex_cen.tasks.surfex.offline import Offline_MPI_Uenv


def setup(t, **kw):
    return Driver(
        tag='offline_openloop',
        ticket=t,
        nodes=[
            Preprocess_Uenv_Namelist(tag='preprocess', ticket=t, **kw),
            Offline_openloop(tag='offline_openloop', ticket=t, **kw),
        ],
        options=kw,
    )


class Offline_openloop(Offline_MPI_Uenv):
    '''
    This is the task for an OFFLINE-MPI execution before any data assimilation.
    All members are initialised by the same PREP file, not associated to any member.
    '''
    def get_remote_inputs(self):

        self.get_forcing(localname='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc')
        self.get_ecoclimap()
        self.get_drdt_bst_fit()
        self.get_pgd()
        self.get_prep()
        self.get_executable_from_uenv()

    def get_local_inputs(self):

        self.get_namelist_from_cache()

    def get_prep(self):
        """
        All members are initialised by the same PREP file, not associated to any member.
        --> This method differs from the one in the main _Offline_MPI class because it is
        explicitely NOT associated to any member.

        """

        self.sh.title('Input PREP')
        prep_tbi = vortex.input(
            local          = 'PREP.nc',
            role           = 'SnowpackInit',
            experiment     = self.conf.get('prep_xpid', self.conf.xpid),
            username       = self.conf.get('prep_user', None),
            date           = self.conf.get('prep_date', self.conf.datebegin),
            vapp           = self.conf.get('prep_vapp', self.conf.vapp),
            vconf          = self.conf.get('prep_vconf', self.conf.vconf),
            geometry       = self.conf.geometry,
            nativefmt      = 'netcdf',
            kind           = 'PREP',
            model          = 'surfex',
            namespace      = 'vortex.multi.fr',
            vortex1        = self.conf.get('prep_vortex1', False),
            namebuild      = 'flat@cen',  # TODO : passer en variable de configuration ?
            block          = self.conf.get('prep_block', 'prep'),
            intent         = 'inout',
        ),
        print(self.ticket.prompt, 'prep_tbi =', prep_tbi)
        print()

    def put_prep(self):
        """
        Archive PREP files as "background" state --> force block value to "prep/bg"
        """

        self.sh.title('Output PREP')
        prep_tbo = vortex.output(
            local          = 'PREP_[date:ymdh].nc',
            role           = 'SnowpackInit',
            experiment     = self.conf.xpid,
            geometry       = self.conf.geometry,
            # MV : comprendre avec Matthieu L les cas d'usages avec "dailyprep" (reforecast ?)
            # et faire une tâche spécifique à ces cas là.
#            date           = list_dates_end_pro if not self.conf.dailyprep else
#                                list(daterange(tomorrow(base=datebegin), dateend)),
            date           = self.list_dates_end_pro,
            nativefmt      = 'netcdf',
            kind           = 'PREP',
            model          = 'surfex',
            namespace      = self.namespace_out,
            namebuild      = 'flat@cen',  # TODO : passer en variable de configuration
            block          = 'prep/background',
            member         = self.conf.get('member', None),
        ),
        print(self.ticket.prompt, 'prep_tbo =', prep_tbo)
        print()
