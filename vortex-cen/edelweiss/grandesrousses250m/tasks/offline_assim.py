# -*- coding:Utf-8 -*-

import vortex
from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.offline import Offline_MPI_Uenv


def setup(t, **kw):
    return Driver(
        tag='offline_assim',
        ticket=t,
        nodes=[
            # No need for preprocess since the nameilst pre-processing is already include
            # in the "Surfex_Parallel" algo component
            Offline_assim(tag='offline_assim', ticket=t, **kw),
        ],
        options=kw,
    )


class Offline_assim(Offline_MPI_Uenv):
    '''
    This is the task for an OFFLINE-MPI execution after a snow data assimilation step.
    Each simulaiton member is initialised by a different PREP file identified by its *member* value.
    '''
    def get_remote_inputs(self):

        self.get_forcing(localname='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc')
        self.get_ecoclimap()
        self.get_drdt_bst_fit()
        self.get_pgd()
        self.get_prep()
        self.get_executable_from_uenv()
        self.get_namelist_from_uenv()

    def get_prep(self):
        """
        All members are initialised by a different PREP file coming from a SODA analysis
        --> Force *block* value to "prep/an" and *member" to the associated member value.

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
            block          = 'prep/an',
            member         = self.conf.member,
            intent         = 'inout',
        ),
        print(self.ticket.prompt, 'prep_tbi =', prep_tbi)
        print()
