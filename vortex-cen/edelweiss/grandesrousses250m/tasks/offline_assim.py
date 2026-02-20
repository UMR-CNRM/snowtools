# -*- coding:Utf-8 -*-

from mkjob.nodes import Driver
import vortex
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
    '''
    def get_remote_inputs(self):

        self.get_forcing(localname='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc')
        self.get_ecoclimap()
        self.get_drdt_bst_fit()
        self.get_pgd()
        self.get_prep()
        self.get_executable()
        self.get_namelist()

    def get_namelist(self):
        """
        Get nameilst from UEnv
        """
        self.sh.title('Toolbox input Namelist')
        namelist_tbi = vortex.input(
            role     = 'Nam_surfex',
            # Dans un UEnv, plusieurs namelistes peuvent être stockées dans une archive ".tar",
            # le footprint *source* permet de définir le nom exact de la nameliste à récupérer.
            source   = self.conf.namelist_source,  # ex : OPTIONS_default.nam
            genv     = self.conf.genv,
            kind     = 'namelist',
            model    = 'surfex',
            local    = 'OPTIONS.nam',
            # MV : la nameliste va être modifiée, il faut s'assurer du droit d'écriture (<==> intent='inout')
            intent   = 'inout',
        )
        print(self.ticket.prompt, 'namelist_tbi =', namelist_tbi)
        print()
