# -*- coding:Utf-8 -*-

from mkjob.nodes import Driver
import vortex
from vortex_cen.tasks.regrid.add_slopes import AddSlopes
from vortex_cen.tasks.surfex.offline import Offline_MPI_Uenv
#from vortex_cen.tasks.surfex.pre_process import Preprocess_Uenv_Namelist


def setup(t, **kw):
    return Driver(
        tag='surfex',
        ticket=t,
        nodes=[
            AddSlopes(tag='addslopes', ticket=t, **kw),
            # No need for preprocess since the nameilst pre-processing is already include
            # in the "Surfex_Parallel" algo component
            #Preprocess_Uenv_Namelist(tag='preprocess', ticket=t, **kw),
            Offline_reanalysis(tag='offline', ticket=t, **kw),
        ],
        options=kw,
    )


class Offline_reanalysis(Offline_MPI_Uenv):
    '''
    OFFLINE reanalysis task :
    Get all constant inputs from a User Environment.
    Get forcing file during the compute step (step.02) only because it comes from the
    output of a previous execution of "AddSlopes" task.
    '''
    def get_remote_inputs(self):

        self.get_ecoclimap()
        self.get_drdt_bst_fit()
        self.get_pgd()
        self.get_prep()
        self.get_executable()
        self.get_namelist()

    def get_local_inputs(self):
        self.get_forcing(localname='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc')

    def get_pgd(self):
        self.sh.title('Input PGD')
        pgd = vortex.input(
            role           = 'SurfexClim',
            kind           = 'pgdnc',
            nativefmt      = 'netcdf',
            model          = 'surfex',
            local          = 'PGD.nc',
            geometry       = self.conf.geometry,
            genv           = self.conf.genv,
            gvar           = 'PGD_[geometry:tag]',
        ),
        print(self.ticket.prompt, 'PGD =', pgd)
        print()

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
