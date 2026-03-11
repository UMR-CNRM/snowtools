# -*- coding:Utf-8 -*-

from mkjob.nodes import Driver
import vortex
from vortex_cen.tasks.regrid.add_slopes import AddSlopes
from vortex_cen.tasks.surfex.offline import Offline_MPI_Uenv


def setup(t, **kw):
    return Driver(
        tag='surfex',
        ticket=t,
        nodes=[
            AddSlopes(tag='addslopes', ticket=t, **kw),
            # No need for preprocess since the namelist pre-processing is already included
            # in the "Surfex_Parallel" algo component
            Offline_reanalysis(tag='offline', ticket=t, **kw),
        ],
        options=kw,
    )


class Offline_reanalysis(Offline_MPI_Uenv):
    '''
    OFFLINE reanalysis task :
    - Get all constant inputs (including the PGD file) from a User Environment.
    - Get forcing file(s) on a compute node (step.02) because it comes from the
      output of a previous execution of "AddSlopes" task.
    '''
    def get_remote_inputs(self):

        self.get_ecoclimap()
        self.get_drdt_bst_fit()
        self.get_pgd()
        self.get_prep()
        self.get_executable()
        self.get_namelist_from_uenv()

    def get_local_inputs(self):
        self.get_forcing(localname='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc')

    def get_pgd(self):
        """
        Get PGD file from a User Environment (reanalysis case only !)
        """
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
