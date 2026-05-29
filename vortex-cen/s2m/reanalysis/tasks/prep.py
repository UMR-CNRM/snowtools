# -*- coding:Utf-8 -*-

import vortex
from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.prep import _Prep_Construct


def setup(t, **kw):
    return Driver(
        tag='prep',
        ticket=t,
        nodes=[
            MakePrep(tag='makeprep', ticket=t, **kw),
        ],
        options=kw,
    )


class MakePrep(_Prep_Construct):

    def get_init_TG(self):
        self.get_init_TG_from_uenv()

    def get_namelist(self):
        self.get_namelist_from_uenv()

    def get_prep_executable(self):
        self.get_prep_exe_from_uenv()

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
