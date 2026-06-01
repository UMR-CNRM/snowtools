# -*- coding:Utf-8 -*-

from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.offline import Offline_MPI_Uenv
from vortex_cen.tasks.surfex.pre_process import _Preprocess
from vortex_cen.tasks.surfex.pgd import GetPgd1D
from vortex_cen.tasks.surfex.prep import GetPrep
from vortex_cen.tasks.surfex.init_clim_ground_temperature import InitClimGroundTemperature


def setup(t, **kw):
    return Driver(
        tag='surfex',
        ticket=t,
        nodes=[
            PreProcess(tag='preprocess', ticket=t, **kw),
            MakeClimGroundTemperature(tag='inittg', ticket=t, **kw),
            GetPgd1D(tag='pgd', ticket=t, **kw),
            GetOrMakePrep(tag='prep', ticket=t, **kw),
            Offline(tag='offline', ticket=t, **kw),
        ],
        options=kw,
    )


class MakeClimGroundTemperature(InitClimGroundTemperature):

    def process(self):
        if self.conf.get('climground', False):
            # Check if a PREP file already exists
            prep = self.get_prep(fatal=False)
            # If no PREP file found, launch the generation of init_TG file
            if not prep[0]:
                super().process()
        else:
            pass


class PreProcess(_Preprocess):

    def get_remote_inputs(self):
        if 'namelist_path' in self.conf:
            self.get_namelist_from_path()
        else:
            self.get_namelist_from_uenv()
        self.get_forcing(localname='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc')


class GetOrMakePrep(GetPrep):

    def get_init_TG(self):
        init_tg = self.get_init_TG_from_cache_or_archive(fatal=False)
        if not init_tg[0]:
            self.get_init_TG_from_uenv()


class Offline(Offline_MPI_Uenv):

    def get_remote_inputs(self):

        self.get_ecoclimap()
        self.get_drdt_bst_fit()
        self.get_pgd()
        self.get_executable()

    def get_local_inputs(self):
        self.get_prep()
        self.get_namelist_from_cache()
        self.get_forcing(localname='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc')
