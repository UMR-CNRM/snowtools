# -*- coding: utf-8 -*-

from vortex.layout.nodes import Driver
from vortex_cen.tasks.surfex.pre_process import Preprocess_Uenv_Namelist, Preprocess_Local_Namelist
from vortex_cen.tasks.surfex.pgd import Pgd_Uenv_Pgd, Pgd_Local_Pgd
from vortex_cen.tasks.surfex.init_clim_ground_temperature import InitClimGroundTemperature
from vortex_cen.tasks.surfex.prep import Prep_Uenv_TG_Uenv_Prep, Prep_Local_TG_Local_Prep


def setup(t, **kw):
    return Driver(
        tag='prep',
        ticket=t,
        nodes=[
                Preprocess_Local_Namelist(tag='preprocess_local_namelist', ticket=t, **kw),
                Pgd_Local_Pgd(tag='pgd_local_pgd', ticket=t, **kw),
                InitClimGroundTemperature(tag='init_tg', ticket=t, **kw),
                Prep_Local_TG_Local_Prep(tag='prep_local_tg_local_prep', ticket=t, **kw),
                Preprocess_Uenv_Namelist(tag='preprocess_uenv_namelist', ticket=t, **kw),
                Pgd_Uenv_Pgd(tag='pgd_uenv_pgd', ticket=t, **kw),
                Prep_Uenv_TG_Uenv_Prep(tag='prep_uenv_tg_uenv_prep', ticket=t, **kw),
        ],
        options=kw,
    )
