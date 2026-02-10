# -*- coding: utf-8 -*-

from vortex.layout.nodes import Driver, Family
from vortex_cen.tasks.surfex.pre_process import Preprocess_Uenv_Namelist, Preprocess_Local_Namelist
from vortex_cen.tasks.surfex.pgd import Pgd_Uenv_Pgd, Pgd_Local_Pgd


def setup(t, **kw):
    return Driver(
        tag='pgd',
        ticket=t,
        nodes=[
            Family(
                Preprocess_Local_Namelist(tag='preprocess_local_namelist', ticket=t, **kw),
                Pgd_Local_Pgd(tag='pgd_local_pgd', ticket=t, **kw),
                tag='pgd_local',
        ),
            Family(
                Preprocess_Uenv_Namelist(tag='preprocess_uenv_namelist', ticket=t, **kw),
                Pgd_Uenv_Pgd(tag='pgd_uenv_pgd', ticket=t, **kw),
                tag='pgd_uenv',
            )
        ],
        options=kw,
    )
