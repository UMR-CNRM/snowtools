# -*- coding: utf-8 -*-

from vortex.layout.nodes import Driver, Family
from vortex_cen.tasks.surfex.pre_process import Preprocess_Uenv_Namelist, Preprocess_Local_Namelist
from vortex_cen.tasks.surfex.pgd import Pgd_Uenv_Pgd, Pgd_Local_Pgd
from vortex_cen.tasks.surfex.prep import Prep_Uenv_TG_Uenv_Prep, Prep_Local_TG_Local_Prep
from vortex_cen.tasks.surfex.offline import Offline_MPI_Uenv, Offline_MPI_Local


def setup(t, **kw):
    return Driver(
        tag='prep',
        ticket=t,
        nodes=[
            Family(
                Preprocess_Local_Namelist(tag='preprocess_local_namelist', ticket=t, **kw),
                Pgd_Local_Pgd(tag='pgd_local_pgd', ticket=t, **kw),
                Prep_Local_TG_Local_Prep(tag='prep_local_tg_local_prep', ticket=t, **kw),
                Offline_MPI_Local(tag='offline_mpi_local', ticket=t, **kw),
                tag='prep_local',
        ),
            Family(
                Preprocess_Uenv_Namelist(tag='preprocess_uenv_namelist', ticket=t, **kw),
                Pgd_Uenv_Pgd(tag='pgd_uenv_pgd', ticket=t, **kw),
                Prep_Uenv_TG_Uenv_Prep(tag='prep_uenv_tg_uenv_prep', ticket=t, **kw),
                Offline_MPI_Uenv(tag='offline_mpi_uenv', ticket=t, **kw),
                tag='prep_uenv',
            )
        ],
        options=kw,
    )
