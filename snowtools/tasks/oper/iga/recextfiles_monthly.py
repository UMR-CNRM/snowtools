"""
Created on 6 sep. 2021

@author: Ferriol
"""

from .monthly_surfex_reanalysis import Monthly_Surfex_Reanalysis
from .safran_analyse_ensemble import Safran
from vortex.layout.nodes import Driver


def setup(t, **kw):
    return Driver(
        tag='Surfex_Parallel',
        ticket=t,
        nodes=[
            Monthly_Surfex_Reanalysis(tag='Monthly_Surfex_Reanalysis', ticket=t, **kw),
            Safran(tag='Safran', ticket=t, **kw),
        ],
        options=kw
    )
