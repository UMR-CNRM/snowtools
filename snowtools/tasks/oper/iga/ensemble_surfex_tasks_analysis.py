"""
Created on 7 nov. 2017

@author: lafaysse
"""

from .ensemble_surfex_tasks_common import Ensemble_Surfex_Task
from .ensemble_surfex_tasks_bdpe import Rapatrie_Forcing, Rapatrie_Prep, Rapatrie_Pro
from vortex.layout.nodes import Driver


def setup(t, **kw):
    return Driver(
        tag='Surfex_Parallel',
        ticket=t,
        nodes=[
            Ensemble_Surfex_Task(tag='Ensemble_Surfex_Task', ticket=t, **kw),
            Rapatrie_Forcing(tag='Rapatrie_Forcing', ticket=t, **kw),
            Rapatrie_Pro(tag='Rapatrie_Pro', ticket=t, **kw),
            Rapatrie_Prep(tag='Rapatrie_Prep', ticket=t, **kw),
        ],
        options=kw
    )
